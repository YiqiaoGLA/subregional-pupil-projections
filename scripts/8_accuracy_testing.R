## the scripts for accuracy testing were created in 7_a and 7_b. 
## this script assesses actual vs projected reception numbers, calculates accuracy and percentage of time the real points fall within the 95% confidence intervals, and creates some visualisations. 

## 0. libraries, functions, plot settings

library(data.table)
library(forecast)
library(ggplot2)
library(gglaplot)
library(scales)


functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)

pal <- gla_pal(gla_theme = "default", n = 10)
theme_set(theme_gla(gla_theme = "default", free_y_facets = TRUE))

## 1. reading in all of the datasets, adding input date range as a variable, getting them all into one dataset

  ### 1.1, reading in the ets projections
ets_projections_files <- list.files("output_projections/for_tests/ets_windows") # getting list of all ets projections. They are all saved in separate csvs

ets_projections <- lapply( # reading them into a list
  X = paste0("output_projections/for_tests/ets_windows/", ets_projections_files),
  FUN = fread
)

ets_input_range <- gsub(".csv", "", gsub("ets_projection_", "", ets_projections_files, fixed = TRUE), fixed = TRUE) # isolating the input range, so that we can add it as a variable. several things wrong with this (1) I should have included the input year date range when I created each projection, (2) surely there is a better way than nested gsubs

ets_projections <- mapply( # adding input range years as a variable 
  FUN = function(input_data, input_year_range){return(input_data[, input_year_range := input_year_range])}, 
  input_data = ets_projections,
  input_year_range = ets_input_range,
  SIMPLIFY = FALSE
)

ets_projections <- rbindlist(ets_projections) # converting it into one data.table

ets_projections[, input_start := tstrsplit(input_year_range, split = "_")[[1]]] # isolating start and end of input data
ets_projections[, input_end := tstrsplit(input_year_range, split = "_")[[2]]]


  ### 1.2. reading in the past average projections
pa_projections_files <- list.files("output_projections/for_tests/past_average_windows") # getting list of all pa projections. They are all saved in separate csvs

pa_projections <- lapply( # reading them into a list
  X = paste0("output_projections/for_tests/past_average_windows/", pa_projections_files),
  FUN = fread
)

pa_input_range <- gsub(".csv", "", gsub("past_average_projection_", "", pa_projections_files, fixed = TRUE), fixed = TRUE) # isolating the input range, so that we can add it as a variable. several things wrong with this (1) I should have included the input year date range when I created each projection, (2) surely there is a better way than nested gsubs

pa_projections <- mapply( # adding input range years as a variable 
  FUN = function(input_data, input_year_range){return(input_data[, input_year_range := input_year_range])}, 
  input_data = pa_projections,
  input_year_range = pa_input_range,
  SIMPLIFY = FALSE
)

pa_projections <- rbindlist(pa_projections)

pa_projections[, input_start := tstrsplit(input_year_range, split = "_")[[1]]]  # isolating start and end of input data
pa_projections[, input_end := tstrsplit(input_year_range, split = "_")[[2]]]


  ### 1.3 reading in and cleaning the real values
pupil_data <- fread("data/processed_data/pupil_numbers/itl_pupil_numbers_1112_to_2223.csv")

pupil_data[, year := as.numeric(substr(time_period, 1, 4))] # extracting year as a variable. Again, TO DO - this should be done in an earlier script

real_reception_data <- pupil_data[nc_year == "reception", c("itl221cd", "year", "headcount")]


## 2. joining the real data onto the projected data

setkey(real_reception_data, "itl221cd", "year")

setkey(ets_projections, "itl22cd", "year")
setkey(pa_projections, "itl22cd", "year")

ets_projections <- real_reception_data[ets_projections]
pa_projections <- real_reception_data[pa_projections]


  ### 2.1. sub-task - adding on the ITL names. Most of the datasets I've saved only have codes.  
itl_code_name_lookup <- fread("lookups/itl_code_name_lookup.csv")

setkey(itl_code_name_lookup, "itl22cd")

setkey(ets_projections, "itl221cd")
ets_projections <- itl_code_name_lookup[ets_projections]


setkey(pa_projections, "itl221cd")
pa_projections <- itl_code_name_lookup[pa_projections]


## 3. getting total headline figures - mean average percentage error, proportion of time the real projections fall within the prediction interval, for both methods

calculate_mape(real_series = ets_projections[, headcount], 
               projected_series = ets_projections[, mean_projection])

calculate_mape(real_series = pa_projections[, headcount], 
               projected_series = pa_projections[, mean_projection])


calculate_percentage_within_intervals(real_series = ets_projections[, headcount], 
                                      upper_interval = ets_projections[, upper_pi], lower_interval = ets_projections[, lower_pi])

calculate_percentage_within_intervals(real_series = pa_projections[, headcount], 
                                      upper_interval = pa_projections[, upper_pi], lower_interval = pa_projections[, lower_pi])


## 4. getting each of the metrics by geography

  ### 4.1. calculating the metrics
ets_mape_itl <- ets_projections[, .(mape = calculate_mape(headcount, mean_projection)),
                                by = itl22nm]
ets_mape_itl <- ets_mape_itl[order(mape), ]

ets_within_itl <- ets_projections[, .(perc_within = calculate_percentage_within_intervals(real_series = headcount, upper_interval = upper_pi, lower_interval = lower_pi)),
                                  by = itl22nm]
ets_within_itl <- ets_within_itl[order(perc_within), ]


pa_mape_itl <- pa_projections[, .(mape = calculate_mape(headcount, mean_projection)),
                               by = itl22nm]
pa_mape_itl <- pa_mape_itl[order(mape), ]

pa_within_itl <- pa_projections[, .(perc_within = calculate_percentage_within_intervals(real_series = headcount, upper_interval = upper_pi, lower_interval = lower_pi)),
                                by = itl22nm]
pa_within_itl <- pa_within_itl[order(perc_within), ]


  ### 4.2. getting them into one data.table
ets_mape_itl[, type := "Exponential smoothing + bootstrapping"]
pa_mape_itl[, type := "Past average"]

both_mape_itl <- rbind(ets_mape_itl, pa_mape_itl)

ets_within_itl[, type := "Exponential smoothing + bootstrapping"]
pa_within_itl[, type := "Past average"]

both_within_itl <- rbind(ets_within_itl, pa_within_itl)


  ### 4.3. making some visualisations
positions_mape <- pa_mape_itl[order(mape) ,itl22nm]

png(file = "plots/adhoc_plots/mape.png",
    height = 8, width = 12, units = "in", res = 600)

ggplot() + 
  geom_bar(data = both_mape_itl, stat = "identity", position = position_dodge(width = 0.7),
           aes(x = mape, y = itl22nm, fill = type)) + 
  scale_y_discrete(limits = positions_mape) + 
  scale_x_continuous(labels = dollar_format(prefix = "", suffix = "%")) + 
  scale_colour_manual(values = c(pal[1], pal[2]), aesthetics = "fill") + 
  ggtitle("Mean average percentage error, by ITL2 and modelling method")
  
dev.off()


positions_within <- ets_within_itl[order(perc_within), itl22nm]

png(file = "plots/adhoc_plots/perc_within.png",
    height = 8, width = 12, units = "in", res = 600)

ggplot() + 
  geom_bar(data = both_within_itl, stat = "identity", position = position_dodge(width = 0.7),
           aes(x = perc_within, y = itl22nm, fill = type)) + 
  scale_y_discrete(limits = positions_within) + 
  scale_x_continuous(labels = dollar_format(prefix = "", suffix = "%")) + 
  scale_colour_manual(values = c(pal[1], pal[2]), aesthetics = "fill") + 
  ggtitle("Percentage within prediction intervals, by ITL2 and modelling method")

dev.off()



## 6. do it for for each year - as in, distance between the projected year of interest and last year of real data used for the projection. This way we can assess accuracy as we project further into the future. 

  ### 6.1. calculating years projected ahead, for each entry
ets_projections[, years_projected := as.numeric(year) - as.numeric(input_end)]
pa_projections[, years_projected := as.numeric(year) - as.numeric(input_end)]

  ### 6.2. calculating the metrics by the number of years projected ahead
ets_mape_byyear <- ets_projections[, .(mape = calculate_mape(real_series = headcount, projected_series = mean_projection)),
                by = years_projected]
ets_mape_byyear[, type := "ets"]

pa_mape_byyear <- pa_projections[, .(mape = calculate_mape(real_series = headcount, projected_series = mean_projection)),
               by = years_projected]
pa_mape_byyear[, type := "pa"]

both_mape_byyear <- rbind(ets_mape_byyear, pa_mape_byyear)


ets_within_byyear <- ets_projections[, .(perc_within = calculate_percentage_within_intervals(real_series = headcount, upper_interval = upper_pi, lower_interval = lower_pi)),
                by = years_projected]
ets_within_byyear[, type := "ets"]

pa_within_byyear <- pa_projections[, .(perc_within = calculate_percentage_within_intervals(real_series = headcount, upper_interval = upper_pi, lower_interval = lower_pi)),
                by = years_projected]
pa_within_byyear[, type := "pa"]

both_within_byyear <- rbind(ets_within_byyear, pa_within_byyear)

  ### 6.3. making visualizations
positions_mape <- ets_mape_byyear[order(mape), years_projected]

png(file = "plots/adhoc_plots/mape_by_year.png",
    height = 8, width = 12, units = "in", res = 600)

ggplot() + 
  geom_bar(data = both_mape_byyear, stat = "identity", position = position_dodge(width = 0.7),
           aes(x = mape, y = years_projected, fill = type)) + 
  scale_y_discrete(limits = positions_mape) + 
  scale_x_continuous(labels = dollar_format(prefix = "", suffix = "%")) + 
  scale_colour_manual(values = c(pal[1], pal[2]), aesthetics = "fill") + 
  ggtitle("Mean absolute percentage error, by year since input data \nand modelling method")

dev.off()


positions_within <- ets_within_byyear[order(perc_within), years_projected]

png(file = "plots/adhoc_plots/perc_within_by_year.png",
    height = 8, width = 12, units = "in", res = 600)

ggplot() + 
  geom_bar(data = both_within_byyear, stat = "identity", position = position_dodge(width = 0.7),
           aes(x = perc_within, y = years_projected, fill = type)) + 
  scale_y_discrete(limits = positions_within) + 
  scale_x_continuous(labels = dollar_format(prefix = "", suffix = "%")) + 
  scale_colour_manual(values = c(pal[1], pal[2]), aesthetics = "fill") + 
  ggtitle("Percentage within prediction intervals, by year since input data \nand modelling method")

dev.off()


