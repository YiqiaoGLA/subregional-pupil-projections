## this is more a rough and experimental file. 
## not intended for anyone to look at, but keeping it here because I mean to develop it


## 0. libraries and functions
library(data.table)


## 1. reading in the datasets

  ### 1.1 reading in real data
pupils <- fread("data/processed_data/pupil_numbers/itl_pupil_numbers_1112_to_2223.csv")

pupils[, year := as.numeric(substr(time_period, 1, 4))] ## creating the year variable

pupils <- pupils[, c("year", "itl221cd", "nc_year", "headcount")] ## narrowing down to columns we want

colnames(pupils)[colnames(pupils) == "itl221cd"] <- "itl22cd"


  ### 1.2. reading in projections
reception_projections <- fread("output_projections/initial_tenyear/reception_projections_2023_2032_ratio_ets.csv")

year_one_projections <- fread("output_projections/initial_tenyear/year_one_projections_2023_2032_ratio_ets.csv")

year_two_eleven_forecasts <- fread("output_projections/initial_tenyear/year2_year11_projections.csv")

  ### 1.3. reading in the "data resource" of the inputs for plotting
plotting_inputs <- fread("data_resources/inputs_for_plotting_cohort_tracking.csv")


## 2. getting all datasets into one data.table

  ### 2.1. making all columns align
colnames(pupils)[colnames(pupils) == "headcount"] <- "pupil_number"

pupils <- pupils[, c("year", "itl22cd", "nc_year", "pupil_number")]

reception_projections[, nc_year := "reception"]
colnames(reception_projections)[colnames(reception_projections) == "mean_projection"] <- "pupil_number"
reception_projections <- reception_projections[, c("year", "itl22cd", "nc_year", "pupil_number")]

year_one_projections[, nc_year := "year_group_1"]
colnames(year_one_projections)[colnames(year_one_projections) == "mean_projection"] <- "pupil_number"
year_one_projections <- year_one_projections[, c("year", "itl22cd", "nc_year", "pupil_number")]

year_two_eleven_forecasts <- year_two_eleven_forecasts[, c("year", "itl22cd", "nc_year", "pupil_number")]


  ### 2.2. rbinding them all into one, and adding itl names
pupil_data <- rbind(pupils, reception_projections, year_one_projections, year_two_eleven_forecasts)

itl_code_name_lookup <- fread("lookups/itl_code_name_lookup.csv")

setkey(itl_code_name_lookup, "itl22cd")
setkey(pupil_data, "itl22cd")

pupil_data <- itl_code_name_lookup[pupil_data]

pupil_data[itl22nm == "Gloucestershire, Wiltshire and Bath/Bristol area", 
           itl22nm :=  "Gloucestershire, Wiltshire and Bath&Bristol area"] # need to change the name of this itl, because when I try to create a filename using the itl name, the slash messes it up


## 3. 

### 3.1. defining and testing the function to extract the plot data

unique_geogs <- pupil_data[, unique(itl22cd)]
geog <- unique_geogs[1]

start_cohort_year <- 2018
start_cohort_nc_year <- "reception"
end_cohort_nc_year <- "year_group_11"
pupil_data <- pupil_data
max_year <- 2032

extract_cohort_data <- function(start_cohort_year, start_cohort_nc_year, end_cohort_nc_year, geog, pupil_data, max_year = 2032){
  
  ### getting the sequence of nc years that we need to project for
  nc_year_vec <- c("reception", "year_group_1", "year_group_2", "year_group_3", "year_group_4", "year_group_5", "year_group_6", "year_group_7", 
                   "year_group_8", "year_group_9", "year_group_10", "year_group_11", "year_group_12", "year_group_13", "year_group_14")
  
  nc_year_inds <- (which(nc_year_vec == start_cohort_nc_year)):(which(nc_year_vec == end_cohort_nc_year))
  
  nc_year_seq <- nc_year_vec[nc_year_inds]
  
  ### getting the sequence of calendar years that we need to project for
  year_seq <- (start_cohort_year):(start_cohort_year + length(nc_year_seq) - 1)
  
  ### getting rid of any years included in the sequence above that go beyond the maximum year in the dataset
  year_seq <- year_seq[year_seq <= max_year]
  
  nc_year_seq <- nc_year_seq[1:length(year_seq)]
  
  ### extracting the data for the calendar year:nc year combinations
  cohort_tracking_ind <- numeric(length(nc_year_seq))
  
  plot_data <- data.table(
    year = year_seq,
    nc_year = nc_year_seq,
    pupil_number = numeric(length(year_seq))
  )

  for(i in 1:nrow(plot_data)){
    
    ind <- which(pupil_data$year == year_seq[i] & pupil_data$nc_year == nc_year_seq[i] & pupil_data$itl22cd == geog)
    
    plot_data[i, "pupil_number"] <- pupil_data[ind, "pupil_number"]
    
    
  }
  
  return(plot_data)

  
}

check <- extract_cohort_data(start_cohort_year = 2018, start_cohort_nc_year = "reception", end_cohort_nc_year = "year_group_11", geog = unique_geogs[1], pupil_data = pupil_data)

geog <- unique_geogs[1]


  ### 3.2. making the plot - tracking progression through the cohorts - all of them for one geography, on one graph

years_to_plot <- c(2015, 2017, 2019, 2021)
line_cols <- c("purple", "darkgreen", "orange", "darkblue")

j <- 1

for(j in 1:length(unique_geogs)){
  
  geog <- unique_geogs[j]
  
  #ylim_max <- pupil_data[itl22cd == geog, 
  #                       max(pupil_number)]
  
  #ylim_min <- pupil_data[itl22cd == geog & year == 2032 & nc_year == "year_group_11",
  #                       min(pupil_number)]
  
  all_pupils_in_tracking <- list()
  
  for(i in 1:length(years_to_plot)){
    
    start_cohort_year <- years_to_plot[i]
    
    dat <- extract_cohort_data(start_cohort_year = start_cohort_year, start_cohort_nc_year = "reception", end_cohort_nc_year = "year_group_11", geog = geog, pupil_data = pupil_data)
    
    all_pupils_in_tracking[[i]] <- dat
    
  }
  
  all_pupils_in_tracking_dt <- rbindlist(all_pupils_in_tracking)
  
  ylim_max <- all_pupils_in_tracking_dt[, max(pupil_number)]
  ylim_min <- all_pupils_in_tracking_dt[, min(pupil_number)]
  
  
  plot_name <- pupil_data[itl22cd == geog, unique(itl22nm)]
  
  png(file = paste0("plots/cohort_tracking/", plot_name, ".png"),
      height = 7, width = 12, units = "in", res = 600)
  
  plot(x = 1, y = 1, type = "n", bty = "n", las = 1, xlab = "", ylab = "",
       xlim = c(2015, 2032), ylim = c(ylim_min, ylim_max))
  
  title(main = plot_name)
  
  lines(x = c(2022.5, 2022.5), y = c(ylim_min, ylim_max), lty = 2, col = "grey")
  
  for(i in 1:length(years_to_plot)){
    
    start_cohort_year <- years_to_plot[i]
    line_col <- line_cols[i]
    
    plot_data <- extract_cohort_data(start_cohort_year = start_cohort_year, start_cohort_nc_year = "reception", end_cohort_nc_year = "year_group_11", geog = geog, pupil_data = pupil_data)
    
    lines(x = plot_data[, year], y = plot_data[, pupil_number], lwd = 2, col = line_col)
  }
  
  dev.off()

}

