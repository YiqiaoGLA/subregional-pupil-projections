## this is more a rough and experimental file. 
## not intended for anyone to look at, but keeping it here because I mean to develop it


## 0. libraries and functions
library(data.table)

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)



## 1. reading in the datasets

  ### 1.1 reading in real data
pupils <- fread("data/processed_data/pupil_numbers/itl_pupil_numbers_1112_to_2223.csv")

pupils[, year := as.numeric(substr(time_period, 1, 4))] ## creating the year variable

pupils <- pupils[, c("year", "itl221cd", "nc_year", "headcount")] ## narrowing down to columns we want

colnames(pupils)[colnames(pupils) == "itl221cd"] <- "itl22cd"


  ### 1.2. reading in projections
projections <- fread("output_projections/initial_tenyear/reception_year_11_projections_2023_2032.csv")

  ### 1.3. reading in the "data resource" of the inputs for plotting
plotting_inputs <- fread("data_resources/inputs_for_plotting_cohort_tracking.csv")


## 2. getting all datasets into one data.table

  ### 2.1. making all columns align
colnames(pupils)[colnames(pupils) == "headcount"] <- "pupil_number"

pupils <- pupils[, c("year", "itl22cd", "nc_year", "pupil_number")]

colnames(projections)[colnames(projections) == "mean_projection"] <- "pupil_number"

projections <- projections[, c("year", "itl22cd", "nc_year", "pupil_number")]

  ### 2.2. rbinding them all into one, and adding itl names
pupil_data <- rbind(pupils, projections)

itl_code_name_lookup <- fread("lookups/itl_code_name_lookup.csv")

setkey(itl_code_name_lookup, "itl22cd")
setkey(pupil_data, "itl22cd")

pupil_data <- itl_code_name_lookup[pupil_data]

pupil_data[itl22nm == "Gloucestershire, Wiltshire and Bath/Bristol area", 
           itl22nm :=  "Gloucestershire, Wiltshire and Bath&Bristol area"] # need to change the name of this itl, because when I try to create a filename using the itl name, the slash messes it up


## 3. making a plot of cohort progression throughout the years

  ### 3.2. making the plot - tracking progression through the cohorts - all of them for one geography, on one graph
unique_geogs <- pupil_data[, unique(itl22cd)]

years_to_plot <- c(2015, 2017, 2019, 2021)
line_cols <- c("purple", "darkgreen", "orange", "darkblue")

for(j in 1:length(unique_geogs)){
  
  geog <- unique_geogs[j]
  
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


## 4. making a similar set of plots, but this time making the x-axis reception, year 1, year 2, etc, and indexing to the first year, to see if the trends are different

unique_geogs <- pupil_data[, unique(itl22cd)]

years_to_plot <- c(2015, 2017, 2019, 2021)
line_cols <- c("purple", "darkgreen", "orange", "darkblue")

geog <- unique_geogs[j]

for(j in 1:length(unique_geogs)){

  geog <- unique_geogs[j]

    #### extracting the data for the cohorts that we want. 
  all_pupils_in_tracking <- list()

  for(i in 1:length(years_to_plot)){
  
    start_cohort_year <- years_to_plot[i]
  
    dat <- extract_cohort_data(start_cohort_year = start_cohort_year, start_cohort_nc_year = "reception", end_cohort_nc_year = "year_group_11", geog = geog, pupil_data = pupil_data)
  
    all_pupils_in_tracking[[i]] <- dat
  
  }

    #### turning them into indices
  all_pupils_in_tracking_ind <- lapply(
    X = all_pupils_in_tracking,
    FUN = function(data){return(data[, pn_index := 100*(pupil_number/pupil_number[1])])}
  )


    #### getting the y-axis limits and the plot name (which will be the name of the ITL2 geography)
  ylim_max <- max(unlist(lapply(
    X = all_pupils_in_tracking_ind,
    FUN = function(dt){return(max(dt[, pn_index]))}
  )))

  ylim_min <- min(unlist(lapply(
    X = all_pupils_in_tracking_ind,
    FUN = function(dt){return(min(dt[, pn_index]))}
  )))

  plot_name <- pupil_data[itl22cd == geog, unique(itl22nm)]

    #### making the plot
  png(file = paste0("plots/cohort_tracking_indexed/", plot_name, ".png"),
      height = 7, width = 12, units = "in", res = 600)
  
  plot(x = 1:12, y = rep(1, 12), type = "n", bty = "n", axes = FALSE,
      ylim = c(ylim_min, ylim_max), ylab = "", xlab = "")

  axis(side = 2, las = 1)
  axis(side = 1, at = 1:12, 
      labels = c("R", "Year 1", "Year 2", "Year 3", "Year 4", "Year 5", "Year 6", "Year 7", "Year 8", "Year 9", "Year 10", "Year 11"),
      las = 2)

  title(main = plot_name)

  for(i in 1:length(all_pupils_in_tracking_ind)){
  
    series_to_plot <- all_pupils_in_tracking_ind[[i]][, pn_index]
  
    lines(x = 1:12, y = series_to_plot, lwd = 3, col = line_cols[i])
  
  }

  legend("topright", legend = c("2015 start", "2017 start", "2019 start", "2021 start"), 
        col = line_cols, lwd = 3, bty = "n")

  dev.off()

}

