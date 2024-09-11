### This script produces the projections for all nc years past year 1 - so, year 2 to year 11. 
### This script simulates the process of a year progressing through school to create projections. There will be a start year - for example, projected year 1 in 2025, or real year 4 in 2022. 
### It will then apply a series of ratios to "progress" the cohort to year 11. For example, 0.99 will continue from year 1 to year 2 the next year, 0.98 of those year 2 students will carry over to year 3, etc. 
### And these ratios are derived from historical ratios, projected forward. 

## 0. libraries and functions

library(data.table)
library(parallel)
library(doParallel)

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)


## 1. reading in and cleaning the data

  ### 1.1. the pupils dataset
pupils <- fread("data/processed_data/pupil_numbers/itl_pupil_numbers_1112_to_2223.csv")

pupils[, year := as.numeric(substr(time_period, 1, 4))] ## creating the year variable (TO DO - again, need to do this in an earlier script)

pupils <- pupils[, c("year", "itl221cd", "nc_year", "headcount")] ## narrowing down to columns we want

colnames(pupils)[colnames(pupils) == "itl221cd"] <- "itl22cd"

  ### 1.2. the projected progression ratios
pupils_ratio <- fread("data/processed_data/pupil_numbers/pupils_projected_ratio_carrying_over.csv") # the projected carryover ratios, derived in script 3_c

ratio_projection_bootstrapped <- readRDS("output_projections/intermediate_outputs/full_bootstrapped_ratios_carryover.RDS") # the full bootstrapped versions of the ratios above. Important for doing the bootstrapping for this process. 


  ### 1.3. the year 1 projections
year_1_ts_forecast <- readRDS("output_projections/intermediate_outputs/year_one_forecast_ts.rds")

year_1_dt_forecast <- fread("output_projections/initial_tenyear/year_one_projections_2023_2032_ratio_ets.csv")

year_1_bootstrapped <- readRDS("output_projections/intermediate_outputs/year_one_full_bootstrapped.rds")

  ### 1.4. the starting points for the tracking
starting_points <- fread("data_resources/start_years.csv")


## 2. doing the projections
## this is done in two main sections, based on different types of starting points. 
## the first set of starting points is projected year 1 up to 2031. 
## the second set of starting points is real data. It is nc years 2 to 11, for a last calendar year of real data. 
## there is a schematic in the methodology document that should make this clearer. 


  ### 2.1. using year one projections as the start year
starting_points_proj <- starting_points[start_year_status == "projection", ]

all_geogs <- year_1_dt_forecast[, unique(itl22cd)]

projections_for_all_geographies <- list()

for(j in 1:length(all_geogs)){
  
    #### narrowing down to the geography of interest
  projections_for_geography <- list()
  
  geog <- all_geogs[j]
  
    #### simulating the progression through the nc years (i.e. projecting) for each of the starting points. 
  for(i in 1:nrow(starting_points_proj)){
    
    start_cohort_year <- starting_points_proj[i, year] # selecting the starting cohort year, e.g. 2024
    
    start_cohort_nc_year <- starting_points_proj[i, nc_year] # selecting the nc year for the starting cohort - in this loop it will always be year 1
    
    start_cohort_size <- year_1_dt_forecast[year == start_cohort_year & itl22cd == geog, mean_projection] # extracting the number of pupils in the starting cohort
    
    cohort_ratios <- extract_ratios(start_cohort_year = start_cohort_year, # from the projected carry over ratio dataset, extracting the set of ratios we need to use to progress this cohort up to nc year 11
                                    start_cohort_nc_year = start_cohort_nc_year, end_cohort_nc_year = "year_group_11", 
                                    geog = geog, ratio_dataset = pupils_ratio, max_year = 2032)
    
    pupil_projection <- track_cohort(start_cohort_size = start_cohort_size, start_cohort_year = start_cohort_year, # applying the ratios to the starting cohort
                                     start_cohort_nc_year = start_cohort_nc_year, cohort_ratios = cohort_ratios)
    
    projections_for_geography[[i]] <- pupil_projection
    
  }
  
  projections_for_all_geographies[[j]] <- projections_for_geography
  
}

names(projections_for_all_geographies) <- all_geogs

    #### converting from a list of lists of data.tables into one large data.table
projections_for_all_geographies <- lapply(
  X = projections_for_all_geographies,
  FUN = rbindlist
)

projections_for_all_geographies <- rbindlist(projections_for_all_geographies, idcol = "itl22cd")


    #### 2.1.1. adding on uncertainty intervals for those projections with projected starting points

n_cores <- detectCores()

cores_to_use <- round(0.75*n_cores)

cl <- makeCluster(cores_to_use)

clusterEvalQ(cl = cl, expr = c(library(forecast),
                               library(data.table)))

clusterExport(cl = cl, c("all_geogs", "starting_points_proj", "year_1_dt_forecast",
                         "extract_ratios_bootstrapped", "track_cohort", "extract_bootstrapped_prediction_intervals"), envir = environment())

registerDoParallel(cl)

pis_proj_list <- list()

end_output_test_proj <- foreach(j = 1:length(all_geogs)) %dopar% { # defining a list outside of the loop and then adding things iteratively from the loop doesn't work. Instead, foreach actually outputs a list with each iteration of the loop as an item in the list
  
  end_output <- list() ## TO BE RENAMED
  
  geog <- all_geogs[j]
  
  for(i in 1:nrow(starting_points_proj)){
    
    #### extracting the parameters - the year in question, the nc year in question, and the number of pupils in the year/nc-year combination
    start_cohort_year <- starting_points_proj[i, year]
    max_year <- 2032
    
    start_cohort_nc_year <- starting_points_proj[i, nc_year]
    end_cohort_nc_year <- "year_group_11"
    
    start_cohort_size <- year_1_dt_forecast[year == start_cohort_year & itl22cd == geog, mean_projection] 
    
    #### extracting the bootstrapped cohort ratios we'll use to track the pupils' progress through the year
    cohort_ratios_bootstrapped <- extract_ratios_bootstrapped(start_cohort_year = start_cohort_year, start_cohort_nc_year = start_cohort_nc_year, geog = geog, ratio_dataset_bootstrapped = ratio_projection_bootstrapped)
    
    #### extracting the bootstrapped year 1 projection starting point
    year_1_bootstrapped_geog <- year_1_bootstrapped[[geog]]
    
    series_ind <- grep(start_cohort_year, colnames(year_1_bootstrapped_geog))
    
    start_cohort_size_series <- unlist(year_1_bootstrapped_geog[, ..series_ind])
    
    #### tracking progression through the years for each starting cohort, by the 1,000 bootstrapped ratio series
    bootstrapped_pupil_projection <- list()
    
    for(z in 1:length(start_cohort_size_series)){
      
      input_starting_size <- start_cohort_size_series[z]
      
      input_ratios <- unlist(cohort_ratios_bootstrapped[z, ])
      
      output_projection <- track_cohort(start_cohort_year = start_cohort_year,
                                        start_cohort_nc_year = start_cohort_nc_year,
                                        output_to_return = "vector",
                                        start_cohort_size = input_starting_size,
                                        cohort_ratios = input_ratios)
      
      bootstrapped_pupil_projection[[z]] <- output_projection
      
    }
    
    bootstrapped_pupil_projection_dt <- rbindlist(bootstrapped_pupil_projection)
    

    #### extracting the 95% prediction intervals and adding the years and nc_years
    pupil_projection_pis <- extract_bootstrapped_prediction_intervals(bootstrapped_pupil_projection_dt, pi_level = 90)
    
    nc_year_vec <- c("year_group_1", "year_group_2", "year_group_3", "year_group_4", "year_group_5", "year_group_6", "year_group_7", 
                     "year_group_8", "year_group_9", "year_group_10", "year_group_11", "year_group_12", "year_group_13", "year_group_14")
    
    nc_year_inds <- (which(nc_year_vec == start_cohort_nc_year) + 1):(which(nc_year_vec == end_cohort_nc_year))
    
    nc_year_seq <- nc_year_vec[nc_year_inds]
    
    year_seq <- (start_cohort_year + 1):(start_cohort_year + length(nc_year_seq))
    
    year_seq <- year_seq[year_seq <= max_year]
    
    nc_year_seq <- nc_year_seq[1:length(year_seq)]
    
    pupil_projection_pis[, year := year_seq]
    
    pupil_projection_pis[, nc_year := nc_year_seq]
    
    end_output[[i]] <- pupil_projection_pis
    
  }
  
  pis_proj_list[[j]] <- end_output
  
  
}

names(end_output_test_proj) <- all_geogs

    #### converting from a list of lists of data.tables into one data.table
end_output_test_proj <- lapply(
  X = end_output_test_proj,
  FUN = rbindlist
)

prediction_intervals_for_all_geographies_projected <- rbindlist(end_output_test_proj, idcol = "itl22cd")


  ### 2.2. using real data from year 2, year 3, etc, as the starting point
starting_points_actual <- starting_points[start_year_status == "actual", ]

all_geogs <- year_1_dt_forecast[, unique(itl22cd)]

projections_for_all_geographies_actual <- list()

for(j in 1:length(all_geogs)){
  
  
  projections_for_geography <- list()
  
    #### narrowing down to the geography of interest
  geog <- all_geogs[j]
  
  
    #### simulating the progression through the nc years (i.e. projecting) for each of the starting points. 
  for(i in 1:nrow(starting_points_actual)){
    
    start_cohort_year <- starting_points_actual[i, year] # selecting the starting cohort year - in this case, it will always be the last year for which we have real data
    
    start_cohort_nc_year <- starting_points_actual[i, nc_year] # selecting the nc year for the starting cohort - this will vary from year 1 to year 10
    
    start_cohort_size <- pupils[year == start_cohort_year & nc_year == start_cohort_nc_year & itl22cd == geog, headcount] # extracting the number of pupils in the starting cohort
    
    cohort_ratios <- extract_ratios(start_cohort_year = start_cohort_year, # from the projected carry over ratio dataset, extracting the set of ratios we need to use to progress this cohort up to nc year 11
                                    start_cohort_nc_year = start_cohort_nc_year, end_cohort_nc_year = "year_group_11", 
                                    geog = geog, ratio_dataset = pupils_ratio, max_year = 2032) ## TO DO - hardcoded max year...need to get rid of
    
    pupil_projection <- track_cohort(start_cohort_size = start_cohort_size, start_cohort_year = start_cohort_year, # applying the ratios to the starting cohort
                                     start_cohort_nc_year = start_cohort_nc_year, cohort_ratios = cohort_ratios)
    
    projections_for_geography[[i]] <- pupil_projection
    
  }
  
  projections_for_all_geographies_actual[[j]] <- projections_for_geography
  
}

names(projections_for_all_geographies_actual) <- all_geogs

    #### converting from a list of lists of data.tables into one large data.table
projections_for_all_geographies_actual <- lapply(
  X = projections_for_all_geographies_actual,
  FUN = rbindlist
)

projections_for_all_geographies_actual <- rbindlist(projections_for_all_geographies_actual, idcol = "itl22cd")

    #### 2.2.1. adding on uncertainty intervals for the projections with real starting points
    #### effectively the same as 2.2, except that instead of using one series of cohort ratios we're using 1,000 bootstrapped samples of that cohort ratio series
    #### but there is a difference with this bootstrapping process and the process in 2.1.2. In this case, because we have real starting points and not projected starting points, there is no uncertainty attached to the starting points. So we don't use 1,000 permuted versions of th starting points. We use the same one, repeated 1,000 times. 

n_cores <- detectCores()

cores_to_use <- round(0.75*n_cores)

cl <- makeCluster(cores_to_use)

clusterEvalQ(cl = cl, expr = c(library(forecast),
                               library(data.table)))

clusterExport(cl = cl, c("all_geogs", "starting_points_actual", "pupils",
                         "extract_ratios_bootstrapped", "track_cohort", "extract_bootstrapped_prediction_intervals"), envir = environment())

registerDoParallel(cl)


pis_list_actual <- list()

end_output_test <- foreach(j = 1:length(all_geogs)) %dopar% { # defining a list outside of the loop and then adding things iteravily from the loop doesn't work. Instead, foreach actually outputs a list with each iteration of the loop as an item in the list
  
  
  end_output <- list() ## TO BE RENAMED
  
  geog <- all_geogs[j]
  
  for(i in 1:nrow(starting_points_actual)){
    
    #### extracting the parameters - the year in question, the nc year in question, and the number of pupils in the year/nc-year combination
    start_cohort_year <- starting_points_actual[i, year]
    max_year <- 2032
    
    start_cohort_nc_year <- starting_points_actual[i, nc_year]
    end_cohort_nc_year <- "year_group_11"
    
    start_cohort_size <- pupils[year == start_cohort_year & nc_year == start_cohort_nc_year & itl22cd == geog, headcount]
    
    #### extracting the bootstrapped cohort ratios we'll use to track the pupils' progress through the year
    cohort_ratios_bootstrapped <- extract_ratios_bootstrapped(start_cohort_year = start_cohort_year, start_cohort_nc_year = start_cohort_nc_year, geog = geog, ratio_dataset_bootstrapped = ratio_projection_bootstrapped)
    
    bootstrapped_pupil_projection <- apply(X = cohort_ratios_bootstrapped, # tried to turn this into an lapply, but was even slower
                                           MARGIN = 1,
                                           FUN = function(input_cohort_series){return(track_cohort(start_cohort_size = start_cohort_size, 
                                                                                                   start_cohort_year = start_cohort_year,
                                                                                                   start_cohort_nc_year = start_cohort_nc_year,
                                                                                                   output_to_return = "vector",
                                                                                                   cohort_ratios = input_cohort_series))},
                                           simplify = FALSE)
    
    bootstrapped_pupil_projection_dt <- rbindlist(bootstrapped_pupil_projection)
    
    #### extracting the 95% prediction intervals and adding the years and nc_years
    pupil_projection_pis <- extract_bootstrapped_prediction_intervals(bootstrapped_pupil_projection_dt, pi_level = 90)
    
    nc_year_vec <- c("year_group_1", "year_group_2", "year_group_3", "year_group_4", "year_group_5", "year_group_6", "year_group_7", 
                     "year_group_8", "year_group_9", "year_group_10", "year_group_11", "year_group_12", "year_group_13", "year_group_14")
    
    nc_year_inds <- (which(nc_year_vec == start_cohort_nc_year) + 1):(which(nc_year_vec == end_cohort_nc_year))
    
    nc_year_seq <- nc_year_vec[nc_year_inds]
    
    year_seq <- (start_cohort_year + 1):(start_cohort_year + length(nc_year_seq))
    
    year_seq <- year_seq[year_seq <= max_year]
    
    nc_year_seq <- nc_year_seq[1:length(year_seq)]
    
    pupil_projection_pis[, year := year_seq]
    
    pupil_projection_pis[, nc_year := nc_year_seq]
    
    end_output[[i]] <- pupil_projection_pis
    
  }
  
  pis_list_actual[[j]] <- end_output
  
  
}

names(end_output_test) <- all_geogs

    #### converting from a list of lists of data.tables to one big data.table
end_output_test <- lapply(
  X = end_output_test,
  FUN = rbindlist
)

prediction_intervals_for_all_geographies_actual <- rbindlist(end_output_test, idcol = "itl22cd")


## 3. getting the projections all into the one dataset

full_primary_projections <- rbind(projections_for_all_geographies, projections_for_all_geographies_actual)

full_prediction_intervals <- rbind(prediction_intervals_for_all_geographies_actual,
                                   prediction_intervals_for_all_geographies_projected)

full_primary_projections <- full_prediction_intervals[full_primary_projections, on = c(itl22cd = "itl22cd", year = "year", nc_year = "nc_year")]


## 4. writing the final projections

full_primary_projections <- full_primary_projections[, c("itl22cd", "year", "nc_year", "pupil_number", "upper_pi", "lower_pi")]

full_primary_projections <- full_primary_projections[order(itl22cd, nc_year, year), ] # doesn't really matter, but nice to have them in a better order

fwrite(x = full_primary_projections,
       file = "output_projections/initial_tenyear/year2_year11_projections_2023_2032.csv")



### SOME QUICK PLOTTING/CHECKING TO SEE IF THE PLOTS MAKE SENSE
### I am still troubleshooting this, so I'm leaving this very rough code in the file. Will be deleted when the bootstrapping scheme for this script is finalised. 

look <- full_primary_projections[itl22cd == "TLE3", ]

geog_sel <- all_geogs[1]

nc_year_sel <- "year_group_3"


to_plot <- full_primary_projections[itl22cd == geog_sel & nc_year == nc_year_sel, ]

to_plot
ylim_max <- max(to_plot[, c("pupil_number", "upper_pi", "lower_pi")])
ylim_min <- min(to_plot[, c("pupil_number", "upper_pi", "lower_pi")])

pol_color <- rgb(255, 0, 0, 100, maxColorValue = 255)

plot(x = 1, y = 1, type = "n", 
     xlim = c(2023, 2032), ylim = c(ylim_min, ylim_max),
     las = 1, ylab = "", xlab = "", bty = "n")

grid()

polygon(x = c(2023:2032, 2032:2023),
        y = c(to_plot[, lower_pi], rev(to_plot[, upper_pi])),
        col = pol_color, border = NA)

lines(x = 2023:2032, y = to_plot[, pupil_number], col = "blue", lwd = 3)



## TEST PLOT

plot(x = 1, y = 1, type = "n", 
     ylim = c(min(cohort_ratios_bootstrapped), max(cohort_ratios_bootstrapped)), xlim = c(2028, 2032))

for(z in 1:1000){
  
  lines(x = 2028:2032, y = cohort_ratios_bootstrapped[z, ], lwd = 0.2)
  
}

lines(x = 2028:2032, y = cohort_ratios, lwd = 3, col = "red")

## END OF TEST PLOT


