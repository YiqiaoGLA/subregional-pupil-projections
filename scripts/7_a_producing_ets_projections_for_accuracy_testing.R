## this script creates one of the two datasets for accuracy testing. This is the script that produces projections using the final intended methodology, i.e. using exponential smoothing and creating prediction intervals using bootstrapping.
## There is a more detailed description of the scheme for accuracy testing in the report/methodology document. But we're using a "window approach", where we produce projections based on all possible windows of real data, of varying series length.
## I am effectively using the same script as script 5_a, the one that produces the 10 year final projections for reception. But it is amended so that most of the modelling is done within a for-loop, looping over different input years, so that we end up with lots of projections to go over. 
## it's a bit clunky and not a very elegant or pleasing solution, but it works just fine and produces the projection I want for real vs actual


## A. libraries and functions

library(data.table)
library(forecast)

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)

## B. reading in data, any small cleaning tasks, and extracting vector of unique geographies

  ### B.1. reading in combined lagged births and reception, and creating the ratio
births_reception_og <- fread("data/processed_data/combined/births_reception_lag4.csv")

births_reception_og[, ratio := headcount/annual_births_lag4]


  ### B.2. reading in births, creating year variable (shouldn't I do that in the processing script...?), creating lagged year variable narrowing down to desired years, and getting rid of forward slashes in the name 
births_og <- fread("data/processed_data/births/itl_births_92_to_23.csv") # TO DO - file name shouldn't be date specific

births_og[, year := (tstrsplit(date, "-", fixed = TRUE)[1])]
births_og[, year := as.numeric(year)]

births_og[, year_lag_4 := year + 4]

births_og[, gss_name := gsub("/", "&", gss_name, fixed = TRUE)] # need to change name of one itl because it has a slash in it


  ### B.3. extracting vector of unique geographies
all_geogs <- births_og[, unique(gss_code)]


  ### B.4. reading in the range of years for accuracy testing
input_years <- fread("data_resources/accuracy_testing_windows.csv")


## C. looping through the input years, creating projections for each one

for(z in 1:nrow(input_years)){
  
  ## 0. extracting the number of years to project ahead
  years_ahead <- input_years[z, years_to_project] 

  ## 1. narrowing down on input years
  start_year <- input_years[z, start_real]
  end_year <- input_years[z, end_real]
  
  births <- births_og[year >= start_year & year <= end_year, ]
  
  births_reception <- births_reception_og[year >= start_year & year <= end_year, ]
  
  ## 2. converting the input data into lists, where each entry into the list is the data for one geography
  
  ### 2.1. converting ratios into a list of ts objects
  all_ts_ratio_list <- list()
  
  for(i in 1:length(all_geogs)){
    
    geog <- all_geogs[i]
    
    all_ts_ratio_list[[i]] <- ts(data = births_reception[itl_cd == geog, ratio], 
                                 start = min(births_reception[, year]), 
                                 frequency = 1)
    
  }
  
  names(all_ts_ratio_list) <- all_geogs
  
  ### 2.2. converting births into a list of ts objects
  all_ts_births_list <- list()
  
  for(i in 1:length(all_geogs)){
    
    geog <- all_geogs[i]
    
    all_ts_births_list[[i]] <- ts(data = births[gss_code == geog, annual_births], 
                                  start = min(births[, year]), 
                                  frequency = 1)
    
  }
  
  names(all_ts_births_list) <- all_geogs
  
  
  ## 3. projecting forward the ratio
  
  projected_ratios <- lapply(
    X = all_ts_ratio_list,
    FUN = project_with_ets,
    periods_ahead = years_ahead
  )
  
  
  ## 4. projecting forward births
  
  ### 4.1. projecting forward 10 years
  projected_births <- lapply(
    X = all_ts_births_list,
    FUN = project_with_ets,
    periods_ahead = years_ahead
  )
  
  ### 4.2. lagging births by four years
  projected_births_lagged <- lapply(
    X = projected_births,
    FUN = lag_time_series,
    years_to_lag = 4
  )
  
  ### 4.3. extracting the real lagged births data to replace the four "missing" years at the start, as ts objects within a list
  latest_year <- min(time(projected_births_lagged[[1]])) - 1 # extracting the latest year for real data we'll want to add
  earliest_year <- latest_year - 3 # extracting the earliest year for real data we'll want to add
  
  real_data_list <- list()
  
  for(i in 1:length(all_geogs)){
    
    geog <- all_geogs[i]
    
    real_births_ts <- ts(
      data = births[gss_code == geog & year_lag_4 %in% latest_year:earliest_year, annual_births],
      start = earliest_year,
      frequency = 1
    )
    
    real_data_list[[i]] <- real_births_ts
    
  }
  
  names(real_data_list) <- all_geogs
  
  ### 4.4. adding the two time series (real births and projected births) together
  real_and_projected_births <- mapply(
    FUN = concatenate_time_series,
    time_series_1 = real_data_list,
    time_series_2 = projected_births_lagged,
    SIMPLIFY = FALSE
  )
  
  
  ## 5. combining births and ratio projections to get mean central reception forecast
  
  reception_forecasts <- mapply(
    FUN = get_ts_product,
    time_series_1 = projected_ratios,
    time_series_2 = real_and_projected_births,
    SIMPLIFY = FALSE
  )
  
  
  ## 6. generating full range of bootstrapped forecasts for ratio
  
  ### 6.1. extract the residuals
  ratio_resids <- lapply(
    X = all_ts_ratio_list,
    FUN = extract_residuals_ets
  )
  
  ### 6.1. resample the residuals
  ratio_resid_resamples <- lapply(
    X = ratio_resids,
    FUN = resample_for_bootstrapping,
    n_resamples = 1000
  )
  
  ### 6.2. add the resampled residuals to the original series
  ### instead of a for-loop, could be mapply with sweep as the function
  final_ratio_resamples <- list()
  
  for(i in 1:length(all_geogs)){
    
    num_ratio_vec <- as.numeric(all_ts_ratio_list[[i]])
    resamples <- ratio_resid_resamples[[i]]
    
    final_ratio_resamples[[i]] <- data.table(sweep(resamples, 2, num_ratio_vec, "+"))
    
  }
  
  names(final_ratio_resamples) <- all_geogs
  
  
  ### 6.3. forecasting each resample
  ### using a for loop instead of an lapply. Better than lists within lists, I think, or an lapply within an lapply.
  
  first_year <- min(time(all_ts_ratio_list[[1]]))
  
  ratio_bootstrapped_forecasts <- list()
  
  for(i in 1:length(all_geogs)){
    
    ratio_resamples_geog <- final_ratio_resamples[[i]]  
    
    ratio_resamples_geog_ts <- apply(X = ratio_resamples_geog, MARGIN = 1, FUN = ts, 
                                     start = first_year, frequency = 1, simplify = FALSE)
    
    ratio_resamples_geog_forecast <- lapply(
      X = ratio_resamples_geog_ts,
      FUN = project_with_ets,
      periods_ahead = years_ahead
    )
    
    ratio_resamples_geog_forecast_dt <- convert_tslist_to_dt(ratio_resamples_geog_forecast)
    
    ratio_bootstrapped_forecasts[[i]] <- ratio_resamples_geog_forecast_dt
    
  }
  
  names(ratio_bootstrapped_forecasts) <- all_geogs
  
  
  ## 7. generating full range of bootstrapped forecasts for births, including both real and projected births
  
  ### 7.1. extract the residuals
  births_resids <- lapply(
    X = all_ts_births_list, 
    FUN = extract_residuals_ets
  )
  
  ### 7.2. resample the residuals
  births_resid_resamples <- lapply(
    X = births_resids,
    FUN = resample_for_bootstrapping,
    n_resamples = 1000
  )
  
  ### 7.3. add the resampled residuals to the original series
  ### instead of a for-loop, could be mapply with sweep as the function
  final_births_resamples <- list()
  
  for(i in 1:length(all_geogs)){
    
    num_births_vec <- as.numeric(all_ts_births_list[[i]])
    resamples <- births_resid_resamples[[i]]
    
    final_births_resamples[[i]] <- data.table(sweep(resamples, 2, num_births_vec, "+"))
    
  }
  
  names(final_births_resamples) <- all_geogs
  
  ### 7.4. forecasting each resample
  ### using a for loop instead of an lapply. Better than lists within lists, I think, or an lapply within an lapply
  first_year <- min(time(all_ts_births_list[[1]]))
  
  births_bootstrapped_forecasts <- list()
  
  for(i in 1:length(all_geogs)){
    
    births_resamples_geog <- final_births_resamples[[i]]  
    
    births_resamples_geog_ts <- apply(X = births_resamples_geog, MARGIN = 1, FUN = ts, 
                                      start = first_year, frequency = 1, simplify = FALSE)
    
    births_resamples_geog_forecast <- lapply(
      X = births_resamples_geog_ts,
      FUN = project_with_ets,
      periods_ahead = years_ahead
    )
    
    births_resamples_geog_forecast_dt <- convert_tslist_to_dt(births_resamples_geog_forecast)
    
    births_bootstrapped_forecasts[[i]] <- births_resamples_geog_forecast_dt
    
  }
  
  names(births_bootstrapped_forecasts) <- all_geogs
  
  
  ### 7.5. adding the real births data to the time series
  
  #### 7.5.1. getting each series of real births into a data.table, where it is repeated 1,000 times
  real_births_repeated <- lapply(
    X = real_data_list,
    FUN = function(input_series){return(
      data.table(matrix(rep(input_series, each = 1000), ncol = length(input_series))) # inelegant solution - fix later
    )}
  )
  
  #### 7.5.2. combining each real births and projected births and lagging
  real_projected_births_bootstrapped <- list()
  
  for(i in 1:length(real_births_repeated)){
    
    ## combining the real and projected births
    combined <- cbind(real_births_repeated[[i]], births_bootstrapped_forecasts[[i]])
    
    ## getting rid of the last four columns
    combined <- combined[, 1:(ncol(combined) - 4)]
    
    ## placing the combined data.frame into the list
    real_projected_births_bootstrapped[[i]] <- combined
    
  } # TO DO - the colnames are wrong, so should fix them. Not important but still should get this sort of thing right. 
  
  names(real_projected_births_bootstrapped) <- all_geogs
  
  
  ## 8. getting bootstrapped reception prediction intervals
  
  ### 8.1. combining projected births and ratios to get reception bootstrapped forecast interval
  bootstrapped_reception_intervals <- mapply(
    FUN = function(dt1, dt2){return(dt1*dt2)},
    dt1 = ratio_bootstrapped_forecasts,
    dt2 = real_projected_births_bootstrapped,
    SIMPLIFY = FALSE
  )
  
  ### 8.2. extracting the 95% prediction intervals
  bootstrapped_prediction_intervals <- lapply(
    FUN = extract_bootstrapped_prediction_intervals,
    X = bootstrapped_reception_intervals,
    pi_level = 95
  )
  
  
  ## 9. getting everything into one data frame for output
  
  ### 9.1. turning the bootstrapped prediction intervals into one data.table
  bootstrapped_prediction_intervals <- rbindlist(bootstrapped_prediction_intervals, 
                                                 idcol = "itl22cd")
  
  ### 9.2. turning the reception forecasts into one data.table
  reception_forecasts <- lapply(
    X = reception_forecasts,
    FUN = function(input_ts){return(data.table(year = time(input_ts), mean_projection = input_ts))}
  )
  
  reception_forecasts <- rbindlist(reception_forecasts)
  
  ### 9.3. combine uncertainty and predictions themselves into one dt for output
  final_dt <- cbind(reception_forecasts, bootstrapped_prediction_intervals)
  
  final_dt <- final_dt[, c("year", "itl22cd", "mean_projection", "upper_pi", "lower_pi")]
  
  
  ## 10. writing the output

  output_file_name <- paste0("ets_projection_", start_year, "_", end_year, ".csv")
  output_file_path <- paste0("output_projections/for_tests/ets_windows/", output_file_name)
  
  fwrite(
    x = final_dt,
    file = output_file_path
  )
  
}

