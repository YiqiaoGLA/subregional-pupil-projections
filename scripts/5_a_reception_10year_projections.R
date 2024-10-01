
# creating 10 year projections for reception
# the full methodology for how this is done will be in  nearby methodology document. 
# but briefly, what is happening in the script is that births are projected 10 years forwards, the ratio of births to reception 4 years later is projected 10 years forwards, and the two series are multiplied by each other to get a projection of reception. 
# real births are used for the first few years of reception projections, and then projected births are used. 
# and then to get uncertainty, bootstrapping is used - the same process above is repeated 1,000 times, based on 1000 slight permutations of the historical time series in question. 

## 0. libraries and functions

source("scripts/0_inputs.R")

library(data.table)
library(forecast)

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)

## 1. reading in data, doing a few small cleaning tasks, and extracting vector of unique geographies

  ### 1.1. reading in combined lagged births and reception, and creating the ratio

last_useful_year <- max_year - 1
births_reception_filename <- paste0("data/processed_data/combined/births_reception_lag4_", last_useful_year, ".csv")

births_reception <- fread(births_reception_filename)

births_reception[, ratio := headcount/annual_births_lag4]


  ### 1.2. reading in births, creating year variable (TO DO - which really should be in an earlier script), creating lagged year variable narrowing down to desired years, and getting rid of forward slashes in the name 
births_filename <- paste0("data/processed_data/births/itl_births_92_to_", substr(max_year, 3, 4), ".csv")

births <- fread(births_filename) # TO DO - file name shouldn't be date specific

births[, year := (tstrsplit(date, "-", fixed = TRUE)[1])]
births[, year := as.numeric(year)]

births[, year_lag_4 := year + 4]

births <- births[year > 2010 & year < max_year, ] # NOTE - keeping in 2010 and not updating, meaning the time series for births will get longer for each year. Fine for now, and not going to make much of a difference, but this is something we might like to revisit later. 

births[, gss_name := gsub("/", "&", gss_name, fixed = TRUE)] # need to change name of one itl because it has a slash in it

  ### 1.3. extracting vector of unique geographies
all_geogs <- births[, unique(gss_code)]


## 2. converting the input data into lists, where each entry into the list is the data for one geography (the data being a time series of reception number). Doing this because the input into the forecasting functions has to be a time series.

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
  periods_ahead = 10
)


## 4. projecting forward births

  ### 4.1. projecting forward 10 years
projected_births <- lapply(
  X = all_ts_births_list,
  FUN = project_with_ets,
  periods_ahead = 10
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

  ### 4.4. adding the two time series (real births and projected births) together into one time series
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


## 6. generating full range of bootstrapped forecasts for ratio (which amounts to doing the same as above, sections 3-5, but a thousand times)

  ### 6.1. extract the residuals
ratio_resids <- lapply(
  X = all_ts_ratio_list,
  FUN = extract_residuals_ets
)

  ### 6.2. resample the residuals
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


  ### 6.4. forecasting each resample
  ### using a for loop instead of an lapply. Better than lists within lists, I think, or an lapply within an lapply.

first_year <- min(time(all_ts_ratio_list[[1]])) # as an input when I create the time series

ratio_bootstrapped_forecasts <- list()

for(i in 1:length(all_geogs)){
  
  ratio_resamples_geog <- final_ratio_resamples[[i]]  
  
  ratio_resamples_geog_ts <- apply(X = ratio_resamples_geog, MARGIN = 1, FUN = ts, 
                                   start = first_year, frequency = 1, simplify = FALSE)
  
  ratio_resamples_geog_forecast <- lapply(
    X = ratio_resamples_geog_ts,
    FUN = project_with_ets,
    periods_ahead = 10
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
    periods_ahead = 10
  )
  
  births_resamples_geog_forecast_dt <- convert_tslist_to_dt(births_resamples_geog_forecast)
  
  births_bootstrapped_forecasts[[i]] <- births_resamples_geog_forecast_dt
  
}

names(births_bootstrapped_forecasts) <- all_geogs


  ### 7.5. adding the real births data to the time series

    #### 7.5.1. getting each series of real births into a data.table, where it is repeated 1,000 times
    #### the reason for this is because they are real births, not projections, so we don't permute the series to create 1,000 slightly different series. There is no uncertainty related to them, and we want to reflect that. So instead we have the exact same time series, repeated 1,000 times, to be multiplied by the 1,000 permutations of the ratio series. 
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

  ### 8.1. combining projected births and ratios to get reception bootstrapped forecasts
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


## 10. saving the outputs
output_filename <- paste0("output_projections/initial_tenyear/reception_projections_", max_year, "_", max_year + 9, "_ratio_ets.csv")

fwrite(x = final_dt,
       file = output_filename)


rm(list = ls())
gc()
gc()
gc()
