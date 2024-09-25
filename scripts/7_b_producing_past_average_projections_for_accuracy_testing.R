## this script creates one of the two datasets for accuracy testing. This is the script that produces projections using a more naive method that we want to test against and hopefully improve on, i.e. using and applying past averages of the ratio series, and creating a rough estimate of uncertainty by varying how long we go back to take the past average.
## There is a more detailed description of the scheme for accuracy testing in the report/methodology document. But we're using a "window approach", where we produce projections based on all possible windows of real data, of varying series length.
## I am effectively using the same script as script 5_a, the one that produces the 10 year final projections for reception. But it is amended so that most of the modelling is done within a for-loop, looping over different input years, so that we end up with lots of projections to go over. 
## is it also amended to use a different function for projecting the ratio series forward - one that uses past averages of ratios rather than the exponential smoothing
## the script I'm using is a bit clunky and not a very elegant or pleasing solution, but it works just fine and produces the projection I want for real vs actual

## the past averages for ratios that I'm using are 1, 3, 5 years back. If I change 5 to 4, then we'll have more windows for accuracy testing, and crucially we'll have more years to test when there is no real births data but only projected births data. 
## possibly to do after. 


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
  
  ## 2. getting births projections
  
  ### 2.1. getting births into a list of time series objects
  all_ts_births_list <- list()
  
  for(i in 1:length(all_geogs)){
    
    geog <- all_geogs[i]
    
    all_ts_births_list[[i]] <- ts(data = births[gss_code == geog, annual_births], 
                                  start = min(births[, year]), 
                                  frequency = 1)
    
  }
  
  names(all_ts_births_list) <- all_geogs
  
  ### 2.2. projecting forward 10 years
  projected_births <- lapply(
    X = all_ts_births_list,
    FUN = project_with_ets,
    periods_ahead = years_ahead
  )
  
  ### 2.3. lagging births by four years
  projected_births_lagged <- lapply(
    X = projected_births,
    FUN = lag_time_series,
    years_to_lag = 4
  )
  
  ### 2.4. extracting the real lagged births data to replace the four "missing" years at the start, as ts objects within a list
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
  
  ### 2.5. adding the two time series (real births and projected births) together
  real_and_projected_births <- mapply(
    FUN = concatenate_time_series,
    time_series_1 = real_data_list,
    time_series_2 = projected_births_lagged,
    SIMPLIFY = FALSE
  )
  
  
  ## 3. projecting forward ratios
  
  ### 3.1. converting ratios into a list of ts objects
  all_ts_ratio_list <- list()
  
  for(i in 1:length(all_geogs)){
    
    geog <- all_geogs[i]
    
    all_ts_ratio_list[[i]] <- ts(data = births_reception[itl_cd == geog, ratio], 
                                 start = min(births_reception[, year]), 
                                 frequency = 1)
    
  }
  
  names(all_ts_ratio_list) <- all_geogs
  
  
  ### 3.2. projecting forward ratios, based on the past 1, 3, and 5 years of data (4 instead of 5 would give more series for accuracy testing. Consider that)
  projected_ratios_1 <- mapply(
    FUN = project_with_past_averages,
    input_ts <- all_ts_ratio_list,
    past_periods = 1,
    periods_ahead = years_ahead,
    SIMPLIFY = FALSE
  )
  
  projected_ratios_3 <- mapply(
    FUN = project_with_past_averages,
    input_ts <- all_ts_ratio_list,
    past_periods = 3,
    periods_ahead = years_ahead,
    SIMPLIFY = FALSE
  )
  
  projected_ratios_5 <- mapply(
    FUN = project_with_past_averages,
    input_ts <- all_ts_ratio_list,
    past_periods = 5,
    periods_ahead = years_ahead,
    SIMPLIFY = FALSE
  )
  
  
  ## 4. getting the projected reception series
  
  reception_forecasts_1 <- mapply(
    FUN = get_ts_product,
    time_series_1 = projected_ratios_1,
    time_series_2 = real_and_projected_births,
    SIMPLIFY = FALSE
  )
  
  reception_forecasts_3 <- mapply(
    FUN = get_ts_product,
    time_series_1 = projected_ratios_3,
    time_series_2 = real_and_projected_births,
    SIMPLIFY = FALSE
  )
  
  reception_forecasts_5 <- mapply(
    FUN = get_ts_product,
    time_series_1 = projected_ratios_5,
    time_series_2 = real_and_projected_births,
    SIMPLIFY = FALSE
  )
  
  ## 5. selecting the upper, central, and lower forecasts
  
  upper_bound <- mapply(
    FUN = choose_upper_series,
    input_ts_1 = reception_forecasts_1,
    input_ts_2 = reception_forecasts_3,
    input_ts_3 = reception_forecasts_5,
    SIMPLIFY = FALSE
  )
  
  lower_bound <- mapply(
    FUN = choose_lower_series,
    input_ts_1 = reception_forecasts_1,
    input_ts_2 = reception_forecasts_3,
    input_ts_3 = reception_forecasts_5,
    SIMPLIFY = FALSE
  )
  
  central_forecast <- mapply(
    FUN = choose_central_series,
    input_ts_1 = reception_forecasts_1,
    input_ts_2 = reception_forecasts_3,
    input_ts_3 = reception_forecasts_5,
    SIMPLIFY = FALSE
  )
  
  ## 6. converting it all to a single data.table
  
  ### 6.1. reshaping upper forecast (the reshapings could be dealt with using a function, and I have been copying and pasting the code...but I don't like overly specific, single-use functions)
  upper_bound <- convert_tslist_to_dt(upper_bound)
  upper_bound[, itl22cd := all_geogs]
  
  upper_bound <- melt(data = upper_bound, id.vars = "itl22cd",
                      variable.name = "year", value.name = "upper_pi")
  
  ### 6.2. reshaping central forecast
  central_forecast <- convert_tslist_to_dt(central_forecast)
  central_forecast[, itl22cd := all_geogs]
  
  central_forecast <- melt(data = central_forecast, id.vars = "itl22cd",
                           variable.name = "year", value.name = "mean_projection")
  
  ### 6.3. reshaping lower forecast
  lower_bound <- convert_tslist_to_dt(lower_bound)
  lower_bound[, itl22cd := all_geogs]
  
  lower_bound <- melt(data = lower_bound, id.vars = "itl22cd",
                      variable.name = "year", value.name = "lower_pi")
  
  ### 6.4. joining the tables into one final output
  setkey(upper_bound, "itl22cd", "year")
  setkey(lower_bound, "itl22cd", "year")
  
  bounds <- upper_bound[lower_bound]
  
  setkey(bounds, "itl22cd", "year")
  setkey(central_forecast, "itl22cd", "year")
  
  final_output <- central_forecast[bounds]
  
  final_output[, year := gsub("year_", "", year)]
  
  
  ## 7. writing the output
  
  output_file_name <- paste0("past_average_projection_", start_year, "_", end_year, ".csv")
  output_file_path <- paste0("output_projections/for_tests/past_average_windows/", output_file_name)
  
  fwrite(
    x = final_output,
    file = output_file_path
  )
  
  
}

