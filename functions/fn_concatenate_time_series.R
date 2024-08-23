
## function to concatenate two time series together
## it would be nice to extend this function so that (1) it could take any number of time series, not just two, and (2) they don't have to inputted in order

concatenate_time_series <- function(time_series_1, time_series_2){
  
  combined_times <- c(time(time_series_1), time(time_series_2))
  
  combined_series <- c(time_series_1, time_series_2)
  
  ### NOTE - need to do some check on combined series here, to make sure no duplicate years, runs, in order....etc
  
  combined_ts_object <- ts(data = combined_series, start = combined_times[1], frequency = 1)
  
  return(combined_ts_object)
}

