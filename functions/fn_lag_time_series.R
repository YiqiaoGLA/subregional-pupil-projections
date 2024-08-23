
## Function that lags a time series
## again, the years must be in order

lag_time_series <- function(ts_to_lag, 
                            years_to_lag){
  
  lagged_ts <- ts(data = ts_to_lag, 
                  start = time(ts_to_lag)[1] + years_to_lag, 
                  frequency = 1)
  
  return(lagged_ts)
  
}
