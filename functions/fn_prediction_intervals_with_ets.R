
## function to produce theoretical prediction intervals for an input ts object using exponential smoothing
## when you use exponential smoothing to cast forward a series with the forecast package, it automatically produces prediction intervals. This function produces just those prediction intervals. 

prediction_intervals_with_ets <- function(input_ts, 
                             model = "MMN", 
                             alpha = NULL,
                             beta = NULL,
                             periods_ahead,
                             pi_level){
  
  ets_model <- ets(y = input_ts, model = model, alpha = alpha, beta = beta) # making an exponential smoothing model based on the input time series, while setting parameters
  
  forecast_from_ets <- forecast(ets_model, h = periods_ahead, PI = TRUE, level = pi_level) # producing a projection from this model, setting parameters for the prediction intervals as appropriate
  
  pis <- data.table( # extracting the prediction intervals from the output projection
    upper = forecast_from_ets$upper,
    lower = forecast_from_ets$lower
  )
  
  colnames(pis) <- paste0(colnames, "_", pi_level) # renaming the columns 
  
  return(pis)
  
}
