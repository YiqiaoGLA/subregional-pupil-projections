
## function to project forward an input ts object using exponential smoothing
## it is mostly a wrapper function but a useful one that makes the code a lot neater. The function allows you to define most of the parameters of forecasting an object based on exponential smoothing.

project_with_ets <- function(input_ts, 
                        model = "MMN", 
                        alpha = NULL,
                        beta = NULL,
                        damped = NULL,
                        phi = NULL,
                        periods_ahead){
  
  require(forecast)
  
  ets_model <- ets(y = input_ts, model = model, alpha = alpha, beta = beta, damped = damped, phi = phi) # creating an exponential smoothing model based on an input time series
  
  forecast_from_ets <- forecast(ets_model, h = periods_ahead, PI = FALSE) # projecting forwards from the model
  
  return(forecast_from_ets$mean) # returning just the mean value of the model - it comes with various other outputs but all we want is the mean
  
}

