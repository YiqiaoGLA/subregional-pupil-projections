
## function to project forward an input ts object using exponential smoothing

project_with_ets <- function(input_ts, 
                        model = "MMN", 
                        alpha = NULL,
                        beta = NULL,
                        damped = NULL,
                        phi = NULL,
                        periods_ahead){
  
  require(forecast)
  
  ets_model <- ets(y = input_ts, model = model, alpha = alpha, beta = beta, damped = damped, phi = phi)
  
  forecast_from_ets <- forecast(ets_model, h = periods_ahead, PI = FALSE)
  
  return(forecast_from_ets$mean)
  
}

