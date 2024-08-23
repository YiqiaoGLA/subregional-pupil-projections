## a function to extract the residuals from exponential smoothing modelling on a time series

extract_residuals_ets <- function(input_ts, 
                                  model = "MMN", 
                                  alpha = NULL,
                                  beta = NULL,
                                  damped = NULL,
                                  phi = NULL){
  
  ets_model <- ets(y = input_ts, model = model, alpha = alpha, beta = beta, damped = damped, phi = phi)
  
  ts_series <- ets_model$x
  
  ts_fit <- ets_model$fitted
  
  residuals <- as.numeric(ts_series - ts_fit)
  
  return(residuals)
  
}
