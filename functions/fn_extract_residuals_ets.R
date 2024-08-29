## a function to extract the residuals from exponential smoothing modelling on a time series

extract_residuals_ets <- function(input_ts, 
                                  model = "MMN", 
                                  alpha = NULL,
                                  beta = NULL,
                                  damped = NULL,
                                  phi = NULL){
  
  ets_model <- ets(y = input_ts, model = model, alpha = alpha, beta = beta, damped = damped, phi = phi) # create the exponential smoothing model based on the input series and appropriate parameters
  
  ts_series <- ets_model$x # extracting the original values
  
  ts_fit <- ets_model$fitted # extracting the fitted values
  
  residuals <- as.numeric(ts_series - ts_fit) # getting the difference between real and fitted, i.e. the residuals
  
  return(residuals)
  
}
