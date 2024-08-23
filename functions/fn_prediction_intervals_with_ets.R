
## function to produce theoretical prediction intervals for an input ts object using exponential smoothing

prediction_intervals_with_ets <- function(input_ts, 
                             model = "MMN", 
                             alpha = NULL,
                             beta = NULL,
                             periods_ahead,
                             pi_level){
  
  ets_model <- ets(y = input_ts, model = model, alpha = alpha, beta = beta)
  
  forecast_from_ets <- forecast(ets_model, h = periods_ahead, PI = TRUE, level = pi_level)
  
  pis <- data.table(
    upper = forecast_from_ets$upper,
    lower = forecast_from_ets$lower
  )
  
  colnames(pis) <- paste0(colnames, "_", pi_level)
  
  return(pis)
  
}
