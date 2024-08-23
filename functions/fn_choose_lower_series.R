## function to select the lower forecast from three input series
## in general I am trying to make all of my functions generalisable to other situations
## but this is an exception....maybe change this later

choose_lower_series <- function(input_ts_1, input_ts_2, input_ts_3){
  
  list_series <- list(input_ts_1, input_ts_2, input_ts_3)
  
  mean_1 <- mean(input_ts_1)
  mean_2 <- mean(input_ts_2)
  mean_3 <- mean(input_ts_3)
  
  min_ind <- which.min(c(mean_1, mean_2, mean_3))
  
  lower_series <- list_series[[min_ind]]
  
  return(lower_series)
  
}

