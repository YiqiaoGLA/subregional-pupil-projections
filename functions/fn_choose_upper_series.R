## function to select the upper forecast from three input series
## in general I am trying to make all of my functions generalisable to other situations
## but this is an exception....maybe change this later

choose_upper_series <- function(input_ts_1, input_ts_2, input_ts_3){
  
  list_series <- list(input_ts_1, input_ts_2, input_ts_3) # putting all of the input series into one list
  
  mean_1 <- mean(input_ts_1) # getting the mean of each of the series
  mean_2 <- mean(input_ts_2)
  mean_3 <- mean(input_ts_3)
  
  max_ind <- which.max(c(mean_1, mean_2, mean_3)) # selecting the series with the highest mean (along with the line below)
  
  upper_series <- list_series[[max_ind]]
  
  return(upper_series)
  
}
