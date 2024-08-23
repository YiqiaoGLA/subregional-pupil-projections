## function to select the central forecast from three input series
## in general I am trying to make all of my functions generalisable to other situations
## but this is an exception....maybe change this later

choose_central_series <- function(input_ts_1, input_ts_2, input_ts_3){
  
  list_series <- list(input_ts_1, input_ts_2, input_ts_3)
  
  series_means <- unlist(lapply(X = list_series, FUN = mean))
  
  central_ind <- which(series_means == median(series_means))
  
  central_series <- list_series[[central_ind]]
  
  return(central_series)
  
}


