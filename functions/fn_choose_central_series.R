## function to select the central forecast from three input series
## in general I am trying to make all of my functions generalisable to other situations
## but this is an exception....maybe change this later

choose_central_series <- function(input_ts_1, input_ts_2, input_ts_3){
  
  list_series <- list(input_ts_1, input_ts_2, input_ts_3) # getting the input time series into the same list
  
  series_means <- unlist(lapply(X = list_series, FUN = mean)) # getting the means of each of the series
  
  central_ind <- which(series_means == median(series_means)) # choosing the series in the middle, via means, along with the line below
  
  central_series <- list_series[[central_ind]]
  
  return(central_series)
  
}


