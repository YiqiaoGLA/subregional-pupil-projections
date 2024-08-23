## function to take in a time series, and project forward that series based on the average of the past x number of years
## designed for use on ratios, and will likely only be used for ratios, but could be used on any time series
## inputs are just an input time series, how many years to go back to take the average, and how far forward we want to project

project_with_past_averages <- function(input_ts, past_periods, periods_ahead){
  
  start_ind <- length(input_ts) - past_periods + 1 ## the +1 is because indexing is inclusive 
  end_ind <- length(input_ts)
  
  mean_value <- mean(input_ts[start_ind:end_ind]) # getting the mean of the x number of past years
  
  output_ts <- ts(data = rep(mean_value, periods_ahead), # the "series" is just the mean value, repeated for as many years as we want
                  start = max(time(input_ts)) + 1, # so that the projected time series starts the year after the input series
                  frequency = 1)
  
  return(output_ts)
  
}

