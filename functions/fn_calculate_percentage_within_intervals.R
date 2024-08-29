## this function is designed to evaluate the performance of generated predictions intervals. 
## it takes the input prediction intervals, the corresponding real series, and calculates the percentage of the time that the real series falls within the prediction intervals
## the years for the real series, the upper interval, adn the lower interval must be aligning

calculate_percentage_within_intervals <- function(real_series, upper_interval, lower_interval){
  
  log_within_intervals <- real_series <= upper_interval & real_series >= lower_interval # creating a logical vector (TRUE/FALSE) of whether the real data points fall within the prediction intervals
  
  perc_within_intervals <- 100*(sum(log_within_intervals)/length(log_within_intervals)) # turning this into a percentage (along with line below)
  
  perc_within_intervals <- round(perc_within_intervals, 3)

  return(perc_within_intervals)
    
}