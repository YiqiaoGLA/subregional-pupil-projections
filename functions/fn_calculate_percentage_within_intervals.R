
calculate_percentage_within_intervals <- function(real_series, upper_interval, lower_interval){
  
  log_within_intervals <- real_series <= upper_interval & real_series >= lower_interval
  
  perc_within_intervals <- 100*(sum(log_within_intervals)/length(log_within_intervals))
  
  perc_within_intervals <- round(perc_within_intervals, 3)

  return(perc_within_intervals)
    
}