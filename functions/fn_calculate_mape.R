## this function calculates mean absolute percentage errorm, a simple metric to assess accuracy of a set of projections against real values

calculate_mape <- function(real_series, projected_series){
  
  abs_differences <- abs(real_series - projected_series) # calculating the absolute difference between the real and projected series
  
  percentage_differences <- 100*(abs_differences/real_series) # expressing the differences as a percentage
  
  mape <- mean(percentage_differences) # getting the mean of these percentages
  
  mape <- round(mape, 3)
  
  return(mape)
  
}
