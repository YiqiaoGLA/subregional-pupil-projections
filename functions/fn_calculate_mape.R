
calculate_mape <- function(real_series, projected_series){
  
  abs_differences <- abs(real_series - projected_series)
  
  percentage_differences <- 100*(abs_differences/real_series)
  
  mape <- mean(percentage_differences)
  
  mape <- round(mape, 3)
  
  return(mape)
  
}
