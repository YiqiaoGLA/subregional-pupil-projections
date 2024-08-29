
## function for extracting prediction intervals
## the input is a data.table where each row is a bootstrapped resample, and each column is a year
## input options for level are 90 and 95. For now

extract_bootstrapped_prediction_intervals <- function(input_bootstrap_dt, pi_level = 95){
  
  ### setting the right quantiles for the desired prediction intervals
  if(pi_level == 95){
    
    lower <- 0.025
    upper <- 0.975
    
  }else if(pi_level == 90){
    
    lower <- 0.05
    upper <- 0.95
    
  }
  
  ### for each point along the series, extracting the upper quantile from the bootstrapped series
  upper_pi <- apply(
    X = input_bootstrap_dt,
    MARGIN = 2,
    FUN = quantile,
    probs = upper
  )
  
  ### for each point along the series, extracting the lower quantile from the bootstrapped series
  lower_pi <- apply(
    X = input_bootstrap_dt,
    MARGIN = 2,
    FUN = quantile,
    probs = lower
  )
  
  ### putting them into a single output
  output_pis <- data.table( # I can easily change this function to output either a list or a data.table
    upper_pi = upper_pi,
    lower_pi = lower_pi
  )
  
  return(output_pis)
  
}
