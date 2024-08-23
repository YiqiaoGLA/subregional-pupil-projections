
## a function for resampling from the residuals
## the output is a data.table. It would be nice, but not essential, to have the choice to output a list

resample_for_bootstrapping <- function(input_vector, n_resamples){
  
  n <- length(input_vector)
  
  output_list <- list()
  i <- 1
  for(i in 1:n_resamples){
    
    resample <- sample(x = input_vector, size = n, replace = TRUE)
    
    resample <- abs(resample)*sign(runif(length(resample), -1, 1)) # NOTE - EXPERIMENTAL LINE! TO FORCE THE BOOTSTRAPPING TO BE SYMMETRIC
    
    output_list[[i]] <- resample
    
  }
  
  output_dt <- rbindlist(lapply(X = output_list, FUN = as.list))
  
  return(output_dt)
  
}

