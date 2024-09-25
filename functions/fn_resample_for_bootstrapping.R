
## a function for resampling from the residuals. Well, can resample from anything, but will be used in this project to resample from residuals
## the output is a data.table. It would be nice, but not essential, to have the choice to output a list

resample_for_bootstrapping <- function(input_vector, n_resamples){
  
  ### setting the size of each resample, which will be the same size as the input vector
  n <- length(input_vector)
  
  output_list <- list()

  ### resampling with replacement from the input vector. Taking as many resamples as was set at the beginning of the function
  for(i in 1:n_resamples){
    
    resample <- sample(x = input_vector, size = n, replace = TRUE)
    
    #resample <- abs(resample)*sign(runif(length(resample), -1, 1)) # NOTE - EXPERIMENTAL LINE! TO FORCE THE BOOTSTRAPPING TO BE SYMMETRIC
    
    output_list[[i]] <- resample
    
  }
  
  ### for each iteration of the loop, the resample was saved in a list. The line below converts from a list into a data.table
  output_dt <- rbindlist(lapply(X = output_list, FUN = as.list))
  
  return(output_dt)
  
}

