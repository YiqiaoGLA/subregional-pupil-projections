
## function to produce full range of bootstrapped forecasts
## will contain functions within functions, which I hate, but a bootstrapping function makes logical sense (does one clear thing, repeatable, etc) and will make my code a lot neater
## the only inputs are (1) a list of time series objects, all the same length of course, (2) the number of periods ahead to project

create_bootstrapped_forecasts <- function(input_ts_list, periods_ahead, damped = NULL, phi = NULL, model = "MMN"){
  
  ## extract the residuals
  resids <- lapply(
    X = input_ts_list,
    FUN = extract_residuals_ets,
    damped = damped,
    phi = phi,
    model = model
  )
  
  ### resample the residuals
  resid_resamples <- lapply(
    X = resids,
    FUN = resample_for_bootstrapping,
    n_resamples = 1000
  )
  
  ### add the resampled residuals to the original series
  ### instead of a for-loop, could be mapply with sweep as the function
  
  final_resamples <- list()

  for(i in 1:length(input_ts_list)){
    
    num_vec <- as.numeric(input_ts_list[[i]])
    resamples <- resid_resamples[[i]]
    
    final_resamples[[i]] <- data.table(sweep(resamples, 2, num_vec, "+"))
    
  }
  
  names(final_resamples) <- names(input_ts_list)
  
  ### forecast each resample
  ### using a for loop instead of an lapply. Better than lists within lists, I think, or an lapply within an lapply.
  
  first_year <- min(time(input_ts_list[[1]]))
  
  bootstrapped_forecasts <- list()
  
  for(i in 1:length(input_ts_list)){
    
    resamples_geog <- final_resamples[[i]]  
    
    resamples_geog_ts <- apply(X = resamples_geog, MARGIN = 1, FUN = ts, 
                               start = first_year, frequency = 1, simplify = FALSE)
    
    resamples_geog_forecast <- lapply(
      X = resamples_geog_ts,
      FUN = project_with_ets,
      periods_ahead = periods_ahead,
      damped = damped,
      phi = phi,
      model = model
    )
    
    resamples_geog_forecast_dt <- convert_tslist_to_dt(resamples_geog_forecast)
    
    bootstrapped_forecasts[[i]] <- resamples_geog_forecast_dt
    
  }
  
  names(bootstrapped_forecasts) <- names(input_ts_list)
  
  return(bootstrapped_forecasts)
  
}


