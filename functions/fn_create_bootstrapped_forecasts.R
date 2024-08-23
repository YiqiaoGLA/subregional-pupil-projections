
## function to produce full range of bootstrapped forecasts
## will contain functions within functions, which I hate, but a bootstrapping function makes logical sense (does one clear thing, repeatable, etc) and will make my code a lot neater
## the only inputs are (1) a list of time series objects, all the same length of course, (2) the number of periods ahead to project

## AT THE MOMENT THERE IS A LOT OF TESTING WITHIN THIS FUNCTION, CURRENTLY COMMENTED OUT. THIS WILL NEED TO BE REMOVED, AFTER I HAVE SOLVED THE PROBLEM WITH THE PREDICTION INTERVALS. 

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
  
  ## TEST PLOT
  
  #geog <- "TLC1"
  
  #ylim_min <- min(resid_resamples[[geog]])
  #ylim_max <- max(resid_resamples[[geog]])
  
  #plot(x = 1, y = 1, type = "n", bty = "n",
  #     xlim = c(2012, 2022), ylim = c(ylim_min, ylim_max))
  
  #for(i in 1:1000){
    
  #  lines(x = 2012:2022, y = resid_resamples[[geog]][i,], col = "gray", lwd = 0.2)
    
  #}
  
  ## END OF TEST PLOT (outcome is that they all look fine, perfectly distributed around 0)
  
  
  ### add the resampled residuals to the original series
  ### instead of a for-loop, could be mapply with sweep as the function
  
  final_resamples <- list()

  for(i in 1:length(input_ts_list)){
    
    num_vec <- as.numeric(input_ts_list[[i]])
    resamples <- resid_resamples[[i]]
    
    final_resamples[[i]] <- data.table(sweep(resamples, 2, num_vec, "+"))
    
  }
  
  names(final_resamples) <- names(input_ts_list)
  
  
  ## TEST PLOT
  
  #geog <- "TLG3"
  
  #ylim_min <- min(final_resamples[[geog]])
  #ylim_max <- max(final_resamples[[geog]])
  
  #plot(x = 1, y = 1, type = "n", bty = "n",
  #     xlim = c(2012, 2022), ylim = c(ylim_min, ylim_max))
  
  #for(i in 1:1000){
    
  #  lines(x = 2012:2022, y = final_resamples[[geog]][i,], col = "gray", lwd = 0.2)
    
  #}
  
  #lines(x = 2012:2022, y = input_ts_list[[geog]], lwd = 3, col = "red")
  
  ## END OF TEST PLOT
  
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
  
  
  ## TEST PLOT
  
  #geog <- "TLF3"
  
  #ylim_min <- min(bootstrapped_forecasts[[geog]])
  #ylim_max <- max(bootstrapped_forecasts[[geog]])
  
  #plot(x = 1, y = 1, type = "n", bty = "n",
  #     xlim = c(2012, 2032), ylim = c(ylim_min, ylim_max))
  
  #for(i in 1:1000){
    
  #  lines(x = 2023:2032, y = bootstrapped_forecasts[[geog]][i,], col = "gray", lwd = 0.2)
    
  #}
  
  #lines(x = 2012:2022, y = input_ts_list[[geog]], col = "red", lwd = 2)
  
  #lines(x = 2023:2032, y = all_nc_years_projected_ratios[["year_group_7"]][[geog]],
  #      col = "blue", lwd = 2)
  
  ## TEST PLOT END
  
  ## NEW TEST PLOT
  
  #gray_col <- rgb(red = 211, green = 211, blue = 211, alpha = 100, maxColorValue = 255)

  
  #gray_col <- alpha("gray", alpha = 0.025)

  
  
  #geog <- "TLI5"
  
  #for(geog in all_geogs){
  
  #  ylim_min <- min(c(min(bootstrapped_forecasts[[geog]]), min(final_resamples[[geog]])))
  #  ylim_max <- max(c(max(bootstrapped_forecasts[[geog]]), max(final_resamples[[geog]])))
  
  #  png(filename = paste0("plots/temp_diagnostics/", geog, ".png"),
  #      height = 8, width = 10, units = "in", res = 500)
  
  #  plot(x = 1, y = 1, type = "n", bty = "n",
  #       xlim = c(2012, 2032), ylim = c(ylim_min, ylim_max))
  
  #  for(i in 1:1000){
    
  #    lines(x = 2012:2022, y = final_resamples[[geog]][i,], col = gray_col, lwd = 1.25)
    
  #  }
  
  #  lines(x = 2012:2022, y = input_ts_list[[geog]], col = "red", lwd = 2)
  
  #  for(i in 1:1000){
    
  #    lines(x = 2023:2032, y = bootstrapped_forecasts[[geog]][i,], col = gray_col, lwd = 1.25)
    
  #  }
  
  #  lines(x = 2023:2032, y = all_nc_years_projected_ratios[["year_group_7"]][[geog]],
  #      col = "blue", lwd = 2)
  
  #  dev.off()
  #  }
  
  ## NEW TEST PLOT END
  
  
  return(bootstrapped_forecasts)
  
}


