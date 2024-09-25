#### this script is focused on the ratio of pupils carrying over from year 1 to year 2, year 2 to year 3, year 3 to year 4, etc...
#### Because these ratios will be used to forecast all nc years past year group 1. 
#### we will take data on pupil numbers in either predicted year 1, or real years 2-10, and then use historical carryover ratios to simulate progression of this cohort up until year 11. 

#### But first, we need to use the historical carryover ratios to make projections of these ratios. This script first calculates all carryover ratios possible in the pupil numbers dataset, by ITL2 sub-region. 
#### then we forecast each series. So for TLC2, we'll have 10 years or so past data of the proportion of pupils who progress from year 2 to year 3. We then project this time series 10 years into the future. 

## 0. libraries and functions
library(data.table)
library(forecast)

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)


## 1. reading in data, small cleaning tasks
pupils <- fread("data/processed_data/pupil_numbers/itl_pupil_numbers_1112_to_2223.csv")

pupils[, year := as.numeric(substr(time_period, 1, 4))] ## creating the year variable

pupils <- pupils[, c("year", "itl221cd", "nc_year", "headcount")] ## narrowing down to columns we want

## 2. adding previous year onto current (as in, matching this year's year 1 with last year's reception, this year's year 2 with last year's year 1, etc)
## doing this by first creating an alternate dataset, with the same information aside from a different year variable. This year variable is lagged by one year. 
## There is also a new variable added to this dataset. This is next year's nc year. So for year 3, the corresponding entry in this new variable is year 4, as an example. 
## then we join the two datasets, in such a way as we end up with (for example), year 1 in 2011, year 2 in 2012, year 3 in 2013, etc...

  ### 2.1. creating duplicate dataset, containing last year's headcount for last year's students
pupils_lagged <- data.table(
  year_lag1 = pupils[, year] + 1,
  itl221cd = pupils[, itl221cd],
  nc_year = pupils[, nc_year],
  previous_headcount = pupils[, headcount]
)

prev_ncyear_lookup <- data.table(
  current_nc_year = c("reception" ,"year_group_1", "year_group_2", "year_group_3", "year_group_4", "year_group_5", "year_group_6", 
                       "year_group_7", "year_group_8", "year_group_9", "year_group_10", "year_group_11", "year_group_12", "year_group_13"),
  next_nc_year = c("year_group_1" ,"year_group_2", "year_group_3", "year_group_4", "year_group_5", "year_group_6", "year_group_7", 
                      "year_group_8", "year_group_9", "year_group_10", "year_group_11", "year_group_12", "year_group_13", "year_group_14")
)

setkey(pupils_lagged, "nc_year")
setkey(prev_ncyear_lookup, "current_nc_year")

pupils_lagged <- prev_ncyear_lookup[pupils_lagged]

pupils_lagged <- pupils_lagged[, c("year_lag1", "itl221cd", "next_nc_year", "previous_headcount")]


  ### 2.2. joining the two datasets and calculating the proportion carrying over
pupils <- pupils_lagged[pupils, on = c(year_lag1 = "year", itl221cd = "itl221cd", next_nc_year = "nc_year")]

pupils <- pupils[, c("year_lag1", "itl221cd", "next_nc_year", "headcount", "previous_headcount")] # selecting and rearranging columns

pupils[, proportion_continuing := headcount/previous_headcount]

pupils <- pupils[!is.na(proportion_continuing),] # removing na values, that are generated from nc years where there is no previous nc year. 



## 3. quick cleaning, writing the final dataset as a standalone output, before doing any projections. 

colnames(pupils)[colnames(pupils) == "next_nc_year"] <- "nc_year"
colnames(pupils)[colnames(pupils) == "itl221cd"] <- "itl22cd"
colnames(pupils)[colnames(pupils) == "year_lag1"] <- "year"

fwrite(x = pupils,
       file = "data/processed_data/pupil_numbers/pupils_ratio_carrying_over.csv")


## 4. projecting forwards the ratios for each of the years

pupils <- pupils[!(nc_year %in% c("year_group_12", "year_group_13", "year_group_14")), ] # assuming for now that we're not modelling past year 11. Might need to be revised later. 

  ### 4.1. getting the ratio series for each geography for each nc_year into a time series object. 
  ### using a for loop within a for loop here. It's typically not recommended to do this, but I think this is more readable than any alternatives (such as a complicated structure of lapplys, or an lapply in a loop) and there isn't a meaningful performance punishment. 

nc_years <- c("year_group_2", "year_group_3", "year_group_4", "year_group_5", "year_group_6", 
              "year_group_7", "year_group_8", "year_group_9", "year_group_10", "year_group_11")

geogs <- pupils[, unique(itl22cd)]

all_ratios_ts_list <- list()


for(j in 1:length(nc_years)){ # first for loop - choosing an nc year, filtering the dataset to this nc year

  nc_year_j <- nc_years[j]
  
  nc_year_data <- pupils[nc_year == nc_year_j, c("year", "itl22cd", "proportion_continuing")]

  nc_year_ratio_tslist <- list()

  for(i in 1:length(geogs)){ # then, for this nc year, looping through the geographies and making a time series. 
                             # Meaning we will end up with a two-level list. For each nc year, a list with each entry containing the historical series for a geography. 
                             # And then all of the nc years are in one big list. 
    
    geog <- geogs[i]
  
    geog_ts <- ts(data = nc_year_data[itl22cd == geog, proportion_continuing],
                  start = min(nc_year_data[itl22cd == geog, year]),
                  frequency = 1)
  
    nc_year_ratio_tslist[[i]] <- geog_ts
  
  }

  names(nc_year_ratio_tslist) <- geogs

  all_ratios_ts_list[[j]] <- nc_year_ratio_tslist
  
}

names(all_ratios_ts_list) <- nc_years


  ### 4.2. projecting forward the ratios
all_nc_years_projected_ratios <- list()

for(j in 1:length(nc_years)){
  
  nc_year_tslist <- all_ratios_ts_list[[j]]
  
  projected_nc_year_ratio <- lapply(
    X = nc_year_tslist,
    FUN = project_with_ets,
    periods_ahead = 10,
    model = "MNN"# MNN because were are telling it to project no trend. So we're just holding this ratio series constant. 
  )
  
  all_nc_years_projected_ratios[[j]] <- projected_nc_year_ratio
  
}

names(all_nc_years_projected_ratios) <- nc_years


## 5. calculating uncertainty for the projections above. We're not interesting in producing predictions for the ratios themselves, but instead for using the uncertainty here as an input into the uncertainty for the final projection, when we multiply these ratios by the starting cohort.
## when the usual method of calculating uncertainty was used - resampling the input data, repeating the projection on each of these resamples - we ended up with very asymmetric distributions of bootstrapped projections around the central forecast. And given that a starting cohort would be multiplied by these asymmetric distributions again and again, the end resulting prediction intervals were very skewed. 
## so now, what we are doing is taking the prediction intervals that are produced automatically by the forecast.ets method, and producing 1,000 bootstrapped samples around these. This way, we have produced a set of bootstrapped projections that are perfectly symmetrically distributed around the central forecast, but that is grounded in a real uncertainty calculation. 
## We are assuming a normal distribution around the estimate, which is probably not a sound assumption, but I think it's good enough for this case. 

  ### 5.1. getting the prediction intervals for the projection above
all_nc_years_prediction_intervals <- list()

for(j in 1:length(nc_years)){
  
  nc_year_tslist <- all_ratios_ts_list[[j]]
  
  pis_nc_year_ratio <- lapply(
    X = nc_year_tslist,
    FUN = prediction_intervals_with_ets,
    periods_ahead = 10,
    pi_level = 95,
    model = "MNN"# MNN because were are telling it to project no trend. So we're just holding this ratio series constant. 
  )
  
  all_nc_years_prediction_intervals[[j]] <- pis_nc_year_ratio
  
}

names(all_nc_years_prediction_intervals) <- nc_years


  ### 5.2. calculating the standard error from the 95% prediction intervals
all_nc_years_standard_errors <- list()

for(i in 1:length(nc_years)){
  
  nc_year_pis <- all_nc_years_prediction_intervals[[i]]
  
  nc_year_se <- lapply(
    X = nc_year_pis,
    FUN = function(input_df){return((input_df[,1] - input_df[, 2])/3.92)} # confidence interval to standard error conversion is the difference between the upper and lower limits, divided by 3.92. Because the confidence intervals are calculated as -+1.96*standard_error. 
  )
  
  all_nc_years_standard_errors[[i]] <- nc_year_se
  
}

names(all_nc_years_standard_errors) <- nc_years


  ### 5.3. creating the "bootstrapped samples" from the central forecast and the standard errors
  ### looping through each geography and each nc_year, and creating 1,000 bootstrapped versions of the projection by drawing from a normal distribution, with the central forecast as the mean and the standard error calculated above as the standard deviation

min_year_out <- min(time(all_nc_years_projected_ratios[[1]][[1]])) # these are needed for the column names of the output data.table
max_year_out <- max(time(all_nc_years_projected_ratios[[1]][[1]]))

simulated_bootstraps_all <- list()

for(i in 1:length(nc_years)){
  
  nc_year_sel <- nc_years[i] # selecting the nc year
  
  nc_year_all_geogs <- list()
  
  for(j in 1:length(geogs)){
    
    geog <- geogs[j] # selecting the geography
    
    rats <- all_nc_years_projected_ratios[[nc_year_sel]][[geog]] # extracting the ratio series for the nc year and geography specified
    ses <- all_nc_years_standard_errors[[nc_year_sel]][[geog]] # extracting the standard error around the ratio for the nc year and geography specified
    
    rats <- as.numeric(rats)
    
    simulated_bootstraps <- mapply( # drawing the 1,000 samples of the ratio projecting
      FUN = rnorm,
      mean = rats,
      sd = ses,
      n = 1000,
      SIMPLIFY = TRUE
    )
    
    simulated_bootstraps <- data.table(simulated_bootstraps)
  
    names(simulated_bootstraps) <- paste0("year_", min_year_out:max_year_out)
  
    nc_year_all_geogs[[j]] <- simulated_bootstraps
        
  }
  
  names(nc_year_all_geogs) <- geogs
  
  simulated_bootstraps_all[[i]] <- nc_year_all_geogs
  
}

names(simulated_bootstraps_all) <- nc_years

bootstrapped_all <- simulated_bootstraps_all # I should fix the script so that this isn't needed - this is because "bootstrapped all" was the name of the output of the original way of getting the 1,000 bootstrapped samples of the ratio


## 6. getting the projections into one data.table

projected_ratios_dt_list <- list()

for(j in 1:length(all_nc_years_projected_ratios)){
  
  nc_year_projections_dt <- lapply(
    X = all_nc_years_projected_ratios[[j]],
    FUN = function(input_ts){return(data.table(year = time(input_ts), projected_proportion_continuing = input_ts))}
  )
  
  nc_year_projections_dt <- rbindlist(nc_year_projections_dt, idcol = "itl22cd")
  
  projected_ratios_dt_list[[j]] <- nc_year_projections_dt
  
}

names(projected_ratios_dt_list) <- nc_years

projected_ratios_dt <- rbindlist(projected_ratios_dt_list, idcol = "nc_year")


## 7. writing the final output

projected_ratios_dt <- projected_ratios_dt[order(nc_year, itl22cd, year), ]
projected_ratios_dt <- projected_ratios_dt[, c("year", "itl22cd", "nc_year", "projected_proportion_continuing")]

fwrite(x = projected_ratios_dt,
       file = "data/processed_data/pupil_numbers/pupils_projected_ratio_carrying_over.csv")

saveRDS(object = bootstrapped_all,
        file = "output_projections/intermediate_outputs/full_bootstrapped_ratios_carryover.RDS")



## SOMETHING TO CHECK

## I am still a little unsure about whether I need to make a correction for standard deviation. 
## One way to test is to calculate prediction intervals for the ratios by the method of taking percentiles of the bootstrapped samples I've created. 
## If I was right in using the standard error as the input into rnorm, then intervals produced using this method should be very similar to the intervals created by using what the forecast.ets method automatically produces. 
## yes, I was write to use standard error as the input for standard deviation, without any further corrections of modifications. I'm leaving this bit of code in here now, in case I need to justify myself later. 

j <- 10

nc_year <- nc_years[j]

par(mfrow = c(4, 9))
par(mai = c(0.1, 0.1, 0.1, 0.1))

for(i in 1:length(geogs)){
  
  geog <- geogs[i]
  
  from_function_pis <- all_nc_years_prediction_intervals[[nc_year]][[geog]]
  
  from_bootstraps <- bootstrapped_all[[nc_year]][[geog]]
  from_bootstraps_pis <- extract_bootstrapped_prediction_intervals(from_bootstraps)
  
  ylim_min <- min(c(min(from_function_pis), min(from_bootstraps_pis)))
  
  ylim_max <- max(c(max(from_function_pis), max(from_bootstraps_pis)))
  
  
  plot(x = 2023:2032, y = rep(1, 10), type = "n", bty = "n", las = 3, ylab = "n", xlab = "n", yaxt = "n", xaxt = "n",
       ylim = c(ylim_min, ylim_max))
  
  lines(x = 2023:2032, y = unlist(from_function_pis[, 1]), lwd = 3, col = "blue")
  lines(x = 2023:2032, y = unlist(from_function_pis[, 2]), lwd = 3, col = "blue")
  
  lines(x = 2023:2032, y = unlist(from_bootstraps_pis[, 1]), lwd = 3, col = "red")
  lines(x = 2023:2032, y = unlist(from_bootstraps_pis[, 2]), lwd = 3, col = "red")
  
}

par(mfrow = c(1, 1))


