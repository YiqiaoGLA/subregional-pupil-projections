#### this script is focused on the ratio of pupils carrying over from year 1 to year 2, year 2 to year 3, year 3 to year 4, etc...
#### Because these ratios will be used to forecast all nc years past year group 1. 
#### we will take data on pupil numbers in either predicted year 1, or real years 2-10, and then use historical carryover ratios to simulate progression of this cohort up until year 11. 

#### But first, we need to use the historical carryover ratios to make projections of these ratios. This script first calculates all carryover ratios possible in the pupil numbers dataset, by ITL2 sub-region. 
#### then we forecast each series. So for TLC2, we'll have 10 years or so past data of the proportion of pupils who progress from year 2 to year 3. We then project this time series 10 years into the future. 


## 0. libraries and functions
library(data.table)
library(forecast)
library(foreach)
library(doParallel)

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
    model = "MNN"#, this should be MMN. I changed in to MNN to get rid of any trend in the projection, for testing on a problem. I will change it back and run it later. 
    #damped = TRUE,
    #phi = 0.8 # setting phi (the dampening parameter) manually leads to the flattest trends. Which is what we want. However, I'm worried that this will lead to high errors when it comes to bootstrapping. For later. 
  ) # changed these last two from TRUE and 0.8, to see if the results look reasonable up to year 11. 
  
  all_nc_years_projected_ratios[[j]] <- projected_nc_year_ratio
  
}

names(all_nc_years_projected_ratios) <- nc_years


  ### 4.3. calculating uncertainty for the ratios
  ### I won't explain here the overall process for calculating uncertainty. There will be a methodology document that explain this close to wherever this script is. 

    #### 4.3.1. getting the bootstrapped forecasts with parallel computing
n_cores <- detectCores()

cores_to_use <- round(0.75*n_cores)

cl <- makeCluster(cores_to_use)

clusterEvalQ(cl = cl, expr = c(library(forecast),
                               library(data.table)))

clusterExport(cl = cl, c("nc_years", "all_ratios_ts_list", "create_bootstrapped_forecasts", "extract_residuals_ets", 
                         "resample_for_bootstrapping", "project_with_ets", "convert_tslist_to_dt"), envir = environment())

registerDoParallel(cl)

bootstrapped_all <- foreach(j = 1:length(nc_years)) %dopar% { 
  
  nc_year_ratios <- all_ratios_ts_list[[j]]
  
  nc_year_ratios_bootstrapped <- create_bootstrapped_forecasts(input_ts_list = nc_year_ratios, periods_ahead = 10, model = "MNN"#, damped = TRUE, phi = 0.8
                                                               )
  
}

names(bootstrapped_all) <- names(all_ratios_ts_list)


    #### 4.3.2. extracting the prediction intervals from the bootstrapped forecasts (because after the bootstrapping, what we have is 1,000 different versions of the projection. To get 95% confidence intervals, we need to take the 97.5th and the 2.5th percentile for each year).
nc_year_pis_all <- list()

for(j in 1:length(bootstrapped_all)){
  
  nc_year_bootstrapped <- bootstrapped_all[[j]]
  
  nc_year_pis <- lapply(
    X = nc_year_bootstrapped,
    FUN = extract_bootstrapped_prediction_intervals
  )
  
  nc_year_pis <- lapply(
    X = nc_year_pis,
    FUN = function(input_dt){return(input_dt[, year := 2023:2032])} # TO REVISIT. Need to make this automated. The specific years that are added to the data.tables. 
  )
  
  nc_year_pis_all[[j]] <- nc_year_pis
  
}

names(nc_year_pis_all) <- names(bootstrapped_all)


## 5. getting the data into one data.table (what we have at the moment is a list of data.tables with the projections, and a list of data.tables with the uncertainty estimates)

  ### 5.1. getting the projections into one data.table
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


  ### 5.2. getting the uncertainty estimates into one data.table
pis_dt_list <- list()

for(j in 1:length(nc_year_pis_all)){
  
  nc_pis <- nc_year_pis_all[[j]]
  
  nc_pis_dt <- rbindlist(nc_pis, idcol = "itl22cd")
  
  pis_dt_list[[j]] <- nc_pis_dt
  
}

names(pis_dt_list) <- names(nc_year_pis_all)

pis_dt <- rbindlist(pis_dt_list, idcol = "nc_year")


  ### 5.3. joining the two tables
projected_ratios_dt[, year := as.numeric(year)]
projected_ratios_dt[, projected_proportion_continuing := as.numeric(projected_proportion_continuing)]


projected_ratios_dt <- projected_ratios_dt[order(nc_year, itl22cd, year), ]
pis_dt <- pis_dt[order(nc_year, itl22cd, year), ]


projected_ratios_dt_pi <- pis_dt[projected_ratios_dt, on = c(nc_year = "nc_year", itl22cd = "itl22cd", year = "year")]


## 6. writing the final output

projected_ratios_dt_pi <- projected_ratios_dt_pi[, c("year", "itl22cd", "nc_year", "projected_proportion_continuing", "upper_pi", "lower_pi")]

fwrite(x = projected_ratios_dt_pi,
       file = "data/processed_data/pupil_numbers/pupils_projected_ratio_carrying_over.csv")

saveRDS(object = bootstrapped_all,
        file = "output_projections/intermediate_outputs/full_bootstrapped_ratios_carryover.RDS") # again, need a better place for these intermediate outputs

