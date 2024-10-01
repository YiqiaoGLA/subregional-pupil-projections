
#### this script takes the pupil numbers dataset at ITL2 region, the births dataset at ITL2 region, and matches them.
#### the key point of this script is to lag the datasets, so that we've matched reception/year one with births 4/5 years earlier. 
#### Because we are specifically interested in the relationship between births and later school numbers, and are using this to predict future pupil numbers. 

#### this is a discrete and clearly-definable enough task that it could be in a function. But I've decided against it. 
#### Because the script is short enough that I can do it all manually and keep it very readable, for the years I want it for. And I've decided that this only makes sense to do for reception and years 1. With year 7 there's too large a lag. 

## 0. libraries and functions

library(data.table)

source("scripts/0_inputs.R")


## 1. reading in datasets

pupils_filename <- paste0("data/processed_data/pupil_numbers/itl_pupil_numbers_1112_to_", final_school_period, ".csv")

births_filename <- paste0("data/processed_data/births/itl_births_92_to_", substr(max_year, 3, 4), ".csv")


pupils <- fread(pupils_filename)

births <- fread(births_filename)


## 2. simple data_processing steps - creating year variables for both datasets, and narrowing pupils down to reception & year 1 (TO DO - YEAR VARIABLES SHOULD BE CREATED EARLIER! In scripts 2 and 3a). 

births[, year := (tstrsplit(date, "-", fixed = TRUE)[1])]
births[, year := as.numeric(year)]


pupils[, year := as.numeric(substr(time_period, 1, 4))]

reception <- pupils[nc_year == "reception", ]

year_1 <- pupils[nc_year == "year_group_1"]


## 3. matching the datasets

  ### 3.1. creating the 4 and 5 year time lags in births
births_4 <- births
births_5 <- births

last_useful_year <- max_year - 1 # why was it this year again? Really need to check this and understand this. 

births_4[, year_lag_4 := year + 4]
births_4[year_lag_4 > last_useful_year | year_lag_4 < 2011, year_lag_4 := NA]

births_5[, year_lag_5 := year + 5]
births_5[year_lag_5 > last_useful_year | year_lag_5 < 2011, year_lag_5 := NA]

  ### 3.1. joining the datasets
setkey(births_4, "gss_code", "year_lag_4")
setkey(reception, "itl221cd", "year")

births_reception_joined <- births_4[reception]

setkey(births_5, "gss_code", "year_lag_5")
setkey(year_1, "itl221cd", "year")

births_year_1_joined <- births_5[year_1]


## 4. cleaning up the datasets

  ### 4.1. reception
to_keep <- c("gss_code", "gss_name", "year_lag_4", "annual_births", "headcount")

births_reception_joined <- births_reception_joined[, ..to_keep]

colnames(births_reception_joined) <- c("itl_cd", "itl_name", "year", "annual_births_lag4", "headcount")

  ### 4.2. year 1
to_keep <- c("gss_code", "gss_name", "year_lag_5", "annual_births", "headcount")

births_year_1_joined <- births_year_1_joined[, ..to_keep]

colnames(births_year_1_joined) <- c("itl_cd", "itl_name", "year", "annual_births_lag5", "headcount")


## 5. writing the datasets

output_reception_filename <- paste0("data/processed_data/combined/births_reception_lag4_", last_useful_year, ".csv")

fwrite(x = births_reception_joined,
       file = output_reception_filename)

output_year_1_filename <- paste0("data/processed_data/combined/births_year_1_lag5_", last_useful_year, ".csv")

fwrite(x = births_year_1_joined,
       file = output_year_1_filename)


rm(list = ls())
gc()
gc()
gc()
