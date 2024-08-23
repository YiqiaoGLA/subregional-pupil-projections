
## script to process school census pupil numbers data from different years into the same format, in the same dataset

## 0. setting up libraries, functions, defaults, etc
library(data.table)

functions_to_read <- list.files("functions")

lapply(
  paste0("functions/", functions_to_read),
  FUN = source
)


## 1. reading in data
dataset_names <- list("1112_ncy.csv", "1213_ncy.csv", "1314_ncy.csv", "1415_ncy.csv", "1516_2223_ncy.csv")

dir <- "data/raw_data/pupil_numbers/"

pupil_data <- lapply(
  X = paste0(dir, dataset_names),
  FUN = fread
)

names(pupil_data) <- gsub(".csv", "", dataset_names)


## 2. cleaning each of the datasets and getting them into one dataset
## most of the rest of this script is in section 2. This is because the reformatting is just one step after another after another, in discrete steps that could be done in any order. So there is no more logical way of structuring this script that just lots of bits.

  ### 2.1. splitting out the two series. Before 2015/2016, each pupil numbers dataset is in a separate file for each year. From 2015/2016 onwards, the data is already nicely formatted and in one dataset. Meaning the processing steps pre- and post- 2015/2016 will need to be different in this initial stage of the processing. 
  ### most of the steps in section 2 will be about processing the pre 2015/2016 data
processed_pupil_data <- pupil_data[[5]]

pupil_data <- pupil_data[1:4]


  ### 2.2. converting column names of all to lowercase
pupil_data <- lapply(
  X = pupil_data,
  FUN = colnames_lower
)


  ### 2.3. adding underscores if there are spaces in column headings 
pupil_data <- lapply(
  X = pupil_data,
  FUN = add_underscores
)


  ### 2.4 selecting only the columns that we need in the datasets

    #### 2.4.1. defining the columns we need - they vary year to year, so there needs to be a different character vector for each year
desc_cols_1112 <- c("old_la_code", "la_name", "new_la_code", "school_type")
desc_cols_1213 <- c("old_la_code", "la_name", "new_la_code", "school_type")

desc_cols_1314 <- c("old_la_code", "la_name", "new_la_code", "phase-type_grouping")
desc_cols_1415 <- c("old_la_code", "la_name", "new_la_code", "phase-type_grouping")

    #### 2.4.2. defining the NC year columns - they are the same across all years
    #### we keep these separate as we'll use these later to pivot the NC year data from wide to long

nc_year_cols <- c("part_time_girls_nursery_1",
  "part_time_girls_nursery_2", "part_time_girls_reception", "part_time_girls_year_group_1", "part_time_girls_year_group_2",
  "part_time_girls_year_group_3", "part_time_girls_year_group_4", "part_time_girls_year_group_5", "part_time_girls_year_group_6",
  "part_time_girls_year_group_7", "part_time_girls_year_group_8", "part_time_girls_year_group_9", "part_time_girls_year_group_10", 
  "part_time_girls_year_group_11", "part_time_girls_year_group_12", "part_time_girls_year_group_13", "part_time_girls_year_group_14", 
  "full_time_girls_nursery_1", "full_time_girls_nursery_2", "full_time_girls_reception", 
  "full_time_girls_year_group_1", "full_time_girls_year_group_2", "full_time_girls_year_group_3", "full_time_girls_year_group_4", 
  "full_time_girls_year_group_5", "full_time_girls_year_group_6", "full_time_girls_year_group_7", "full_time_girls_year_group_8", 
  "full_time_girls_year_group_9", "full_time_girls_year_group_10", "full_time_girls_year_group_11", "full_time_girls_year_group_12", 
  "full_time_girls_year_group_13", "full_time_girls_year_group_14", "part_time_boys_nursery_1", 
  "part_time_boys_nursery_2", "part_time_boys_reception", "part_time_boys_year_group_1", "part_time_boys_year_group_2", 
  "part_time_boys_year_group_3", "part_time_boys_year_group_4", "part_time_boys_year_group_5", "part_time_boys_year_group_6", 
  "part_time_boys_year_group_7", "part_time_boys_year_group_8", "part_time_boys_year_group_9", "part_time_boys_year_group_10", 
  "part_time_boys_year_group_11", "part_time_boys_year_group_12", "part_time_boys_year_group_13", "part_time_boys_year_group_14", 
  "full_time_boys_nursery_1", "full_time_boys_nursery_2", "full_time_boys_reception", 
  "full_time_boys_year_group_1", "full_time_boys_year_group_2", "full_time_boys_year_group_3", "full_time_boys_year_group_4", 
  "full_time_boys_year_group_5", "full_time_boys_year_group_6", "full_time_boys_year_group_7", "full_time_boys_year_group_8", 
  "full_time_boys_year_group_9", "full_time_boys_year_group_10", "full_time_boys_year_group_11", "full_time_boys_year_group_12", 
  "full_time_boys_year_group_13", "full_time_boys_year_group_14")


    #### 2.4.3. combining the descriptor columns and the NC year columns. Then putting these vectors into a list, so that they can be an input into mapply
all_cols_1112 <- c(desc_cols_1112, nc_year_cols)
all_cols_1213 <- c(desc_cols_1213, nc_year_cols)

all_cols_1314 <- c(desc_cols_1314, nc_year_cols)
all_cols_1415 <- c(desc_cols_1415, nc_year_cols)


list_all_cols <- list(all_cols_1112, all_cols_1213, all_cols_1314, all_cols_1415)


    #### 2.4.4. defining the function to select the columns we need, applying this simple function over each element in the pupil data list (i.e. each year from 2011 to 2014)
select_cols <- function(data, cols){
  
  data <- data[,..cols]
  
  return(data)
  
}

pupil_data <- mapply(
  select_cols,
  data = pupil_data,
  cols =  list_all_cols,
  SIMPLIFY = FALSE)


  ### 2.5. pivoting to long with respect to the time/sex/year columns
to_long <- function(data){
  
  data <- melt(data = data, measure.vars = nc_year_cols)
  
  return(data)
  
}

pupil_data <- lapply(
  X = pupil_data,
  FUN = to_long
)


  ### 2.6. creating the nc year group column (just created by getting rid of part-time/full-time and boys/girls info - leaves only nc year info)
create_nc_year <- function(data){
  
  new_data <- data # TO REVISIT. Why did I do this? There may be a good reason, so come back to this later. 
  
  new_data[, nc_year := variable]
  new_data[, nc_year := gsub("part_time_", "", nc_year)]
  new_data[, nc_year := gsub("full_time_", "", nc_year)]
  new_data[, nc_year := gsub("boys_", "", nc_year)]
  new_data[, nc_year := gsub("girls_", "", nc_year)]
  
  return(new_data)
  
}

pupil_data <- lapply(
  X = pupil_data,
  FUN = create_nc_year
)


  ### 2.7. creating the full-time/part-time variable
create_full_part <- function(data){
  
  new_data <- data
  
  new_data[, full_or_part := character()] # creating empty character variable
  
  new_data[grep("part", variable), # filtering for part-time
           full_or_part := "part_time"] # populating the new variable with part-time
  
  new_data[grep("full", variable), # filtering for full-time
           full_or_part := "full_time"] # populating the new variable with full-time
  
  return(new_data)
  
}

pupil_data <- lapply(
  X = pupil_data,
  FUN = create_full_part
)


  ### 2.8. removing "variable", which was the variable containing all of the time/sex/nc year information
remove_variable <- function(data){ 
  
  data <- data[, -"variable"] 
  
  return(data) 
  
}

pupil_data <- lapply( 
  X = pupil_data, 
  FUN = remove_variable 
) 

  ### 2.9. keep school types - we're keeping all state-funded schools
  ### note - decided to leave out general hospital schools. There are so few of them, and just like independent schools, they don't provide data at nc year level

    #### 2.9.1. pre-2015/16 data
school_keep_1112 <- c("Maintained Primary", "Pupil Referral Unit", "Maintained Secondary",
                      "City Technology College", "Sponsor-Led Academy Secondary", "Converter Academy Secondary", 
                      "Converter Academy Primary", "Secondary Academy Free School", "Primary Academy Free School", 
                      "Sponsor-Led Academy Primary")

school_keep_1213 <- c("maintained primary", "pupil referral unit", "Maintained primary", "maintained secondary",
                      "City Technology College", "Secondary sponsored",
                      "Secondary converter", "primary converter", "secondary free school", "primary free school",
                      "primary sponsored", "UTC", "Studio school", 
                      "Free Schools - Alternative Provision", "Academy AP Converter")

school_keep_1314 <- c("State-funded primary", "State-funded secondary", 
                      "Pupil referral unit")

school_keep_1415 <- c("State-funded primary", "State-funded secondary", 
                      "Pupil referral unit")


list_school_keep <- list(school_keep_1112, school_keep_1213, 
                         school_keep_1314, school_keep_1415) # putting them into a list, in preparation for use in mapply (mapply needs inputs to be in lists)

list_school_var_names <- list("school_type", "school_type", 
                              "phase-type_grouping", "phase-type_grouping")


keep_schools <- function(data, schools_keep, school_var_name){
  
  new_data <- data
  
  new_data <- new_data[get(school_var_name) %in% schools_keep,]
  
  return(new_data)
  
}

pupil_data <- mapply(
  keep_schools, 
  data = pupil_data,
  schools_keep = list_school_keep,
  school_var_name = list_school_var_names,
  SIMPLIFY = FALSE
)


    #### 2.9.2. post 2015/2016 data
processed_pupil_data <- keep_schools(data = processed_pupil_data, 
                                     schools_keep = c("State-funded AP school", "State-funded primary", "State-funded secondary"), 
                                     school_var_name = "phase_type_grouping")


  ### 2.10. replacing the suppressed values with 1 (which was the average of the range of possible numbers that were suppressed, although do REVISIT THIS), and converting value from the character vector to a numeric vector
replace_suppressed_convert_numeric <- function(data){
  
  new_data <- data # TO REVISIT - again, why did I do it like this? 
  
  new_data <- new_data[value == "x", value := "1"]
  new_data <- new_data[, value := as.numeric(value)]
  
  return(new_data)
  
}

pupil_data <- lapply(
  X = pupil_data,
  FUN = replace_suppressed_convert_numeric
)



### 2.11. aggregate over sex and school type (as in, getting rid of these columns and summing)

    #### 2.11.1. pre-2015/16 data
agg_sex_school <- function(data){
  
  new_data <- data
  
  new_data <- new_data[, .(value = sum(value)),
                   by = list(old_la_code, new_la_code, la_name, nc_year, full_or_part)
                   ]
  
  return(new_data)
}


pupil_data <- lapply(
  X = pupil_data,
  FUN = agg_sex_school
)


    #### 2.11.2. post-2015/16 data
processed_pupil_data <- processed_pupil_data[, .(full_time = sum(full_time), part_time = sum(part_time), headcount = sum(headcount), fte = sum(fte)),
                                             by = list(time_period, old_la_code, new_la_code, la_name, ncyear)]


  ### 2.12. pivot wider with respect to full-time/part time
pivot_wide_time <- function(data){
  
  data <- dcast(data, 
                old_la_code + new_la_code + la_name + nc_year ~ full_or_part, 
                value.var = "value")
  
  return(data)
  
}

pupil_data <- lapply(
  X = pupil_data,
  FUN = pivot_wide_time
)


  ### 2.13 calculate headcount and fte
calculate_headcount_fte <- function(data){
  
  data[, headcount := full_time + part_time]
  data[, fte := full_time + 0.5*part_time]
  
  return(data)
  
}

pupil_data <- lapply(
  X = pupil_data,
  FUN = calculate_headcount_fte
)


  ### 2.14. giving a timeperiod to each year of data
time_periods <- paste0("20", gsub("_ncy", "", names(pupil_data))) # taking the dataset names, removing "_ncy", and adding "20" as a prefix. This will create a time period variable in line with the nicely formatted 2015/2016 data

add_time_periods <- function(data, time_period_value){
  
  data[, time_period := time_period_value] # assigning the new variable equal to the time period
  
  n <- ncol(data)
  cols_rearrange <- c(n, 1:(n - 1))
  
  data <- data[, ..cols_rearrange] # rearranges the columns so that the newly created column appears as the first column rather than the last
  
}

pupil_data <- mapply(
  FUN = add_time_periods,
  data = pupil_data,
  time_period_value = time_periods,
  SIMPLIFY = FALSE
)


  ### 2.15. amending the nc year variable in the processed (post 2015/16) data, to align
colnames(processed_pupil_data) <- gsub("ncyear", "nc_year", colnames(processed_pupil_data))

processed_pupil_data[, nc_year := tolower(nc_year)]
processed_pupil_data[, nc_year := gsub("year ", "year_group_", nc_year)]


  ### 2.16. getting them all into one list
pupil_data[[5]] <- processed_pupil_data

combined_data <- rbindlist(pupil_data)


  ### 2.17. removing nursery
combined_data <- 
  combined_data[!(nc_year %in% c("nursery_1", "nursery_2")),]


## 3. writing the dataset

fwrite(x = combined_data,
       file = "data/processed_data/pupil_numbers/pupil_numbers_1212_to_2223.csv") # TO FIX - naming needs to be automated



## decisions made during data processing

  ## - in 15/16 to 22/23, there is a variable on the type of schools included. In the other datasets, there is no such variable, and there also doesn't seem to be anything in the metadata telling us what's included. 
  ## - if there's no information on what has been included, it is probably fair to assume that everything has been included. 
  ## - a potentially troublesome category is non-maintained state schools. Are these included in the NC year group for the earlier years when it's aggregated to local authority? 
  ## - honestly, I think the answer is yes. As are, probably, independent schools. Because the datasets do contain data on these types of schools, and in the absence of any information on cutting these out when NC year numbers are grouped to LA, we have to assume they've been left in. 
  ## - and, the upshot of this is that if we use the aggregated LA data, we won't be able to cut out independent schools in the projections. And we want to be able to cut out independent schools in the projections, or at least disaggregate them out, because this output is for local authorities who care more about state-funded schools (of course).
  ## - meaning, finally, that we have to use school-level data and choose which types of schools we want. And then aggregate to local authority. 
  ## - what affect will suppression have on this? For tomorrow....

  ## agh - there is no school level for 2010/11. Maybe I'll just have to leave that year out...if all of the rest of them are there. 
  ## there was no data for state-funded special schools in 201314 or 201415. There were a few options, but I decided to take it out for all years. (might be worth making a rough estimation later for what the values might be)

  ## needed to use school-level data and aggregate up, rather than take LA data, because LA data didn't allow us to differentiate between state-funded and independent
  ## for suppressed data, it was assumed that the value of the cell was 1. Could have also picked 1.5 or 2, or alternated between 1 and 2....to revisit. 
  ## there was no data for state-funded special schools in 201314 or 201415. There were a few options, but we decided to take it out for all years. (might be worth making a rough estimation later for what the values might be)
  ## no school-level data for 2010/2011, so had to leave that one out




