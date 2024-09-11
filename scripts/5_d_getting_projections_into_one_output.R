### The projections for Reception, Year 1, and Years 2-11 are created in three separate scripts
### This short script combines the three output files from those scripts into one output file, and adds several new columns to make the output more user-friendly

### 0. libraries, functions, inputs
library(data.table)

start_proj_year <- 2023 # this is the sort of thing I'll have to have running throughout the scripts to make it all automated

end_proj_year <- 2032


### 1. reading in the input data

reception_filepath <- paste0("output_projections/initial_tenyear/reception_projections_", start_proj_year, "_", end_proj_year, "_ratio_ets.csv")
reception <- fread(reception_filepath)

year_one_filepath <- paste0("output_projections/initial_tenyear/year_one_projections_", start_proj_year, "_", end_proj_year, "_ratio_ets.csv")
year_one <- fread(year_one_filepath)

rest_years_filepath <- paste0("output_projections/initial_tenyear/year2_year11_projections_", start_proj_year, "_", end_proj_year, ".csv")
rest_years <- fread(rest_years_filepath)


### 2. making the columns of each of the datasets align

col_ords <- c("itl22cd", "year", "nc_year", "mean_projection", "upper_pi", "lower_pi")

reception[, nc_year := "reception"]

year_one[, nc_year := "year_group_1"]

colnames(rest_years)[colnames(rest_years) == "pupil_number"] <- "mean_projection"

reception <- reception[, ..col_ords]
year_one <- year_one[, ..col_ords]
rest_years <- rest_years[, ..col_ords]

### 3. binding the datasets together

full_dataset <- rbind(reception, year_one, rest_years)


### 4. adding new variables

  #### 4.1. adding numbers for nc_years
nc_year_lookup <- data.table(
  nc_year = c("reception", "year_group_1", "year_group_2", "year_group_3", "year_group_4", "year_group_5", "year_group_6", "year_group_7", "year_group_8", "year_group_9", "year_group_10", "year_group_11"),
  nc_year_number = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
)

full_dataset <- nc_year_lookup[full_dataset, on = c("nc_year" = "nc_year")]

  #### 4.2. adding academic year
full_dataset[, acd_year := paste0(year, "/", year + 1)]


  #### 4.3. rearranging the columns
col_ords <- c("itl22cd", "year", "acd_year", "nc_year", "nc_year_number", "mean_projection", "upper_pi", "lower_pi")

full_dataset <- full_dataset[, ..col_ords]


### 5. saving the dataset

output_filepath <- paste0("output_projections/initial_tenyear/reception_year_11_projections_", start_proj_year, "_", end_proj_year, ".csv")

fwrite(x = full_dataset,
       file = output_filepath)



