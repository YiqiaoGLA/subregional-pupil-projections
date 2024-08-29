## this is a specific function, designed to solely to work with an intermediate dataset in this project. 
## this intermediate dataset is the projected carryover ratios from one nc year to the next year. We need to extract the right series of these ratios to project a cohort through the nc years. 
## for example, if we have the number of nc year 1 pupils in 2021, we need the projected carryover ratio from 2021 to 2022 of year 1 to year 2, then the projected carryover ratio from 2022 to 2023 for year 2 to year 3, the 2023 to 2024 ratio for year 3 to year 4, etc
## this function is designed to extract those ratios. 
## but even further, this function extracts the ratios when the input is a dataset with multiple (usually 1,000) bootstrapped versions of the ratio. 
## very specific function, assumes particular column names and structure of inputs. For now I can't really see a way around it. But would be good to fix this for later. 

extract_ratios_bootstrapped <- function(start_cohort_year = start_cohort_year, 
                                        start_cohort_nc_year = start_cohort_nc_year, end_cohort_nc_year = "year_group_11", 
                                        geog = geog, ratio_dataset_bootstrapped = ratio_projection_bootstrapped, max_year = 2032){
  
  ### getting the sequence of nc years that we need to project for
  nc_year_vec <- c("year_group_1", "year_group_2", "year_group_3", "year_group_4", "year_group_5", "year_group_6", "year_group_7", 
                   "year_group_8", "year_group_9", "year_group_10", "year_group_11", "year_group_12", "year_group_13", "year_group_14")
  
  nc_year_inds <- (which(nc_year_vec == start_cohort_nc_year) + 1):(which(nc_year_vec == end_cohort_nc_year))
  
  nc_year_seq <- nc_year_vec[nc_year_inds]
  
  ### getting the sequence of calendar years that we need to project for
  year_seq <- (start_cohort_year + 1):(start_cohort_year + length(nc_year_seq))
  
  ### getting rid of any years included in the sequence above that go beyond the maximum year in the dataset
  year_seq <- year_seq[year_seq <= max_year]
  
  nc_year_seq <- nc_year_seq[1:length(year_seq)]
  
  ### creating column names for the data.table
  colnames_dt <- paste(nc_year_seq, year_seq, sep = "_")
  
  ### extracting the bootstrapped ratios for the calendar year:nc year combinations (a bit crude and ugly but works fine)
  bootstrapped_tracking_ratios <- data.table() # defining an empty data.table. Ratios will be added as columns to this data.table
  
  ### looping through each of the nc years and extracting the corresponding ratio
  for(z in 1:length(nc_year_seq)){
    
    nc_year_sel <- nc_year_seq[z] # narrowing down to an nc year
    year_sel <- year_seq[z] # narrowing down to the corresponding calendar year
    
    nc_year_proj <- ratio_dataset_bootstrapped[[nc_year_sel]][[geog]] # this line is very specific to the input dataset. Indexes in such as way as to extract the correct nc year ratio projections for the correct geography in question
    
    ind <- grep(year_sel, colnames(nc_year_proj)) # this line, along with the line below it, extracts the ratios for the correct calendar year of the nc year ratio projections
    
    col_to_add <- nc_year_proj[, ..ind]
    
    bootstrapped_tracking_ratios[, new_col := col_to_add] ## adding a new column to the data.table with the ratios
    colnames(bootstrapped_tracking_ratios)[length(colnames(bootstrapped_tracking_ratios))] <- colnames_dt[z] # naming the most recently added column 
    
  }
  
  ### returning the outputs
  return(bootstrapped_tracking_ratios)
  
}
