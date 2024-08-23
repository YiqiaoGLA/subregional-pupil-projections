## function to...
## eugh! Very specific function. Assumes particular names of columns in the input dataset. Fix this. Eugh. 

extract_ratios <- function(start_cohort_year, start_cohort_nc_year, end_cohort_nc_year, geog, ratio_dataset, max_year){
  
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
  
  ### extracting the ratios for the calendar year:nc year combinations
  cohort_tracking_ind <- numeric(length(nc_year_seq))
  
  for(i in 1:length(cohort_tracking_ind)){
    
    ind <- which(ratio_dataset$year == year_seq[i] & ratio_dataset$nc_year == nc_year_seq[i] & ratio_dataset$itl22cd == geog)
    
    cohort_tracking_ind[i] <- ind
    
  }
  
  cohort_ratios <- ratio_dataset[cohort_tracking_ind, projected_proportion_continuing]
  
  ### returning the outputs
  return(cohort_ratios)
  
}
