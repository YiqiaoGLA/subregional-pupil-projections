## function to extract data for a cohort as it progresses though school. 
## the pupil_dataset needs to be in a particular structure, with some particular column names hardcoded too. 

extract_cohort_data <- function(start_cohort_year, start_cohort_nc_year, end_cohort_nc_year, geog, pupil_data, max_year = 2032){
  
  ### getting the sequence of nc years that we need to project for
  nc_year_vec <- c("reception", "year_group_1", "year_group_2", "year_group_3", "year_group_4", "year_group_5", "year_group_6", "year_group_7", 
                   "year_group_8", "year_group_9", "year_group_10", "year_group_11", "year_group_12", "year_group_13", "year_group_14")
  
  nc_year_inds <- (which(nc_year_vec == start_cohort_nc_year)):(which(nc_year_vec == end_cohort_nc_year))
  
  nc_year_seq <- nc_year_vec[nc_year_inds]
  
  ### getting the sequence of calendar years that we need to project for
  year_seq <- (start_cohort_year):(start_cohort_year + length(nc_year_seq) - 1)
  
  ### getting rid of any years included in the sequence above that go beyond the maximum year in the dataset
  year_seq <- year_seq[year_seq <= max_year]
  
  nc_year_seq <- nc_year_seq[1:length(year_seq)]
  
  ### extracting the data for the calendar year:nc year combinations
  cohort_tracking_ind <- numeric(length(nc_year_seq))
  
  plot_data <- data.table(
    year = year_seq,
    nc_year = nc_year_seq,
    pupil_number = numeric(length(year_seq))
  )
  
  for(i in 1:nrow(plot_data)){
    
    ind <- which(pupil_data$year == year_seq[i] & pupil_data$nc_year == nc_year_seq[i] & pupil_data$itl22cd == geog)
    
    plot_data[i, "pupil_number"] <- pupil_data[ind, "pupil_number"]
    
    
  }
  
  return(plot_data)
  
  
}

