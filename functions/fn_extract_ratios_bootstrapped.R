## function to....
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
  
  ### extracting the bootstrapped ratios for the calendar year:nc year combinations (eugh, this is so ugly....(1) heavily comment so that it's clear why I've done all of this, and (2) try to do it better when I've time!)
  bootstrapped_tracking_ratios <- data.table()
  
  z <- 1
  
  for(z in 1:length(nc_year_seq)){
    
    nc_year_sel <- nc_year_seq[z]
    year_sel <- year_seq[z]
    
    nc_year_proj <- ratio_dataset_bootstrapped[[nc_year_sel]][[geog]]
    
    ind <- grep(year_sel, colnames(nc_year_proj))
    
    col_to_add <- nc_year_proj[, ..ind]
    
    bootstrapped_tracking_ratios[, new_col := col_to_add]
    colnames(bootstrapped_tracking_ratios)[length(colnames(bootstrapped_tracking_ratios))] <- colnames_dt[z]
    
  }
  
  ### returning the outputs
  return(bootstrapped_tracking_ratios)
  
}
