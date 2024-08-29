## function to create projections by tracking a cohort from a starting year through to a final year, multiplying by a ratio for each transition from one year to the next
## this function is quite specific to this project. That can be ok, but a greater issue here is that the names of the nc year groups are hardcoded, meaning it can't accommodate a change in the name of the groups.
## which is less fine. So this function will eventually need to be improved. 

track_cohort <- function(start_cohort_size, start_cohort_year, start_cohort_nc_year, cohort_ratios, output_to_return = "DT"){
  
  ### getting number of years ahead to project. Determined by the input ratio vector
  n_to_project <- length(cohort_ratios)
  
  ### getting the vector of years that are to be projected. Determined by the start year, and number of years ahead as determined above
  year_seq <- (start_cohort_year + 1):(start_cohort_year + n_to_project)
  
  ### getting the vector of nc year group to be projected. There's a hardcoded vector of years here, and the vector will be extracted by indexing from the first year after the input year, to the end year as determined by number of years above
  nc_year_vec <- c("year_group_1", "year_group_2", "year_group_3", "year_group_4", "year_group_5", "year_group_6", "year_group_7", 
                   "year_group_8", "year_group_9", "year_group_10", "year_group_11", "year_group_12", "year_group_13", "year_group_14")
  
  nc_year_inds <- (which(nc_year_vec == start_cohort_nc_year) + 1):(which(nc_year_vec == start_cohort_nc_year) + n_to_project)
  
  nc_year_seq <- nc_year_vec[nc_year_inds]
  
  ### setting up the final output data.table
  projected_pupils <- data.table(
    year = year_seq,
    nc_year = nc_year_seq,
    pupil_number = numeric(n_to_project)
  )
  
  ### tracking the cohort through the years, by multiplying it by the ratios (this could do with more explanation)
  cohort_size <- start_cohort_size
  
  for(i in 1:length(cohort_ratios)){
    
    cohort_size <- cohort_size*cohort_ratios[i]
    
    projected_pupils[i, pupil_number := cohort_size]
    
  }
  
  ### returning the output
  
  if(output_to_return == "DT"){
    return(projected_pupils)
    
  }else if(output_to_return == "vector"){

    return(data.table(t(projected_pupils[, pupil_number])))
    
  }
  
  
}



