
## quite a specific function, but useful here and might be useful elsewhere. 
## takes in a list full of ts_objects with the same years, and returns a data.table object with the years in the column heading

convert_tslist_to_dt <- function(input_ts_list){
  
  input_as_numeric_list <- lapply( # converting the input from list of ts objects to list of numeric vectors
    X = input_ts_list,
    FUN = function(input_ts){return(as.list(as.numeric(input_ts)))}
  )
  
  output_dt <- rbindlist(input_as_numeric_list) # converting this list of numeric vectors into a data.table
  
  years <- time(input_ts_list[[1]]) # extracting the years from the input list of time series - they should all be the same
  
  output_colnames <- paste0("year_", years) # creating column names from these years
  
  colnames(output_dt) <- output_colnames # renaming the columns in the output data.table
  
  return(output_dt)
  
}

