
## quite a specific function, but useful here and might be useful elsewhere. 
## takes in a list full of ts_objects with the same years, and returns a data.table object with the years in the column heading

convert_tslist_to_dt <- function(input_ts_list){
  
  input_as_numeric_list <- lapply(
    X = input_ts_list,
    FUN = function(input_ts){return(as.list(as.numeric(input_ts)))}
  )
  
  output_dt <- rbindlist(input_as_numeric_list)
  
  years <- time(input_ts_list[[1]])
  
  output_colnames <- paste0("year_", years)
  
  colnames(output_dt) <- output_colnames
  
  return(output_dt)
  
}

