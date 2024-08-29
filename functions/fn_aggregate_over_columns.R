## simple function, to remove selected columns from a dataset and aggregate the numeric variables

aggregate_over_columns <- function(data, columns_to_remove, columns_to_sum){
  
  # converting input data object into a data.table
  data <- data.table(data) # just in case...might save some time
  
  # creating a vector of the columns we want to aggregate over (which is all columns, minus the ones we've specified that we want to remove and the count columns (because we want to sum those))
  cols_to_rem <- c(columns_to_remove, columns_to_sum)
  
  columns_to_aggregate_by <- colnames(data)[!(colnames(data) %in% cols_to_rem)]
  
  # the final aggregation
  data <- data[,lapply(.SD, sum, na.rm = TRUE), # applying sum function to each column specified in SD cols
               .SDcols = c(columns_to_sum), # setting SDcols. In this case, only the count column(s)
               by = columns_to_aggregate_by] # the columns that we want to aggregate over/by. 
  
  return(data)
  
}

