
## Function that takes in two time series and returns the product of those time series
## the two input time series don't have to be fully aligning - the function will match them based on the years they have in common. The years included could be 2012 to 2017 in one series and 2014 to 2020 in another, and this function will only multiply 2014 to 2017
## however, the years must be in order from earliest to latest

get_ts_product <- function(time_series_1, time_series_2){
  
  ### converting the input ts objects to data.tables, as they're easier to manipulate
  ts_1_dt <- data.table(
    year = time(time_series_1),
    series_1 = as.numeric(time_series_1)
  )
  
  ts_2_dt <- data.table(
    year = time(time_series_2),
    series_2 = as.numeric(time_series_2)
  )
  
  ### narrowing down both datasets to only the years they have in common
  years_in_common <- intersect(ts_1_dt[, year], ts_2_dt[, year])
  
  ts_1_dt <- ts_1_dt[year %in% years_in_common,]
  ts_2_dt <- ts_2_dt[year %in% years_in_common,]
  
  ### joining them
  product_ts_dt <- ts_1_dt[ts_2_dt, on = "year"]
  
  ### getting the product
  product_ts_dt[, product_ts := series_1*series_2]
  
  ### saving and returning the final ts object
  product_ts <- ts(data = product_ts_dt[, product_ts], 
                   start = product_ts_dt[1, year], # this line DOES assume that the years are in order
                   frequency = 1)
  
  return(product_ts)
  
}