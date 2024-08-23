
## very simple function to replace spaces in the column names of a dataset with underscores
## the main reason that this is a function at all is that I'm using an lapply call with this as the fuction, to be applied over a list of datasets. 
## between defining the function inside the data processing script or giving it its own file, I decided to give it its own file

add_underscores <- function(data){
  
  colnames(data) <- gsub(" ", "_", colnames(data))
  
  return(data)
  
}
