
## very simple function to convert all characters in the column names of a dataset to lower
## the main reason that this is a function at all is that I'm using an lapply call with this as the fuction, to be applied over a list of datasets. 
## between defining the function inside the data processing script or giving it its own file, I decided to give it its own file

colnames_lower <- function(data){
  
  colnames(data) <- tolower(colnames(data))
  
  return(data)
  
}

