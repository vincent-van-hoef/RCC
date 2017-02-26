# Check whether the files have the number of columns specified in the columns parameter
columnCheck <- function(x, columns = 3){
  dim(x)[2]==columns
}