# Do all files in a list of files have the same number of columns? If not, stop script.
columnCheck <- function(x){
  col <- sapply(x, function(y) dim(y)[2])
  if(identical(min(col), max(col))){
    print("All files have the same number of columns")
  } else 
    {
  stop("Not all files have the same number of columns")
  }
}

# Do all files in a list of files have the same number of rows?  If not, stop script.
rowCheck <- function(x){
  row <- sapply(x, function(y) dim(y)[1])
  if(identical(min(row), max(row))){
    print("All files have the same number of rows")
  } else 
  {
    stop("Not all files have the same number of rows")
  }
}
# Do all files in a list of files have the same names? Return names.
nameCheck <- function(x){
  tmp <- sapply(x, names)
  return(unique(as.character(tmp)))
}

# Replace spaces with underscores in list of strings
spaceToUnderscore <- function(x){
  x <- gsub(" ", "_", x)
  return(x)
}

# Remove pattern
removePattern <- function(x, pattern="_PBP_tum.csv"){
  x <- gsub(pattern, "", x)
  return(x)
}

# define function to convert from wide to long format
tidyData <- function(x){
 gather(x, Sample, value, -1)
}