# Do all files in a list of files have the same number of columns?
columnCheck <- function(x){
  col <- sapply(x, function(y) dim(y)[2])
  if(all.equal(min(col), max(col))){
    print("All files have the same number of columns")
  } else 
    {
  print("Not all files have the same number of columns")
  }
}

# Do all files in a list of files have the same number of rows?
rowCheck <- function(x){
  col <- sapply(x, function(y) dim(y)[1])
  if(all.equal(min(col), max(col))){
    print("All files have the same number of rows")
  } else 
  {
    print("Not all files have the same number of rows")
  }
}