library(dplyr)
library(tidyr)
library(WriteXLS)

# Import data files and store in a list
listOfFiles <- lapply(list.files(path="./Data/NK activation/PB_pre vs Tum/", pattern="*.csv", full.names = TRUE), function(x) read.csv2(x, sep=",", dec = "."))

# Validate files
columnCheck(listOfFiles)
rowCheck(listOfFiles)

namesOfFiles <- list.files(pattern = "*.csv")
varNames <- gsub(" ", "_", namesOfFiles)
varNames <- gsub("_PB_tum.csv", "", varNames)

# define function to convert from wide to long format
tidyData <- function(x){
  x %>% gather(Sample, value, -1)
}

listOfFilesLong <- lapply(listOfFiles, function(x) tidyData(x))
listOfFilesLong <- lapply(listOfFilesLong, function(x) {names(x)[1] <- "Patient"; x})
listOfFilesLong <- lapply(listOfFilesLong, function(x) {x[x[,2]=="PB", 2] <- "PB_pre"; x})
# assign the correct measurement measurement
for(i in 1:length(listOfFilesLong)){
  names(listOfFilesLong[[i]])[3] <- varNames[i]
}

fullDF <- Reduce(function(...) merge(..., all=TRUE), listOfFilesLong)
WriteXLS(fullDF, "allData.xls")




