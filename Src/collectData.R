library(dplyr)
library(tidyr)
library(WriteXLS)

# 1st set of data
path <- "./Data/NK activation/PB_pre vs Tum/"
# Import data files and store in a list
listOfFiles <- lapply(list.files(path=path, pattern="*.csv", full.names = TRUE), function(x) read.csv2(x, sep=",", dec = "."))

# Validate files
columnCheck(listOfFiles)
rowCheck(listOfFiles)
nameCheck(listOfFiles)

# Rename
for(i in 1:length(listOfFiles)){
  names(listOfFiles[[i]]) <- c("X", "PB.pre", "Tumor")
}

# Clean up names and collecct to assign to the files in long format
namesOfFiles <- list.files(path=path, pattern = "*.csv")
namesOfFiles <- spaceToUnderscore(namesOfFiles)
namesOfFiles <- removePattern(namesOfFiles, pattern="_PB_tum.csv")

# define function to convert from wide to long format
listOfFilesLong <- lapply(listOfFiles, tidyData)

# assign the correct measurement measurement
for(i in 1:length(listOfFilesLong)){
  names(listOfFilesLong[[i]]) <- c("Patient", "Sample", namesOfFiles[i])
}

df_pre_tum <- cbind(listOfFilesLong[[1]][c(1,2)], do.call(cbind.data.frame, lapply(listOfFilesLong, function(x) x[3])))

# 2nd set of data
path <- "./Data/NK activation/PB_pre vs PB_post/"
# Import data files and store in a list
listOfFiles <- lapply(list.files(path=path, pattern="*.csv", full.names = TRUE), function(x) read.csv2(x, sep=",", dec = "."))

# Validate files
columnCheck(listOfFiles)
rowCheck(listOfFiles)
nameCheck(listOfFiles)

# Rename
# assign the correct measurement measurement
for(i in 1:length(listOfFiles)){
  names(listOfFiles[[i]]) <- c("X", "PB.pre", "PB.post")
}

# Clean up names and collecct to assign to the files in long format
namesOfFiles <- list.files(path=path, pattern = "*.csv")
namesOfFiles <- spaceToUnderscore(namesOfFiles)
namesOfFiles <- removePattern(namesOfFiles, pattern="_PB_pre_post.csv")
namesOfFiles <- removePattern(namesOfFiles, pattern="_pre_post.csv")

# define function to convert from wide to long format
listOfFilesLong <- lapply(listOfFiles, tidyData)

# assign the correct measurement measurement
for(i in 1:length(listOfFilesLong)){
  names(listOfFilesLong[[i]]) <- c("Patient", "Sample", namesOfFiles[i])
}

df_pre_post <- cbind(listOfFilesLong[[1]][c(1,2)], do.call(cbind.data.frame, lapply(listOfFilesLong, function(x) x[3])))

# 3d set of data
path <- "./Data/NK activation/Tumor/"
# Import data files and store in a list
listOfFiles <- lapply(list.files(path=path, pattern="*.csv", full.names = TRUE), function(x) read.csv2(x, sep=",", dec = "."))

# Validate files
columnCheck(listOfFiles)
rowCheck(listOfFiles)
nameCheck(listOfFiles)

# assign the correct measurement measurement
for(i in 1:length(listOfFiles)){
  names(listOfFiles[[i]])[c(1,2)] <- c("Patient", "Sample")
}

df_tum <- cbind(listOfFiles[[1]][c(1,2)], do.call(cbind.data.frame, lapply(listOfFiles, function(x) x[c(3,4)])))
