library(dplyr)
library(tidyr)
library(WriteXLS)

path <- "./Data/"
listOfFiles <- lapply(list.files(path=path, pattern="*.csv", full.names = TRUE, recursive = TRUE), function(x) read.csv2(x, sep=",", dec = "."))
# First change names cols, then wide to long, then merge RCC008 (check!) for PBs, then remove RCC010_f
