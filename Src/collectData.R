library(dplyr)
library(tidyr)
library(WriteXLS)

path <- "./Data/"
listOfFiles <- lapply(list.files(path=path, pattern="*.csv", full.names = TRUE, recursive = TRUE), function(x) read.csv2(x, sep=",", dec = "."))
# First change names cols, then wide to long, then merge RCC008 (check!) for PBs, then remove RCC010_f
listOfFiles <- lapply(listOfFiles, function(x) {names(x)[1] <- "Patient"; x})
listOfFiles <- lapply(listOfFiles, function(x) {names(x)[names(x)=="X.1"] <- "Sample"; x})
# Collect names
fileNames <- list.files(path=path, pattern="*.csv", recursive=TRUE)
fileNames <- basename(fileNames)
fileNames <- gsub(".csv", "", fileNames)
# Assign names
names(listOfFiles) <- fileNames

for(i in names(listOfFiles)){
  if(!names(listOfFiles[[i]])[2] == "Sample"){
    listOfFiles[[i]] <- gather(listOfFiles[[i]], key="Patient")
    names(listOfFiles[[i]])[c(2,3)] <- c("Sample", names(listOfFiles[i]))
  }
}
# Correct names, first list all unique names
unique(as.vector(unlist(sapply(listOfFiles, function(x) x[,1]))))
listOfFiles <- lapply(listOfFiles, function(x) {x[,1] <- gsub("^RCC08 T1$", "RCC008_T1", x[,1]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,1] <- gsub("^RCC008 T2$", "RCC008_T2", x[,1]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,1] <- gsub("^RCC008 T1$", "RCC008_T1", x[,1]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,1] <- gsub("^RCC0008 T2$", "RCC008_T2", x[,1]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,1] <- gsub("^RCC0008 T1$", "RCC008_T1", x[,1]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,1] <- gsub("^RCC0001$", "RCC001", x[,1]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,1] <- gsub("^RCC0002$", "RCC002", x[,1]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,1] <- gsub("^RCC0003$", "RCC003", x[,1]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,1] <- gsub("^RCC0004$", "RCC004", x[,1]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,1] <- gsub("^RCC0005$", "RCC005", x[,1]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,1] <- gsub("^RCC0006$", "RCC006", x[,1]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,1] <- gsub("^RCC0007$", "RCC007", x[,1]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,1] <- gsub("^RCC0008$", "RCC008", x[,1]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,1] <- gsub("^RCC0009$", "RCC009_f", x[,1]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,1] <- gsub("^RCC009$", "RCC009_f", x[,1]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,1] <- gsub("^RCC0011$", "RCC011", x[,1]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,1] <- gsub("^RCC0012$", "RCC012", x[,1]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,1] <- gsub("^RCC0013$", "RCC013", x[,1]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,1] <- gsub("^RCC0014$", "RCC014", x[,1]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,1] <- gsub("^RCC0015$", "RCC015", x[,1]); x})

unique(as.vector(unlist(sapply(listOfFiles, function(x) x[,2]))))
listOfFiles <- lapply(listOfFiles, function(x) {x[,2] <- gsub("^PB$", "PB_pre", x[,2]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,2] <- gsub("^Pre.OP$", "PB_pre", x[,2]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,2] <- gsub("^PB.pre$", "PB_pre", x[,2]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,2] <- gsub("^Post.OP$", "PB_post", x[,2]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,2] <- gsub("^PB.post$", "PB_post", x[,2]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,2] <- gsub("^Tumor 1$", "Tumor", x[,2]); x})
listOfFiles <- lapply(listOfFiles, function(x) {x[,2] <- gsub("^Tumor 2$", "Tumor", x[,2]); x})

df <- Reduce(function(x, y) merge(x, y, all=TRUE), listOfFiles)
df <- df %>% arrange(desc(Sample))

toMerge8 <- intersect(grep("RCC008", df$Patient), grep("PB_pre", df$Sample))
toReplace <- df[toMerge8,-c(1:2)]
toReplace <- as.numeric(apply(toReplace, 2, function(x) sort(x)[1]))
df[toMerge8[1], -c(1:2)] <- toReplace
df <- df[-toMerge8[c(2,3)],]

toMerge8 <- intersect(grep("RCC008", df$Patient), grep("PB_post", df$Sample))
toReplace <- df[toMerge8,-c(1:2)]
toReplace <- as.numeric(apply(toReplace, 2, function(x) sort(x)[1]))
df[toMerge8[1], -c(1:2)] <- toReplace
df <- df[-toMerge8[c(2,3)],]
# remove duplicate cols
df <- df[,c(TRUE, TRUE, !duplicated(t(df[,-c(1,2)])))]

postInd <- c(TRUE, TRUE, !duplicated(t(subset(df[,-c(1,2)], df$Sample=="PB_post"))))
preInd <- c(TRUE, TRUE, !duplicated(t(subset(df[,-c(1,2)], df$Sample=="PB_pre"))))
tumorInd <- c(TRUE, TRUE, !duplicated(t(subset(df[,-c(1,2)], df$Sample=="Tumor"))))

df_pre <- subset(df[,preInd], df$Sample=="PB_pre")
df_post <- subset(df[,postInd], df$Sample=="PB_post")
df_tum <- subset(df[,tumorInd], df$Sample=="Tumor")


names(df_pre) <- gsub(" PB-Tum", "", names(df_pre))
names(df_pre) <- gsub(" PB tum", "", names(df_pre))
names(df_pre) <- gsub(" PB pre post", "", names(df_pre))
names(df_pre) <- gsub(" pre post", "", names(df_pre))

names(df_post) <- gsub(" PB-Tum", "", names(df_post))
names(df_post) <- gsub(" PB tum", "", names(df_post))
names(df_post) <- gsub(" PB pre post", "", names(df_post))
names(df_post) <- gsub(" pre post", "", names(df_post))

names(df_tum) <- gsub(" PB-Tum", "", names(df_tum))
names(df_tum) <- gsub(" PB tum", "", names(df_tum))
names(df_tum) <- gsub(" PB pre post", "", names(df_tum))
names(df_tum) <- gsub(" pre post", "", names(df_tum))

bySampList <- list(df_pre, df_post, df_tum)

df_filt <- Reduce(function(x, y) merge(x, y, all=TRUE), bySampList)
df_filt <- df_filt %>% arrange(desc(Sample))
WriteXLS(df_filt, "data.xlsx")
