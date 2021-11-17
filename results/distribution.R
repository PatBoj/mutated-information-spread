library(dplyr)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

params <- 11 # number of saved parameters
dirName <- "30_10_2021_max/" # directory where scripts looks for files
fileNames <- list.files(dirName) # all filenames in the directory

realizations <- max(gsub("(^\\D+)([0-9]+)(.*)", "\\2", fileNames)) # number of independent realizations
trimmedFileNames <- gsub("(^\\D+)([0-9]+)(.*)", "\\1\\3", fileNames) # trimmed file names (without realization number)
trimmedFileNames <- unique(trimmedFileNames) # delete duplicates

# Groups every independent realization into one element in the list
filesGroups <- lapply(trimmedFileNames, function(x) {
  sapply(1:realizations, function(i) {
    gsub("(\\D+_)(_.*)", paste("\\1", i, "\\2", sep=""), x)
  })
})

choosenLength <- 1

# Saves all realizations into histogram
for(i in 1:length(filesGroups)) {
  i <- 1
  # Just gets raw data from files
  rawData <- lapply(filesGroups[[i]], function(x) {
    data.frame(read.table(paste(dirName, x, sep=""), skip = params+1, sep = "\t", header = TRUE, fill=TRUE))
  })
  
  # Gets parameters from all files
  parameters <- lapply(filesGroups[[i]], function(x) {
    data.frame(read.table(paste(dirName, x, sep=""), nrows = params, sep = "\t", stringsAsFactors = FALSE))
  })
  
  # Change data type to dataframe
  rawData <- lapply(rawData, function(x) {
    as.data.frame(x)
  })
  
  rawData <- lapply(seq_along(rawData), function(j) {
    temp <- as.data.frame(rawData[[j]][rawData[[j]]$avg_length == choosenLength, ][, 1])
  })
  
  rawData <- bind_rows(rawData) # binds every realization into one dataframe
  averageParameters <- parameters[[1]]
  
  degree <- sapply(parameters, function(x) {
    as.numeric(x[2,2])
  })
  
  sim <- sapply(parameters, function(x) {
    as.numeric(x[9,2])
  })
  
  averageParameters[2, 2] <- as.character(mean(degree))
  averageParameters[9, 2] <- as.character(mean(sim))
  
  averageParameters <- rbind(averageParameters, c("SD <k>", as.character(sd(degree)), "Standard deviation from average degree"))
  averageParameters <- rbind(averageParameters, c("SD of similarity", as.character(sd(sim)), "Standard deviation from average similarity of the agents"))
  
  # Creates histogram of those raw data, but already in log10 scale
  tempHist <- hist(rawData[, 1],
                   breaks = seq(-0.5, 601, 1),
                   plot = TRUE)
  
  factor <- 10^tempHist$breaks[-1] - 10^tempHist$breaks[-length(tempHist$breaks)] # wideness of every bin
  sum <- sum(tempHist$counts) # just a sum of log10 counts
  tempHist <- data.frame(x = tempHist$mids, y = tempHist$counts/factor/sum) # normalize data
  tempHist <- tempHist[tempHist$y != 0,] # delete all zeros
  tempHist$y <- log10(tempHist$y) # take the log10 from the height of the bins
  
  trimmed <- gsub("(\\D+)(__)(.*)(\\.txt)", "\\1_\\3_hist\\4", trimmedFileNames[i]) # add hist to new files
  newFile <- file(paste(gsub("/", "", dirName), "_hist/", trimmed, sep=""), open = "wt") # create new file
  
  apply(averageParameters, MARGIN=1, function(x) {
    writeLines(paste(x, collapse="\t"), sep="", newFile)
    writeLines("", newFile)
  })
  writeLines("", newFile)
  
  writeLines(paste(colnames(tempHist), collapse="\t"), sep="", newFile)
  writeLines("", newFile)
  
  apply(tempHist, MARGIN=1, function(x) {
    writeLines(paste(as.character(x), collapse="\t"), sep="", newFile)
    writeLines("", newFile)
  })
  
  close(newFile)
  
  print(paste(round(i/length(filesGroups) * 100, digits=2), "%", sep="", collapse=""))
}




