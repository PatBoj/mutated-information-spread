library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

params <- 11 # number of saved parameters
dirName <- "26_06_600/" # directory where scripts looks for files
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

# Saves all realizations into histogram
for(i in 1:length(filesGroups)) {
  i <- 1
  # Just gets raw data from files
  rawData <- lapply(filesGroups[[i]], function(x) {
      data.frame(read.table(paste(dirName, x, sep=""), skip = params+2, sep = "\t", header = TRUE))
    })
  
  # Gets parameters from all files
  parameters <- lapply(filesGroups[[i]], function(x) {
      data.frame(read.table(paste(dirName, x, sep=""), nrows = params, sep = "\t", stringsAsFactors = FALSE))
    })
  
  # Change data type to dataframe
  rawData <- lapply(rawData, function(x) {
    as.data.frame(x)
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
  tempHist <- hist(log10(rawData[[1]]),
                   breaks = seq(-0.05, 6.45, 0.1),
                   plot = FALSE)
  
  # Computes average length of the messages
  length <- sapply(seq_along(tempHist$breaks[-1]), function(j) {
    if(j == 1) {
      condition <- log10(rawData$count) >= tempHist$breaks[j] & log10(rawData$count) <= tempHist$breaks[j+1]
    } else {
      condition <- log10(rawData$count) > tempHist$breaks[j] & log10(rawData$count) <= tempHist$breaks[j+1]
    }
    
    if(sum(condition) == 0) {
      c(0, 0)
    } else {
      tempData <- rawData[condition, ]
      avg <- sum(tempData$count * tempData$avg_length) / sum(tempData$count)
      sd <- sqrt(sum(tempData$count * (tempData$avg_length - avg)**2) / sum(tempData$count))
      c(avg, sd)
    }
  })
    
  factor <- 10^tempHist$breaks[-1] - 10^tempHist$breaks[-length(tempHist$breaks)] # wideness of every bin
  sum <- sum(tempHist$counts) # just a sum of log10 counts
  tempHist <- data.frame(x = tempHist$mids, y = tempHist$counts/factor/sum) # normalize data
  tempHist$lengthAVG <- length[1, ]
  tempHist$lengthSD <- length[2, ]
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
