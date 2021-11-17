library(ggplot2)
library(dplyr)
library(tidyverse)
library(tikzDevice)
library(scales)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(tz="CA")
pdf.options(encoding='ISOLatin2')

myGreen <- rgb(77/255, 175/255, 74/255)
myBlue <- rgb(55/255, 126/255, 184/255)
myRed <- rgb(228/255, 26/255, 28/255)

inch <- function(cm) {
  return(0.393701 * cm)
}

getData <- function(dirName) {
  params <- 13 # number of parameters
  fileNames <- list.files(dirName)
  
  rawData <- lapply(fileNames, function(x) {data.frame(read.table(paste(dirName, x, sep=""), skip = params, sep = "\t", header = TRUE, col.names = c("x", "y")))})
  parameters <- lapply(fileNames, function(x) {data.frame(read.table(paste(dirName, x, sep=""), nrows = params, sep = "\t", stringsAsFactors = FALSE))})
  
  rawData <- lapply(seq_along(rawData), function(i) {cbind(rawData[[i]], network_type = rep(parameters[[i]][3,2], nrow(rawData[[i]])))})
  rawData <- lapply(seq_along(rawData), function(i) {cbind(rawData[[i]], tau = rep(parameters[[i]][6,2], nrow(rawData[[i]])))})
  rawData <- lapply(seq_along(rawData), function(i) {cbind(rawData[[i]], eta = rep(parameters[[i]][5,2], nrow(rawData[[i]])))})
  rawData <- lapply(seq_along(rawData), function(i) {cbind(rawData[[i]], type = rep(parameters[[i]][8,2], nrow(rawData[[i]])))})
  rawData <- lapply(seq_along(rawData), function(i) {cbind(rawData[[i]], sim = rep(round(as.numeric(parameters[[i]][9,2]), digits=2), nrow(rawData[[i]])))})
  
  data <- bind_rows(rawData)
  data$network_type <- as.factor(data$network_type)
  data$tau <- as.factor(data$tau)
  data$eta <- as.factor(data$eta)
  data$type <- as.factor(data$type)
  data$sim <- as.factor(data$sim)
  return(data)
}

data <- getData("06_10_2021_variants_hist/")
data <- data[data$sim == 0, -7]
data$tau <- droplevels(data$tau)

facetLabels <- c("$\\tau = -0.4$", "$\\tau = 0.2$", "$\\tau = 0.8$")
names(facetLabels) <- c("-0.4", "0.2", "0.8")
myLabels <- list("$\\tau = -0.4$", "$\\tau = 0.2$", "$\\tau = 0.8$")

popularityHistogram <- function(histogram) {
  plot <- ggplot(data = histogram) + 
    #geom_hline(yintercept = seq(0, 13, 1), color = "gray", size = 0.05) +
    geom_point(aes(x = x, y = y, color = factor(tau)), size = 0.65, stroke = 0.6, shape = 1) +
    theme(
      text=element_text(size=8),
      axis.text=element_text(color= "black"),
      axis.title.y.right = element_text(vjust = 2, hjust = 0.82),
      panel.border=element_rect(fill = NA),
      panel.background=element_blank(),
      legend.key=element_rect(fill = NA, color = NA),
      legend.background=element_rect(fill = (alpha("white", 0))),
      legend.title=element_blank(),
      legend.position=c(0.8, 0.8),
      legend.box.background=element_rect(colour = "black"),
      legend.spacing.y = unit(0, "mm"),
      legend.text=element_text(size=8),
      strip.text.x = element_text(size=8),
      strip.background = element_rect(color="black", fill="lightgrey"),
      #strip.background = element_blank(),
      #strip.text.x = element_blank()
    ) + 
    scale_color_manual(values = c(myGreen, myRed, myBlue), labels=myLabels) +
    xlab("number of shares") +
    ylab("probability density") +
    scale_x_continuous(limits = c(0, 3.5),
                       breaks = seq(0, 5, 1),
                       labels = math_format(10^.x)) + 
    scale_y_continuous(limits = c(-6.3, 0.6),
                       breaks = seq(-7, 1, 1),
                       labels = math_format(10^.x))
  
  return(plot)
}

tikz(file = "plots/Results_plot_ER-variants.tex", width = inch(9), height = inch(9))
popularityHistogram(data[data$network_type == "ER", ])
dev.off()
