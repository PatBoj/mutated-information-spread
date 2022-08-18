library(ggplot2)
library(dplyr)
library(scales)
library(latex2exp)
library(tikzDevice)
library(tibble)
library(purrr)
library(grid)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(tz="CA")
pdf.options(encoding='ISOLatin2')

myGreen <- rgb(34/255, 136/255, 51/255)
myBlue <- rgb(55/255, 126/255, 184/255)
myRed <- rgb(228/255, 102/255, 119/255)

inch <- function(cm) {
  return(0.393701 * cm)
}

getData <- function(dirName) {
  params <- 13 # number of parameters
  fileNames <- list.files(dirName)
  
  rawData <- lapply(fileNames, function(x) {data.frame(read.table(paste(dirName, x, sep=""), skip = params, sep = "\t", header = TRUE, col.names = c("x", "y", "length", "lengthSD")))})
  parameters <- lapply(fileNames, function(x) {data.frame(read.table(paste(dirName, x, sep=""), nrows = params, sep = "\t", stringsAsFactors = FALSE))})
  
  rawData <- lapply(seq_along(rawData), function(i) {cbind(rawData[[i]], network_type = rep(parameters[[i]][3,2], nrow(rawData[[i]])))})
  rawData <- lapply(seq_along(rawData), function(i) {cbind(rawData[[i]], tau = rep(parameters[[i]][6,2], nrow(rawData[[i]])))})
  rawData <- lapply(seq_along(rawData), function(i) {cbind(rawData[[i]], eta = rep(parameters[[i]][5,2], nrow(rawData[[i]])))})
  rawData <- lapply(seq_along(rawData), function(i) {cbind(rawData[[i]], type = rep(parameters[[i]][8,2], nrow(rawData[[i]])))})
  rawData <- lapply(seq_along(rawData), function(i) {cbind(rawData[[i]], sim = rep(round(as.numeric(parameters[[i]][9,2]), digits=2), nrow(rawData[[i]])))})
  rawData <- lapply(seq_along(rawData), function(i) {cbind(rawData[[i]], size = rep(round(as.numeric(parameters[[i]][1,2]), digits=2), nrow(rawData[[i]])))})
  
  data <- bind_rows(rawData)
  data$network_type <- as.factor(data$network_type)
  data$tau <- as.factor(data$tau)
  data$eta <- as.factor(data$eta)
  data$type <- as.factor(data$type)
  data$sim <- as.factor(data$sim)
  return(data)
}

data <- getData("26_06_loop_hist/")
data <- data[data$sim == 0, -9]
data <- data[data$tau == "-0.4" | data$tau == "0.2" | data$tau == "0.8", ]
data$tau <- droplevels(data$tau)
data$eta <- factor(data$eta, levels = c("0.2", "0.0"))

data$bin_x <- data$x
data$bin_y <- data$length
data$bin_y_SD <- data$lengthSD

data$point_x <- data$x
data$point_y <- data$length
data[data$eta == "0.2", 12] <- data[data$eta == "0.2", 12] - 0.075
data[data$eta == "0.0", 12] <- data[data$eta == "0.0", 12] + 0.075

myLabels <- list("$\\eta = 0.2$", "$\\eta = 0.0$")
facetLabels <- c("$\\tau = -0.4$", "$\\tau = 0.2$", "$\\tau = 0.8$")
names(facetLabels) <- c("-0.4", "0.2", "0.8")

labels_abc <- data.frame(size = c(600, 600, 600),
                         tau = c(-0.4, 0.2, 0.8),
                         labels = c("(a)", "(b)", "(c)"))

popularityHistogram <- function(histogram) {
  plot <- ggplot(data = histogram) + 
    geom_hline(yintercept = seq(0, 13, 1), color = "grey90", size = 0.05) +
    #geom_bar(aes(x = bin_x, y = bin_y/2, fill = factor(eta, levels = rev(levels(eta)))),
    #         stat = "identity", 
    #         position = position_dodge2(preserve = "single"),
    #         width = 0.09) +
    geom_errorbar(aes(x = bin_x, ymin = ifelse((bin_y - lengthSD)/2 <= 0, 0, (bin_y - lengthSD)/2), ymax = (bin_y + lengthSD)/2, y = bin_y/2, color = factor(eta, levels = rev(levels(eta)))),
                  stat = "identity", 
                  position = position_dodge2(preserve = "single"), size = 0.35) +
    geom_point(aes(x = bin_x, y = bin_y/2, color = factor(eta, levels = rev(levels(eta)))),
               position=position_dodge(width=0.09),
               size = 0.4) +
    geom_point(aes(x = x, y = y + 12, shape = eta, color = factor(eta, levels = rev(levels(eta)))), size = 1, stroke = 0.6) +
    theme(
      text=element_text(size=9),
      axis.text=element_text(color= "black", size=9),
      #axis.title.y.right = element_text(vjust = 2, hjust = 0.82),
      panel.border=element_rect(fill = NA),
      panel.background=element_blank(),
      legend.key=element_rect(fill = NA, color = NA),
      legend.background=element_rect(fill = (alpha("white", 0))),
      legend.title=element_blank(),
      legend.position="none",
      legend.box.background=element_rect(colour = "black"),
      legend.spacing.y = unit(0, "mm"),
      legend.text=element_text(size=8),
      strip.text.x = element_text(size=8),
      strip.background.x = element_rect(color="black", fill="lightgrey"),
      #strip.background = element_blank(),
      #strip.text.x = element_blank()
      strip.background.y = element_blank(),
      strip.text.y = element_blank()
    ) + 
    xlab("number of message shares") +
    ylab("probability density") +
    scale_x_continuous(limits = c(-.05, 5.5),
                       breaks = seq(0, 5),
                       labels = math_format(10^.x)) + 
    scale_y_continuous(limits = c(0, 13),
                       breaks = seq(0, 13, 2),
                       labels = math_format(10^.x) (seq(-12, 1, 2)),
                       #expand = c(0, 0),
                       sec.axis = sec_axis(trans=~.*2, 
                                           name = "message length",
                                           breaks = seq(0, 16, 4))) +
    scale_shape_manual(values = c(4, 3), labels=myLabels) +
    scale_color_manual(values = c(myGreen, myRed), labels=myLabels) +
    scale_fill_manual(values = c(myRed, myGreen), labels=myLabels) +
    facet_grid(size ~ tau,
               labeller = labeller(tau = facetLabels)) +
    geom_text(data = labels_abc, aes(x=Inf, y=Inf, label=labels), 
              hjust= 2.5, vjust= 2, size = 4)
  
  return(plot)
}

# 5.44
tikz(file = "plots/Results_plot_loop.tex", width = inch(17), height = inch(7))
popularityHistogram(data[data$network_type == "BA" & data$type == "with_competition", ])
dev.off()

