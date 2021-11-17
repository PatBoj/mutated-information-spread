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

myGreen <- rgb(77/255, 175/255, 74/255)
myBlue <- rgb(55/255, 126/255, 184/255)
myRed <- rgb(228/255, 26/255, 28/255)

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
  rawData <- lapply(seq_along(rawData), function(i) {cbind(rawData[[i]], alpha = rep(parameters[[i]][7,2], nrow(rawData[[i]])))})
  rawData <- lapply(seq_along(rawData), function(i) {cbind(rawData[[i]], type = rep(parameters[[i]][8,2], nrow(rawData[[i]])))})
  rawData <- lapply(seq_along(rawData), function(i) {cbind(rawData[[i]], sim = rep(round(as.numeric(parameters[[i]][9,2]), digits=2), nrow(rawData[[i]])))})
  
  data <- bind_rows(rawData)
  data$network_type <- as.factor(data$network_type)
  data$tau <- as.factor(data$tau)
  data$alpha <- as.factor(data$alpha)
  data$type <- as.factor(data$type)
  data$sim <- as.factor(data$sim)
  return(data)
}

data <- getData("30_10_2021_hist/")
data <- rbind(data, getData("30_10_2021_other_hist/"))
data <- data[data$sim == 0, -9]
data <- data[data$tau == "-0.4" | data$tau == "0.2" | data$tau == "0.8", ]
data$tau <- droplevels(data$tau)
data$alpha <- factor(data$alpha, levels = c("0.2", "0.0"))

data$bin_x <- data$x
data$bin_y <- data$length
data$bin_y_SD <- data$lengthSD

data$point_x <- data$x
data$point_y <- data$length
data[data$alpha == "0.2", 12] <- data[data$alpha == "0.2", 12] - 0.075
data[data$alpha == "0.0", 12] <- data[data$alpha == "0.0", 12] + 0.075

myLabels <- list("$\\alpha = 0.2$", "$\\alpha = 0.0$")
facetLabels <- c("$\\tau = -0.4$", "$\\tau = 0.2$", "$\\tau = 0.8$")
names(facetLabels) <- c("-0.4", "0.2", "0.8")

popularityHistogram <- function(histogram) {
  plot <- ggplot(data = histogram) + 
    geom_hline(yintercept = seq(0, 13, 1), color = "gray", size = 0.05) +
    #geom_bar(aes(x = bin_x, y = bin_y/2, fill = factor(eta, levels = rev(levels(eta)))),
    #         stat = "identity", 
    #         position = position_dodge2(preserve = "single"),
    #         width = 0.09) +
    geom_errorbar(aes(x = bin_x, ymin = (bin_y - lengthSD)/2, ymax = (bin_y + lengthSD)/2, y = bin_y/2, color = factor(alpha, levels = rev(levels(alpha)))),
                  stat = "identity", 
                  position = position_dodge2(preserve = "single"), size = 0.35) +
    geom_point(aes(x = bin_x, y = bin_y/2, color = factor(alpha, levels = rev(levels(alpha)))),
               position=position_dodge(width=0.09),
               size = 0.4) +
    geom_point(aes(x = x, y = y + 12, shape = alpha, color = factor(alpha, levels = rev(levels(alpha)))), size = 1, stroke = 0.6) +
    theme(
      text=element_text(size=8),
      axis.text=element_text(color= "black"),
      axis.title.y.right = element_text(vjust = 2, hjust = 0.82),
      panel.border=element_rect(fill = NA),
      panel.background=element_blank(),
      legend.key=element_rect(fill = NA, color = NA),
      legend.background=element_rect(fill = (alpha("white", 0))),
      legend.title=element_blank(),
      legend.position="none",
      legend.box.background=element_rect(colour = "black"),
      legend.spacing.y = unit(0, "mm"),
      legend.text=element_text(size=8),
      #strip.text.x = element_text(size=8),
      #strip.background = element_rect(color="black", fill="lightgrey"),
      strip.background = element_blank(),
      strip.text.x = element_blank()
    ) + 
    xlab("number of shares") +
    ylab("probability density") +
    scale_x_continuous(limits = c(-.05, 3),
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
    facet_wrap(~ tau,
               labeller = labeller(tau = facetLabels))
  
  return(plot)
}

# 5.44
tikz(file = "plots/Results_plot_ER-no-competition.tex", width = inch(15), height = inch(5.44))
popularityHistogram(data[data$network_type == "ER" & data$type == "with_competition", ])
dev.off()

legend <- function() {
  tempData <- data.frame(x = c(1, 2, 3, 4),
                         y = c(1, 2, 3, 4),
                         shape = c("cross", "cross", "dot", "dot"),
                         color = c("red", "green", "red", "green"))
  
  plot <- ggplot(data = tempData) + 
    geom_point(aes(x = x, y = y, shape = shape), size = 1.8, stroke = 0.7) +
    geom_bar(aes(x = x, y = y, fill = color), stat="identity") +
    theme(
      text=element_text(size=8),
      axis.text=element_text(color= "black"),
      axis.title.y.right = element_text(vjust = 2, hjust = 0.82),
      panel.border=element_rect(fill = NA),
      panel.background=element_blank(),
      legend.key=element_rect(fill = NA, color = NA),
      legend.background=element_rect(fill = (alpha("white", 0))),
      legend.title=element_blank(),
      legend.position="top",
      legend.box.background=element_rect(colour = "black"),
      legend.spacing.y = unit(0, "mm"),
      legend.text=element_text(size=8),
      strip.text.x = element_text(size=8),
      strip.background = element_rect(color="black", fill="lightgrey")
    ) +
    scale_shape_manual(values = c(8, 16), labels=c("probability density", "average message length")) +
    scale_fill_manual(values = c(myRed, myGreen), labels=myLabels)
  legend <- cowplot::get_legend(plot)
  grid.newpage()
  grid.draw(legend)
}

tikz(file = "plots/Results_image_legend.tex", width = inch(14), height = inch(2))
legend()
dev.off()

#filtered <- data[data$network_type == "ER" & data$tau == "0.8" & data$type == "with_competition", ]

#png("plots/plot_test.png", units = "cm", width = 15, height = 6, res = 300)
#popularityHistogram(data[data$network_type == "ER" & data$type == "with_competition", ])
#dev.off()


