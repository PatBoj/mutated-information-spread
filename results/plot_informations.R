library(ggplot2)
library(dplyr)
library(scales)
library(shiny)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
  
  data <- bind_rows(rawData)
  data$network_type <- as.factor(data$network_type)
  data$tau <- as.factor(data$tau)
  data$eta <- as.factor(data$eta)
  data$type <- as.factor(data$type)
  data$sim <- as.factor(data$sim)
  return(data)
}

data <- getData("26_06_600_hist/")
#data <- rbind(data, getData("26_05_2021_other_hist/"))

data$bin_x <- data$x
data$bin_y <- data$length

popularityHistogram <- function(histogram) {
  plot <- ggplot(data = histogram, aes(x = x, y = y, color = network_type, shape = eta, fill = eta, size = length)) + 
    geom_point() +
    geom_bar(aes(x = bin_x, y = bin_y), stat = "identity") +
    theme(
      text=element_text(size = 28),
      axis.text=element_text(color= "black"),
      axis.ticks.length = unit(0, "cm"),
      plot.title=element_text(hjust = 0.5),
      panel.border=element_rect(fill = NA),
      panel.background=element_blank(),
      legend.key=element_rect(fill = NA, color = NA),
      legend.background=element_rect(fill = (alpha("white", 0))),
      legend.title=element_blank(),
      legend.position=c(0.6, 0.8),
      legend.box.background=element_rect(colour = "black"),
      legend.spacing.y = unit(0, "mm")
    ) + 
    xlab("number of shares") +
    ylab("probability density") +
    scale_x_continuous(limits = c(0, 3),
                       breaks = seq(0, 5)) + 
    scale_y_continuous(limits = c(-9, 1.7),
                       breaks = seq(-9, 1)) +
    annotation_logticks() + 
    scale_shape_manual(values = c(21, 22)) +
    scale_color_manual(values = c("red", "green")) + 
      scale_size_binned(limits=c(1, 16),
                        breaks = seq(2, 16, 2))
    
  plot
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("types", "Network types:",
                         c("Random" = "ER",
                           "Non-scale" = "BA")),
      
      checkboxGroupInput("edit", "Ability to edit:",
                         c("Yes" = "0.05",
                           "No" = "0.0")),
      checkboxGroupInput("competition", "With competition",
                         c("Yes" = "with_competition",
                           "No" = "without_competition")),
      sliderInput("tau", "Threshold", min = -1, max = 1, step = 0.04, value = 0.0),
      sliderInput("sim", "Similarity", min = 0, max = 0.6, step = 0.2, value = 0.0)
    ),
    
    mainPanel(
      plotOutput("my_histogram", height = 900)
    )
  )
)

server <- function(input, output) {
  filtered <- reactive({
    data %>%
      filter(network_type %in% input$types,
             tau == format(as.numeric(input$tau), nsmall = 1),
             eta %in% input$edit,
             sim %in% input$sim,
             type %in% input$competition)
  })
  
  output$my_histogram <- renderPlot({
    popularityHistogram(filtered())
  })
}

shinyApp(ui = ui, server = server)
