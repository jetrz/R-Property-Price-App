library(readxl)
library(stringr)
library(tidyverse)
library(shiny)
library(ggplot2)
library(dplyr)

condodata <- read.csv("data/condo.csv")
hdbdata <- read.csv("data/hdb.csv")

ResaleMeanPriceByArea <- function(townSelect, storyRangeSelect) {
    filteredData <- hdbdata
    
    # Filter by selected towns, ignoring "All" if other towns are selected
    if (!"All" %in% townSelect || length(townSelect) > 1) {
    filteredData <- filteredData %>% filter(town %in% townSelect)
    }
    
    # Filter by selected story ranges, ignoring "All" if other ranges are selected
    if (!"All" %in% storyRangeSelect || length(storyRangeSelect) > 1) {
    filteredData <- filteredData %>% filter(storey_range %in% storyRangeSelect)
    }
    
    # Generate the box plot with the filtered data
    ggplot(filteredData, aes(x = town, y = resale_price)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Resale Price Distribution", y = "Price", x = "Town") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}