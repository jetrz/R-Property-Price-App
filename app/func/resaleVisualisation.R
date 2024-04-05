library(readxl)
library(stringr)
library(tidyverse)
library(shiny)
library(ggplot2)
library(dplyr)

condodata <- read.csv("data/condo.csv")
hdbdata <- read.csv("data/hdb.csv")

meanPriceByTown <- function(propertyType, townSelect, storyRangeSelect) {
    if (propertyType == "HDB") {
        filteredData <- hdbdata
    } else {
        filteredData <- condodata
        filteredData <- filteredData %>% rename(town=district, resale_price=price)
    }
    
    # Filter by selected towns, ignoring "All" if other towns are selected
    if (!"All" %in% townSelect || length(townSelect) > 1) {
    filteredData <- filteredData %>% filter(town %in% townSelect)
    }
    
    # Filter by selected story ranges, ignoring "All" if other ranges are selected
    if (propertyType == "HDB" && (!"All" %in% storyRangeSelect || length(storyRangeSelect) > 1)) {
    filteredData <- filteredData %>% filter(storey_range %in% storyRangeSelect)
    }
    
    # Generate the box plot with the filtered data
    ggplot(filteredData, aes(x = as.factor(town), y = resale_price)) +
    geom_boxplot() +
    theme_minimal() +
    labs(title = "Resale Price Distribution", y = "Price", x = "Town") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}