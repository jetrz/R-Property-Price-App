library(dplyr)
library(ggplot2)
library(tidyverse)

condodata <- read.csv("data/condo.csv")
hdbdata <- read.csv("data/hdb.csv")

HDBMeanPriceByArea <- function() {
    hdbdata %>% group_by(town) %>% summarise(count = n())
    hdbdata$town <- as.factor(hdbdata$town)
    hdbdata$district <- as.factor(hdbdata$district)
    # hdbdata$region <- as.factor(hdbdata$region)
    
    all_levels <- levels(hdbdata$district)

    hdbdata$category <- case_when(
        hdbdata$district %in% c(1, 2, 6, 9, 10, 11) ~ "CCR",
        hdbdata$district %in% c(3, 4, 5, 7, 8, 12, 13, 14, 15, 20) ~ "RCR",
        hdbdata$district %in% c(16, 17, 18, 19, 21, 22, 23, 24, 25, 26, 27, 28) ~ "OCR",
        TRUE ~ NA_character_ # Catch-all for unmatched cases
    )

    # Calculate mean prices for each district within merged_hdb, keeping the category for coloring
    mean_prices_by_district_hdb <- hdbdata %>%
        group_by(district, category) %>%
        summarize(mean_price = mean(resale_price, na.rm = TRUE)) %>%
        ungroup()

    # Plotting with districts on the x-axis and colors indicating category
    ggplot(mean_prices_by_district_hdb, aes(x = factor(district), y = mean_price, fill = category)) +
        geom_bar(stat = "identity") + 
        scale_fill_manual(values = c("CCR" = "red", "RCR" = "blue", "OCR" = "green")) +
        theme_minimal() +
        labs(title = "Mean HDB Prices by District with Category Color Coding",
            x = "District",
            y = "Mean Price",
            fill = "Category") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Improve readability of district labels
}

CondoMeanPriceByArea <- function() {
    condodata$district <- as.factor(condodata$district)
    condodata$region <- as.factor(condodata$region)
    all_levels <- levels(condodata$district)
    condodata$category <- case_when(
        condodata$district %in% c(1, 2, 6, 9, 10, 11) ~ "CCR",
        condodata$district %in% c(3, 4, 5, 7, 8, 12, 13, 14, 15, 20) ~ "RCR",
        condodata$district %in% c(16, 17, 18, 19, 21, 22, 23, 24, 25, 26, 27, 28) ~ "OCR",
        TRUE ~ NA_character_ # For any district number not covered above
    )
    mean_prices_by_district_condo <- condodata %>%
        group_by(district, category) %>%
        summarize(mean_price = mean(price, na.rm = TRUE)) %>%
        ungroup()

    # Plotting with districts on the x-axis and colors indicating category
    ggplot(mean_prices_by_district_condo, aes(x = factor(district), y = mean_price, fill = category)) +
    geom_bar(stat = "identity", position = position_dodge()) + 
        scale_fill_manual(values = c("CCR" = "red", "RCR" = "blue", "OCR" = "green")) +
        theme_minimal() +
        labs(title = "Mean Prices by District with Category Color Coding",
            x = "District",
            y = "Mean Price",
            fill = "Category") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Improve readability of district labels
}