library(dplyr)
library(ggplot2)
library(tidyverse)

condodata <- read.csv("data/condo.csv")
hdbdata <- read.csv("data/hdb.csv")

hdbdata %>% group_by(town) %>% summarise(count = n())
hdbdata$town <- as.factor(hdbdata$town)
hdbdata$district <- as.factor(hdbdata$district)
hdbdata$region <- as.factor(hdbdata$category) # changeed this line

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

HDBPriceByAreaBar <- function() {
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

CondoPriceByAreaBar <- function() {
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

CondoPriceByAreaBox <- function() {
    ggplot(hdbdata, aes(x = factor(district), y = resale_price, fill = region)) + 
        geom_boxplot(width = 0.8, position = position_dodge(width = 0.7), outlier.shape = NA) +  # Adjust boxplot width and spacing
        scale_fill_manual(values = c("CCR" = "red", "RCR" = "blue", "OCR" = "green")) +
        theme_minimal() +
        labs(title = "Prices by District with Region Color Coding",
            x = "District",
            y = "Price",
            fill = "Region") +
        theme(axis.text.x = element_text(angle = 0, hjust = 1)) +  # Rotate x-axis labels for better readability
        scale_y_continuous(trans = "log")  # Use log-scale for y-axis if needed
}

mean_prices_by_district_condo <- mean_prices_by_district_condo %>%
  mutate(source = "Condo")

mean_prices_by_district_hdb <- mean_prices_by_district_hdb %>%
  mutate(source = "HDB")

# Combine the datasets
combined_data <- rbind(mean_prices_by_district_condo, mean_prices_by_district_hdb)

HDBPriceByDistrictBar <- function() {
    ggplot(combined_data, aes(x = category, y = mean_price, fill = source)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        theme_minimal() +
        labs(title = "Comparison of Mean Prices by Region",
            x = "Region",
            y = "Mean Price",
            fill = "Source") +
        scale_fill_manual(values = c("Condo" = "blue", "HDB" = "red")) # You can change the colors as per your preference
}

#check types of sale type
condodata$saletype <- as.factor(condodata$saletype)
all_levels_saletype <- levels(condodata$saletype)

#verify if sum of certain sale type is same as sum of years left above 990000
sum(condodata$saletype == "Resale")
sum(condodata$fh_status)

hdbdata <- hdbdata %>%
  mutate(
    lease_years = as.numeric(sub(" years.*", "", remaining_lease)),
    lease_months = as.numeric(sub(".* years ", "", sub(" months", "", remaining_lease))),
    total_lease_years = lease_years + lease_months / 12
  ) %>%
  select(-lease_years, -lease_months)  # Remove the intermediate columns if they are no longer needed

ResalePriceByRegionDensity <- function() {
    ggplot(hdbdata, aes(x = total_lease_years, fill = region)) +
        geom_density(alpha = 0.5) +  # Density plot with transparency
        labs(title = "Density of Resale Prices by Remaining Lease Duration and Region",
            x = "Remaining Lease (Years)",
            y = "Density",
            fill = "Region") +
        scale_fill_manual(values = c("blue", "green", "red")) +  # Custom color palette for regions
        theme_minimal()  # Minimalist theme
}

# Extract years from 'remaining_lease' and calculate total lease duration in years
hdbdata <- hdbdata %>%
  mutate(
    lease_years = as.numeric(sub(" years.*", "", remaining_lease)),
    lease_months = as.numeric(sub(".* years ", "", sub(" months", "", remaining_lease))),
    total_lease_years = lease_years + lease_months / 12
  ) %>%
  select(-lease_years, -lease_months)  # Remove the intermediate columns if they are no longer needed

# Filter HDB data for regions OCR, CCR, and RCR
mean_resale_price_by_region <- hdbdata %>%
  filter(region %in% c("OCR", "CCR", "RCR")) %>%
  group_by(total_lease_years, region) %>%
  summarize(mean_resale_price = mean(resale_price, na.rm = TRUE)) %>%
  ungroup()

RelLeaseDurResalePrice <- function() {
    ggplot(mean_resale_price_by_region, aes(x = total_lease_years, y = mean_resale_price, color = region)) +
        geom_line() +
        labs(title = "Mean Resale Price by Region Over Remaining Lease Duration",
            x = "Remaining Lease (Years)",
            y = "Mean Resale Price",
            color = "Region") +
        theme_minimal()
}