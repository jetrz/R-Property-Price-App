library(dplyr)
library(ggplot2)
library(tidyverse)

condodata <- read.csv("data/condo.csv")
hdbdata <- read.csv("data/hdb.csv")

hdbdata %>% group_by(town) %>% summarise(count = n())
hdbdata$town <- as.factor(hdbdata$town)
hdbdata$district <- as.factor(hdbdata$district)

all_levels <- levels(hdbdata$district)

#Categorise into different regions based on district
hdbdata$region <- case_when(
  hdbdata$district %in% c(1, 2, 6, 9, 10, 11) ~ "CCR",
  hdbdata$district %in% c(3, 4, 5, 7, 8, 12, 13, 14, 15, 20) ~ "RCR",
  hdbdata$district %in% c(16, 17, 18, 19, 21, 22, 23, 24, 25, 26, 27, 28) ~ "OCR",
  TRUE ~ NA_character_ # Catch-all for unmatched cases
)

condodata$district <- as.factor(condodata$district)
condodata$region <- as.factor(condodata$region)
all_levels <- levels(condodata$district)
condodata$region <- case_when(
  condodata$district %in% c(1, 2, 6, 9, 10, 11) ~ "CCR",
  condodata$district %in% c(3, 4, 5, 7, 8, 12, 13, 14, 15, 20) ~ "RCR",
  condodata$district %in% c(16, 17, 18, 19, 21, 22, 23, 24, 25, 26, 27, 28) ~ "OCR",
  TRUE ~ NA_character_ # For any district number not covered above
)


# Calculate mean prices for each district within merged_hdb, keeping the region for coloring
mean_prices_by_district_hdb <- hdbdata %>%
  group_by(district, region) %>%
  summarize(mean_price = mean(resale_price, na.rm = TRUE)) %>%
  ungroup()

#Plot 1
HDBPriceByRegionBar <- function() {
  # Plotting with districts on the x-axis and colors indicating region
  ggplot(mean_prices_by_district_hdb, aes(x = factor(district), y = mean_price, fill = region)) +
    geom_bar(stat = "identity") + 
    scale_fill_manual(values = c("CCR" = "red", "RCR" = "blue", "OCR" = "green")) +
    theme_minimal() +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Mean HDB Prices by District with region Color Coding",
         x = "District",
         y = "Mean Price(SGD)",
         fill = "region") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Improve readability of district labels
}

mean_prices_by_district_condo <- condodata %>%
  group_by(district, region) %>%
  summarize(mean_price = mean(price, na.rm = TRUE)) %>%
  ungroup()

#Plot 2
CondoPriceByRegionBar <- function() {
  # Plotting with districts on the x-axis and colors indicating region
  ggplot(mean_prices_by_district_condo, aes(x = factor(district), y = mean_price, fill = region)) +
    geom_bar(stat = "identity", position = position_dodge()) + 
    scale_fill_manual(values = c("CCR" = "red", "RCR" = "blue", "OCR" = "green")) +
    theme_minimal() +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Mean Prices by District with region Color Coding",
         x = "District",
         y = "Mean Price(SGD)",
         fill = "region") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Improve readability of district labels
}

#Plot 3
CondoPriceByRegionBox <- function() {
  ggplot(hdbdata, aes(x = factor(district), y = resale_price, fill = category)) + 
    geom_boxplot(width = 0.8, position = position_dodge(width = 0.7), outlier.shape = NA) +  # Adjust boxplot width and spacing
    scale_fill_manual(values = c("CCR" = "red", "RCR" = "blue", "OCR" = "green")) +
    theme_minimal() +
    labs(title = "Prices by District with Region Color Coding",
         x = "District",
         y = "Price(SGD)",
         fill = "region") +
    theme(axis.text.x = element_text(angle = 0, hjust = 1)) +  # Rotate x-axis labels for better readability
    scale_y_continuous(trans = "log", labels = scales::comma)  # Use log-scale for y-axis if needed
}


mean_prices_by_district_condo <- mean_prices_by_district_condo %>%
  mutate(source = "Condo")

mean_prices_by_district_hdb <- mean_prices_by_district_hdb %>%
  mutate(source = "HDB")

# Combine the datasets
combined_data <- rbind(mean_prices_by_district_condo, mean_prices_by_district_hdb)

#Plot 4
HousingPriceByDistrictBar <- function() {
  combined_data <- combined_data[!is.na(combined_data$region),]
  ggplot(combined_data, aes(x = region, y = mean_price, fill = source)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme_minimal() +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "Comparison of Mean Prices by Region",
         x = "Region",
         y = "Mean Price (SGD)",
         fill = "Source") +
    scale_fill_manual(values = c("Condo" = "blue", "HDB" = "red"))
}

#check types of sale type
condodata$saletype <- as.factor(condodata$saletype)
all_levels_saletype <- levels(condodata$saletype)

#verify if sum of certain sale type is same as sum of years left above 990000
sum(condodata$saletype == "Resale")
sum(condodata$fh_status)

newhdbdata <- hdbdata %>%
  mutate(
    lease_years = as.numeric(sub(" years.*", "", remaining_lease)),
    lease_months = as.numeric(sub(".* years ", "", sub(" months", "", remaining_lease))),
    total_lease_years = lease_years + lease_months / 12
  ) %>%
  select(-lease_years, -lease_months)  # Remove the intermediate columns if they are no longer needed

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

#Plot 6
RelLeaseDurResalePrice <- function() {
  ggplot(mean_resale_price_by_region, aes(x = total_lease_years, y = mean_resale_price, color = region)) +
    geom_point(size = 2, alpha = 0.5) +  # Smaller and more transparent dots
    geom_smooth(method = "lm", se = FALSE, aes(group = region), color = "black") +  # Add trend line
    labs(title = "Mean Resale Price by Region Over Remaining Lease Duration",
         x = "Remaining Lease (Years)",
         y = "Mean Resale Price",
         color = "Region") +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal()
}


#Classfiy property by sqm area
#0-50sqm: small; 51-100sqm: medium; 100sqm and above: large

#Condo: Add a new column area_sqm to convert sqft to sqm 
condodata$area_sqm <- condodata$area / 10.7639

# Condo: Create a new column "size" based on area_sqm ranges
condodata$size <- cut(condodata$area_sqm, 
                      breaks = c(0, 50, 100, Inf), 
                      labels = c("small", "medium", "large"),
                      include.lowest = TRUE)

# HDB: Create a new column "size" based on floor_area_sqm ranges
hdbdata$size <- cut(hdbdata$floor_area_sqm, 
                    breaks = c(0, 50, 100, Inf), 
                    labels = c("small", "medium", "large"),
                    include.lowest = TRUE)


# Mean price by region and size
# For HDB data
hdbdata$price_per_sqm <- hdbdata$resale_price / hdbdata$floor_area_sqm

hdb_mean_prices <- hdbdata %>%
  group_by(region, size) %>%
  summarize(mean_price_per_sqm = mean(price_per_sqm))

# For condo data
condodata$price_per_sqm <- condodata$price / condodata$area_sq

condo_mean_prices <- condodata %>%
  group_by(region, size) %>%
  summarize(mean_price_per_sqm = mean(price_per_sqm))

# Combine HDB and condo data
new_combined_data <- rbind(
  mutate(hdb_mean_prices, type = "HDB"),
  mutate(condo_mean_prices, type = "Condo")
)

new_combined_data <- data.frame(new_combined_data)

#Plot 7
PriceByRegionAndSize <- function() {
  # Plotting
  ggplot(new_combined_data, aes(x = region, y = mean_price_per_sqm, fill = size)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ type, scales = "free") +
    labs(title = "Mean Price/sqm by Region and Size",
         x = "Region",
         y = "Mean Price/sqm (SGD)",
         fill = "Size") +
    theme_minimal() +
    scale_y_continuous(labels = scales::comma) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    coord_cartesian(ylim = range(new_combined_data$mean_price_per_sqm))
}


# Extract relevant years from 'year' and 'month' columns in condodata and hdbdata
condodata <- condodata %>%
  mutate(transaction_year = as.numeric(paste0("20", year)))

hdbdata <- hdbdata %>%
  mutate(transaction_year = as.numeric(substr(month, 1, 4)))

# Calculate mean price for each year in condodata
mean_price_condo <- condodata %>%
  group_by(transaction_year) %>%
  summarize(mean_price_condo = mean(price, na.rm = TRUE))

# Calculate mean price for each year in hdbdata
mean_price_hdb <- hdbdata %>%
  group_by(transaction_year) %>%
  summarize(mean_price_hdb = mean(resale_price, na.rm = TRUE))

# Combine the mean prices from both datasets
mean_prices <- merge(mean_price_condo, mean_price_hdb, by = "transaction_year", suffixes = c("_condo", "_hdb"))

# Reshape the data from wide to long format
mean_prices_long <- pivot_longer(mean_prices, 
                                 cols = c(mean_price_hdb, mean_price_condo),
                                 names_to = "price_type",
                                 values_to = "mean_price")

# Plot 8
MeanTransactionByYear<- function(){
  ggplot(mean_prices_long, aes(x = transaction_year, y = mean_price, color = price_type)) +
    geom_line() +
    geom_point() +
    labs(title = "Comparison of Mean Transaction Price by Year",
         x = "Year of Transaction",
         y = "Mean Price",
         color = "Price Type") +
    scale_color_manual(values = c("mean_price_hdb" = "red", "mean_price_condo" = "blue")) +
    theme_minimal()}
