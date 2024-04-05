# IGNORE THIS FILE

# library(jsonlite)
# library(ggplot2)
# library(ggmap)

# setwd(".")
# ggmap::register_google(key="AIzaSyD_DIuy-9VqU1afL6erNIvCtcAJkYM4OMg", write=TRUE)
# ggmap::register_stadiamaps("94b20d34-29ab-4571-b1fb-13ed31428156", write=TRUE)

# #The function to get data for given date and time
# #date must in format "yyyy-MM-dd"
# #time must in format "HH:mm:ss"
# getAvailabilityData <- function(date, time) {
#   url = "https://api.data.gov.sg/v1/transport/taxi-availability?date_time="
#   url <- paste0(url,date,"T",time)
#   data <- fromJSON(url)
#   data <- as.data.frame(data$features$geometry$coordinates)
#   colnames(data) <- c("long","lat")
#   data
# }

# #The function is to plot taxi availability for given date and time with given format
# plotAvailability <- function(date, time, format) {
#   data <- getAvailabilityData(date,time)
#   map <- get_map('Singapore', zoom = 11, maptype = 'stamen_toner_lite')
#   if(format=='point') {
#     ggmap(map) +  geom_point(data=data, aes(x = long, y = lat),color='red',alpha=0,5)
#   }
#   else if (format == 'heatmap'){
#     ggmap(map) + stat_density2d(data=data, aes(x=long, y=lat,  fill = ..level.., alpha = ..level..), size = 2, geom = 'polygon')+
#     scale_fill_gradient(low = "green", high = "red", guide=FALSE) + scale_alpha(range = c(0, 1), guide = FALSE)
#   }
#   else {print("Wrong format name")}
  
# }