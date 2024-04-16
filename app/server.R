library(shiny)
library(shinydashboard)
library(stringr)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(kableExtra)

source("func/dataVisualisation.R")
source("func/resaleVisualisation.R")

# Read property data
property_data_hdb <- read.csv('data/hdb.csv')
condo_data <- read.csv('data/condo.csv')

# Define district-to-region mappings
CCR_districts <- c(1:2, 6, 9:11)
RCR_districts <- c(3:5, 7:8, 12:15, 20)
OCR_districts <- c(16:19, 21:28)

# Update the region based on district
property_data_hdb <- property_data_hdb %>%
  mutate(
    region = case_when(
      district %in% CCR_districts ~ "Core Central Region (CCR)",
      district %in% RCR_districts ~ "Rest of Central Region (RCR)",
      district %in% OCR_districts ~ "Outside Central Region (OCR)",
      TRUE ~ "Unknown"
    )
  )

condo_data <- condo_data %>%
  mutate(
    region = case_when(
      region == "CCR" ~ "Core Central Region (CCR)",
      region == "RCR" ~ "Rest of Central Region (RCR)",
      region == "OCR" ~ "Outside Central Region (OCR)",
      TRUE ~ "Unknown"
    )
  )


# Read in shapefile for polygons relating to URA postal districts
districts <- st_read("data/district_polygons/district_polygons.shp", crs = 4326) 

#Nearby Amenities
bus_stop_data <- read.csv("data/Amenities/bus_stops.csv")
mrt_data <- read.csv("data/Amenities/mrt_stations.csv")
hospitals_data <- read.csv("data/Amenities/hospitals.csv")
mall_data <- read.csv("data/Amenities/shopping_mall_coordinates.csv")

# Create icons
bus_stop_icon <- makeIcon(iconUrl = "data/icons/bus-stop-icon.png", iconWidth = 30, iconHeight = 30)
mrt_icon <- makeIcon(iconUrl = "data/icons/mrt-icon.png", iconWidth = 30, iconHeight = 30)
hospital_icon <- makeIcon(iconUrl = "data/icons/hospital-icon.png", iconWidth = 30, iconHeight = 30)
shopping_mall_icon <- makeIcon(iconUrl = "data/icons/shopping-mall-icon.png", iconWidth = 30, iconHeight = 30)

# Define server logic
shinyServer(function(input, output, session) {

  ################################
  ##### PROPERTY FINDER TAB ######
  ################################

  # Reactive expressions to filter data based on selected region
  filtered_data_region_hdb <- reactive({
    property_data_hdb %>%
      filter(region == input$region_select)
  })

  filtered_data_region_condo <- reactive({
    condo_data %>%
      filter(region == input$region_select)
  })
  
  # Render leaflet map for "property_finder" tab
  output$propertyMap_property_finder <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = districts, 
                  weight = 2, 
                  stroke = TRUE, 
                  smoothFactor = 0.5, 
                  fillOpacity = 0.5, 
                  fillColor = ~case_when(
                    district %in% CCR_districts ~ "#FFC0CB",  # Light pink for CCR
                    district %in% RCR_districts ~ "#ADD8E6",  # Light blue for RCR
                    district %in% OCR_districts ~ "#90EE90",  # Light green for OCR
                    TRUE ~ NA_character_
                  ), 
                  color = "#333",  # Dark gray border color
                  popup = ~paste("District:", district),
                  group = "Polygons") %>%
      setView(lng = 103.8196, lat = 1.3521, zoom = 11) %>%
      addLegend(
        position = "bottomright",
        colors = c("#FFC0CB", "#ADD8E6", "#90EE90"),
        labels = c("Core Central Region (CCR)", 
                   "Rest of Central Region (RCR)",
                   "Outside Central Region (OCR)"),
        title = "Regions"
      )
  })
  
  
  # Render box plot UI for selected region
  output$price_chart_ui <- renderUI({
    if (input$dropdown == "Price Charts") {
      fluidRow(
        column(width = 12,
               box(width = NULL, solidHeader = TRUE,
                   selectInput("region_select", "Select Region:", choices = c("Core Central Region (CCR)", 
                                                                              "Rest of Central Region (RCR)",
                                                                              "Outside Central Region (OCR)")),
                   actionButton("update_plot", "Update Plot", style="margin-bottom:1em;"),
                   plotOutput("region_boxplot"),
                   plotOutput("line_chart")
               )
        )
      )
    }
  })
  
  # Render box plot for selected region
  output$region_boxplot <- renderPlot({
    req(input$update_plot)

    if (input$property_type_selector == "HDB") {
      filtered_data <- filtered_data_region_hdb()
    } else {
      filtered_data <- data.frame(filtered_data_region_condo())
      filtered_data <- filtered_data %>% rename(resale_price=price)
    }

    if (!is.null(filtered_data)) {
      ggplot(filtered_data, aes(x = resale_price)) +
        geom_boxplot() +
        labs(title = paste("Average Price in", input$region_select, "Region")) +
        theme_minimal()
    }
  })
  
  # Render line chart for selected town
  output$line_chart <- renderPlot({
    req(input$update_plot)  # Ensure input$update_plot and input$region are not null
    if (input$property_type_selector == "HDB") {
      filtered_data <- filtered_data_region_hdb()
    } else {
      filtered_data <- data.frame(filtered_data_region_condo())
      filtered_data <- filtered_data %>% rename(resale_price=price)
    }

    if (is.null(filtered_data)) { return; }

    if (input$property_type_selector == "HDB") {
      # Extract month and year
      filtered_data$year <- substr(filtered_data$month, 1, 4)  # Corrected from filtered_data_region$month
      filtered_data$month <- sub(".*-", "", filtered_data$month)
    }

    # Find the latest year in the dataset
    latest_year <- as.numeric(max(filtered_data$year)) - 1
    
    # Filter data for the latest year
    filtered_data_latest_year <- filtered_data[filtered_data$year == latest_year, ]
    
    # Calculate average resale price by month
    monthly_avg_price <- filtered_data_latest_year %>%
      group_by(month) %>%
      summarise(avg_resale_price = mean(resale_price))

    # Convert the month column to factor and specify levels as month names
    if (input$property_type_selector == "HDB") {
      monthly_avg_price$month <- factor(monthly_avg_price$month, levels = sprintf("%02d", 1:12), labels = month.abb)
    } else {
      monthly_avg_price$month <- factor(monthly_avg_price$month, levels = as.character(1:12), labels = month.abb)
    }
    
    # Plot the line chart using ggplot2 with the updated month labels
    ggplot(monthly_avg_price, aes(x = month, y = avg_resale_price, group = 1)) +
      geom_line() +
      labs(x = "Month", y = "Average Resale Price", title = paste("Average Resale Price In", as.character(latest_year)))
  })
  
  # Calculate cash proceeds
  cash_proceeds <- reactive({
    input$sale_price - input$outstanding_loan - input$cpf_refund - input$other_fees
  })
  
  # Calculate amount available for purchase
  amount_available <- reactive({
    input$cpf_balance + cash_proceeds() + input$loan + input$hdb_grant - input$bsd - input$absd - input$other_commitments
  })
  
  # Render cash proceeds
  output$cash_proceeds <- renderPrint({
    cash_proceeds()
  })
  
  # Render amount available for purchase
  output$amount_available <- renderPrint({
    amount_available()
  })
  
  ##AMENITIES##
  output$amenities_ui <- renderUI({
    if (input$dropdown == "Nearby Amenities") {
      fluidRow(
        column(width = 12,
               box(width = NULL, solidHeader = TRUE,
                   radioButtons("amenity_selector", label = "Select Amenity:", 
                                choices = c("Hospitals", "Bus Stops", "MRT Stations", "Shopping Malls"),
                                selected = "Hospitals")
               )
        )
      )
    } else {
      NULL
    }
  })
  
  # Function to render nearby amenities on the map
  renderNearbyAmenities <- function(data, icon, popup, lat_col, lon_col) {
    if (nrow(data) > 0 && (lat_col %in% colnames(data)) && (lon_col %in% colnames(data))) {
      leafletProxy("propertyMap_property_finder") %>%
        clearMarkers() %>%
        addMarkers(data = data,
                   icon = icon,
                   lng = data[[lon_col]],
                   lat = data[[lat_col]],
                   popup = popup)
    } else {
      leafletProxy("propertyMap_property_finder") %>%
        clearMarkers()
    }
  }

  # Render map with nearby amenities based on selected option
  observe({
    if (!is.null(input$dropdown) && !is.null(input$amenity_selector) && input$dropdown == "Nearby Amenities") {
      if (input$amenity_selector == "Hospitals") {
        renderNearbyAmenities(hospitals_data, hospital_icon, ~paste0("<b>", hospitals_data$Name, "</b><br>Address: ", hospitals_data$Address), "Y", "X")
      } else if (input$amenity_selector == "Bus Stops") {
        renderNearbyAmenities(bus_stop_data, bus_stop_icon, ~paste0("<b>", bus_stop_data$Description, "</b><br>Stop No: ", bus_stop_data$BusStopCode), "Latitude", "Longitude")
      } else if (input$amenity_selector == "MRT Stations") {
        renderNearbyAmenities(mrt_data, mrt_icon, ~paste0("<b>", mrt_data$STN_NAME, "</b><br>Station No: ", mrt_data$STN_NO), "Latitude", "Longitude")
      } else if (input$amenity_selector == "Shopping Malls") {
        renderNearbyAmenities(mall_data, shopping_mall_icon, ~paste0("<b>", mall_data$Mall.Name, "</b><br>"), "LATITUDE", "LONGITUDE")
      }
    } else if (input$dropdown == "Price Charts") {
      # Clear the map and reset radio buttons
      leafletProxy("propertyMap_property_finder") %>%
        clearMarkers()
      updateRadioButtons(session, "amenity_selector", selected = NULL)
    }
  })

  ################################
  #### DATA VISUALISATION TAB ####
  ################################

  observeEvent(input$HDBPriceByAreaBarButton, {
    output$plotArea <- renderPlot({
      HDBPriceByRegionBar()
    })
  })

  observeEvent(input$CondoPriceByAreaBarButton, {
    output$plotArea <- renderPlot({
      CondoPriceByRegionBar()
    })
  })

  observeEvent(input$CondoPriceByAreaBoxButton, {
    output$plotArea <- renderPlot({
      CondoPriceByRegionBox()
    })
  })

  observeEvent(input$HDBPriceByDistrictBarButton, {
    output$plotArea <- renderPlot({
      HousingPriceByDistrictBar()
    })
  })

  observeEvent(input$ResalePriceByRegionDensityButton, {
    output$plotArea <- renderPlot({
      ResalePriceByRegionDensity()
    })
  })

  observeEvent(input$RelLeaseDurResalePriceButton, {
    output$plotArea <- renderPlot({
      RelLeaseDurResalePrice()
    })
  })

  observeEvent(input$PriceByRegionAndSizeButton, {
    output$plotArea <- renderPlot({
      PriceByRegionAndSize()
    })
  })
  
  observeEvent(input$TransactionByYearButton, {
    output$plotArea <- renderPlot({
      MeanTransactionByYear()
    })
  })

  ################################
  ### TOWN INFO TAB ###
  ################################

  # Observe click on Clear Town Selection button
  observeEvent(input$clearTownHDB, {
    updateSelectInput(session, "townSelectHDB", selected = "All")
  })
  
  # Observe click on Clear Story Range Selection button
  observeEvent(input$clearStoryRange, {
    updateSelectInput(session, "storyRangeSelect", selected = "All")
  })

  output$hdbPricePlot <- renderPlotly({
    p <- meanPriceByTown("HDB", input$townSelectHDB, timeRange=input$time_range_selector, storyRangeSelect=input$storyRangeSelect)
    ggplotly(p, tooltip = c("y", "lower", "middle", "upper", "ymin", "ymax"))
  })

  observeEvent(input$clearTownCondo, {
    updateSelectInput(session, "townSelectCondo", selected = "All")
  })

  output$condoPricePlot <- renderPlotly({
    p <- meanPriceByTown("Condo", input$townSelectCondo, timeRange=input$time_range_selector)
    ggplotly(p, tooltip = c("y", "lower", "middle", "upper", "ymin", "ymax"))
  })

  # ################################
  # ######## AMENITIES TAB #########
  # ################################
  # bus_stop_copy = data.frame(bus_stop_data)
  # bus_stop_copy$Description <- str_to_title((as.character(bus_stop_copy$Description)))
  # mrt_copy = data.frame(mrt_data)
  # mrt_copy$Name <- paste(mrt_copy$STN_NAME, mrt_copy$STN_NO, sep=", ")
  # malls_copy = data.frame(mall_data)
  # malls_copy <- malls_copy %>% rename(Longitude=LONGITUDE)
  # malls_copy <- malls_copy %>% rename(Latitude=LATITUDE)
  # hospitals_copy = data.frame(hospitals_data)
  # hospitals_copy <- hospitals_copy %>% rename(Longitude=X)
  # hospitals_copy <- hospitals_copy %>% rename(Latitude=Y)

  # output$map <- renderLeaflet({
  #   leaflet() %>%
  #     addTiles() %>%
  #     setView(lng = 103.850, lat = 1.290, zoom = 12)
  # })

  # observe({
  #   # Clear existing markers each time the input changes
  #   leafletProxy("map") %>% clearMarkers()

  #   if (input$amenityType == "Bus Stops") {
  #     addMarkersFor(bus_stop_copy, "Description","#FF5733")
  #   } else if (input$amenityType == "MRT Stops") {
  #     addMarkersFor(mrt_copy, "Name","#33CFFF")
  #   } else if (input$amenityType == "Malls") {
  #     addMarkersFor(malls_copy, "Mall.Name","#8E44AD")
  #   } else if (input$amenityType == "Hospitals") {
  #     addMarkersFor(hospitals_copy, "Name","#2ECC71")
  #   } else if (input$amenityType == "All") {
  #     # Add markers for each dataset in turn, with the appropriate popup content
  #     addMarkersFor(bus_stop_copy, "Description","#FF5733")
  #     addMarkersFor(mrt_copy, "Name","#33CFFF")
  #     addMarkersFor(malls_copy, "Mall.Name","#8E44AD")
  #     addMarkersFor(hospitals_copy, "Name","#2ECC71")
  #   }
  # })

  # # Adjusted function to add markers for a given dataset with dynamic popup content
  # addMarkersFor <- function(data, popupColumn, color) {
  #   leafletProxy("map", data = data) %>%
  #     addCircleMarkers(
  #       lng = ~Longitude, lat = ~Latitude,
  #       popup = ~get(popupColumn, envir = as.environment(data)),color = color,
  #     fillColor = color,
  #     fillOpacity = 0.8,
  #     radius = 6
  #     )
  # }

})
