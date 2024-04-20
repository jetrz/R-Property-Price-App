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

    output$graph_desc <- renderUI({
      p("District 10 stands out as the sole district within the Core Central Region (CCR) that includes public housing, with neighboring areas predominantly featuring upscale residences like Good Class Bungalows (GCB) and luxury apartments. This unique positioning could influence the pricing of HDBs in District 10, potentially making them pricier compared to those in other districts. Generally, housing prices tend to decrease as one moves further from the city center. Additionally, HDBs located in the Rest of Central Region (RCR) often fetch higher prices than those in the Outside Central Region (OCR), suggesting a gradient in property prices radiating outward from the central region of Singapore. Prospective buyers should consider these dynamics carefully, especially when contemplating purchases in pricier central districts like District 10.")
    })
  })

  observeEvent(input$CondoPriceByAreaBarButton, {
    output$plotArea <- renderPlot({
      CondoPriceByRegionBar()
    })

    output$graph_desc <- renderUI({
      p("Condominium prices mirror the trends observed in HDBs, with the highest prices typically found in the CCR as visualized in Figure 8. However, unlike HDBs, the variance in condo prices is more pronounced across different districts within the RCR and OCR. For example, condos in District 21 (OCR), a mature estate, may command higher mean prices compared to those in the less mature District 8 (RCR). For property owners in the CCR considering a sale, the premium pricing of this region could potentially yield significant returns. Additionally, when evaluating real estate investments, one should also consider the general maturity of the district, as it plays a crucial role in influencing property values. This factor is particularly important in assessing the potential growth and stability of property prices across various districts.")
    })
  })

  observeEvent(input$CondoPriceByAreaBoxButton, {
    output$plotArea <- renderPlot({
      CondoPriceByRegionBox()
    })

    output$graph_desc <- renderUI({
      p("District 10, located within CCR, is characterized by notably higher property prices, but with a comparatively narrow range of price fluctuation. This suggests a more stable and consistent market within the CCR. In contrast, the RCR exhibits greater variability in property prices, indicating a wider spread that might appeal to investors looking for potential upsides but could deter those adverse to risk. Meanwhile, the OCR tends to display more uniform and consistent price spreads, making it a viable option for buyers with a preference for predictability and lower budgets. For potential buyers, understanding the spread of property prices within these regions is crucial. Those with a lower tolerance for price variability might find the stable pricing in the CCR more appealing if their budget allows, while the OCR offers a more affordable and consistent market for those with budget constraints.")
    })
  })

  observeEvent(input$HDBPriceByDistrictBarButton, {
    output$plotArea <- renderPlot({
      HousingPriceByDistrictBar()
    })

    output$graph_desc <- renderUI({
      p("Condominiums consistently command higher prices across all regions, with OCR typically offering the most affordable options. This price discrepancy among regions is notably more pronounced for condominiums compared to Housing Development Board (HDB) flats. For instance, the mean prices of condos in CCR are nearly double those in the OCR. This stark difference underscores the significant variation in property values across different areas of Singapore. For potential property investors or homeowners considering a change, downgrading from a condominium to an HDB flat within the same region could yield substantial profits, given the considerable price differences. Moreover, transitioning from a condominium in a higher-priced region to an HDB flat in a lower-priced region could potentially result in even greater returns. Understanding these price dynamics is essential for users navigating the real estate market, as it can inform strategic decisions regarding property investments or changes in housing preferences.")
    })
  })

  observeEvent(input$RelLeaseDurResalePriceButton, {
    output$plotArea <- renderPlot({
      RelLeaseDurResalePrice()
    })

    output$graph_desc <- renderUI({
      tags$div(
      p("In general, there is a positive correlation between the mean resale price of HDB and the remaining lease years. Across all three regions, a longer lease term corresponds to higher resale prices. This alignment is intuitive, considering that properties with longer remaining lease durations hold greater value for potential buyers. However, upon closer look, it was also observed that for CCR, the trend is not so obvious as most data fall within the 50-70 years of remaining lease, potentially because most HDB in CCR are relatively old and thus not having longer remaining lease durations. "),
      p("Considering these factors, potential buyers seeking to downgrade to HDB should thoughtfully evaluate the remaining lease duration. Although longer leases typically fetch higher prices, this pattern may not always be true in the CCR due to the prevalence of older properties with shorter remaining leases. Depending on desired lease duration and budget constraints, buyers may explore various regions offering different lease terms. Additionally, they should factor in future resale potential if they consider alternative options down the line.")
      )
    })
  })

  observeEvent(input$PriceByRegionAndSizeButton, {
    output$plotArea <- renderPlot({
      PriceByRegionAndSize()
    })

    output$graph_desc <- renderUI({
      tags$div(
      p("The trend observed for condominiums reveals a decline in the mean property price with an increase in property size. Conversely, the trend for HDB flats appears less consistent, indicating variations in pricing dynamics across different regions. Despite this discrepancy, both property types consistently exhibit higher mean prices in the CCR compared to other regions, underscoring the influential role of this region in shaping market prices."),
      p("The mean price per sqm for properties located in RCR shows a rather interesting trend. While the average price for condominiums tends to decrease alongside property size, the opposite is true for HDB flats. This contrast in pricing trends within the RCR region presents an intriguing insight. It suggests a potential consideration for consumers contemplating a downgrade from larger condominiums to smaller HDB flats, as they may encounter varying price dynamics within this region.")
      )
    })
  })
  
  observeEvent(input$TransactionByYearButton, {
    output$plotArea <- renderPlot({
      MeanTransactionByYear()
    })

    output$graph_desc <- renderUI({
      tags$div(
      p("The mean transaction price for HDB housing and condominiums show different trends over the years. For HDB properties, there is a general upward trend in the resale price from 2018 to 2022. Particularly, the significant increase in resale prices observed in 2021 potentially stemmed from intense bidding wars and substantial cash offers for popular flats during that period. price bidding wars. However, in 2022 the rate of increase was visibly lower compared to 2021, attributing to the property cooling measures implemented in September, 2022. On the other hand, the condominium transaction price fell significantly from 2018 to 2020, and rose back to a similar level from 2021 to 2022. This fluctuation in condominium prices could be influenced by various factors such as changes in market sentiment, the Covid pandemic, and government policies."),
      p("As buyers, it is crucial to be aware of the regulatory environment within the real estate industry, as it significantly influences price movements over time. By understanding regulatory changes and their potential impact on the market, buyers can make more informed decisions and better anticipate price fluctuations. ")
      )
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

})
