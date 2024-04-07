# install.packages(c("shiny", "jsonlite", "ggplot2", "ggmap", "shinydashboard", "leaflet", "sf", "dplyr", "plotly", "kableExtra", "tidyverse", "readxl", "rsconnect"))

library(shiny)
library(shinydashboard)
library(stringr)
library(leaflet)
library(sf)
library(dplyr)
library(plotly)
library(kableExtra)

condodata <- read.csv("data/condo.csv")
hdbdata <- read.csv("data/hdb.csv")

orderedStoryRange <- c("01 TO 03","04 TO 06","07 TO 09","10 TO 12","13 TO 15","16 TO 18","19 TO 21","22 TO 24","25 TO 27","28 TO 30","31 TO 33","34 TO 36","37 TO 39","40 TO 42","43 TO 45","46 TO 48","49 TO 51")

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Property Price Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Property Finder", tabName = "property_finder"),
      menuItem("Calculator", tabName = "calculator"),
      menuItem("Resale Visualisations", tabName = "resale_visualisations"),
      menuItem("Amenities", tabName = "amenities"),
      menuItem("Data Visualisations", tabName = "data_visualisations"),  # New tab for data visualizations
      menuItem("About", tabName = "about"),
      menuItem("FAQs", tabName = "faqs")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "property_finder",
              fluidRow(
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           leafletOutput("propertyMap_property_finder", height = 500)
                       )
                ),
                column(width = 3,
                       radioButtons("property_type_selector", label = "Select Property Type", 
                                    choices = c("HDB", "Condo"), selected = "HDB"),
                       selectInput("dropdown", label = "Select:", choices = c("Price Charts", "Nearby Amenities")),
                       uiOutput("price_chart_ui"),
                       uiOutput("amenities_ui")
                )
              )
      ),
      
      # Calculator tab
      tabItem(tabName = "calculator",
              fluidRow(
                column(width = 6,
                       wellPanel(
                         h4("Calculator"),
                         numericInput("sale_price", "Condo Sale Price (SGD)", value = 0),
                         numericInput("outstanding_loan", "Outstanding Loan Amount (SGD)", value = 0),
                         numericInput("cpf_refund", "CPF Refund (SGD)", value = 0),
                         numericInput("other_fees", "Other Fees (Legal, Property Agent, Taxes, etc.) (SGD)", value = 0)
                       )
                ),
                column(width = 6,
                       wellPanel(
                         h4("Calculator"),
                         numericInput("cpf_balance", "Total CPF Balance Amount (SGD)", value = 0),
                         numericInput("loan", "Loan (SGD)", value = 0),
                         numericInput("hdb_grant", "HDB Grant (SGD)", value = 0),
                         numericInput("bsd", "Buyer's Stamp Duty (BSD) (SGD)", value = 0),
                         numericInput("absd", "Additional Buyer's Stamp Duty (ABSD) (SGD)", value = 0),
                         numericInput("other_commitments", "Other Financial Commitments (SGD)", value = 0)
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       h4("Potential Cash Proceeds from Sale of Condo:"),
                       verbatimTextOutput("cash_proceeds"),
                       h4("Amount Available for Purchasing a HDB:"),
                       verbatimTextOutput("amount_available")
                )
              )
      ),
      # Resale Flats Price Visualisation tab
      tabItem(tabName = "resale_visualisations",
              h2("Resale Flats Price Visualisation"),
              radioButtons("property_type_selector_2", label = "Select Property Type",
                          choices = c("HDB", "Condo"), selected = "HDB"),

              conditionalPanel(
                condition = "input.property_type_selector_2 === 'HDB'",
                radioButtons("time_range_selector", label = "Select Time Range",
                          choices = c("Year", "5 Years", "10 Years", "All"), selected = "All"),
                selectInput("townSelectHDB", "Select Town(s):", 
                          choices = c("All" = "All", unique(hdbdata$town)),selected = "All", multiple = TRUE),
                actionButton("clearTownHDB", "Clear Town Selection"),
                selectInput("storyRangeSelect", "Select Story Range(s):", 
                            choices = c("All"="All", orderedStoryRange),selected = "All", multiple = TRUE),
                actionButton("clearStoryRange", "Clear Story Range Selection"),
                plotlyOutput("hdbPricePlot")
              ),

              conditionalPanel(
                condition = "input.property_type_selector_2 === 'Condo'",
                selectInput("townSelectCondo", "Select Town(s):", 
                            choices = c("All" = "All", unique(condodata$district)),selected = "All", multiple = TRUE),
                actionButton("clearTownCondo", "Clear Town Selection"),
                plotlyOutput("condoPricePlot")
              )
      ),
      # Amenities Tab
      tabItem(tabName = "amenities",
              radioButtons("amenityType", "Choose Amenity Type:",
                   choices = c("All", "Bus Stops", "MRT Stops", "Malls", "Hospitals")),
              leafletOutput("map")
      ),
      # Data Visualisations tab
      tabItem(tabName = "data_visualisations",
              h2("Data Visualisations"),
              p("This is the Data Visualisations page. You can add your visualizations here."),
              p("Click the button to display the HDB Mean Price by Area graph."),
              actionButton("HDBPriceByAreaBarButton", "HDB Prices By District & Region (Bar)"),
              actionButton("CondoPriceByAreaBarButton", "Condo Prices By District & Region (Bar)"),
              actionButton("CondoPriceByAreaBoxButton", "Condo Prices By District & Region (Box)"),
              actionButton("HDBPriceByDistrictBarButton", "HDB Prices By District (Bar)"),
              actionButton("ResalePriceByRegionDensityButton", "Resale Prices By Region (Density)"),
              actionButton("RelLeaseDurResalePriceButton", "Relationship between Remaining Lease Duration & Resale Prices"),
              actionButton("PriceByRegionAndSizeButton", "Mean Price by Region and Size"),
              plotOutput("plotArea")
      ),
      # About tab
      tabItem(tabName = "about",
              h2("About"),
              p("Our app aims to revolutionize property transactions in Singapore by providing transparent and comprehensive market data. With insights into property prices and trends, buyers and sellers can make informed decisions, reducing reliance on costly intermediaries."),
              h3("Objective:"),
              p("Empower users with transparent property market data for informed decision-making."),
              h3("Market Insight:"),
              p("Demand for downgrading properties prompts government intervention; existing platforms lack comprehensive market data."),
              h3("Pain Points:"),
              p("High commission fees, limited access to market data, and time-consuming research."),
              h3("Why the App?"),
              p("Address growing demand for transparency, facilitate informed investment decisions, and assist new home buyers in navigating the market."),
              h3("Features:"),
              p("Market price trend analysis, interactive maps, customizable search filters, and historical data visualization.")
      ),
      # FAQ tab
      tabItem(tabName = "faqs",
              h2("FAQs"),
              p("This is the FAQs page. You can add more content here.")
      )
    )
  )
)


# rsconnect::setAccountInfo(name='jetrz', token='5AB974D84D655BB4DCB1669B981058E9', secret='wmQnFWP4AOiWzYGzs6viid8E2JVskRENTMQztoRw')
# rsconnect::deployApp(appName='DBA3711')