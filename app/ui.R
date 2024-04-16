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

addResourcePath("bg", "data/bg")

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Property Price Helper"),
  dashboardSidebar(
    tags$head(includeCSS(("styles.css"))),
    sidebarMenu(
      menuItem("District Info", tabName = "district_info"),
      menuItem("Town Info", tabName = "town_info"),
      menuItem("Purchase/Sale Calculator", tabName = "calculator"),
      # menuItem("Amenities", tabName = "amenities"),
      menuItem("Data Visualisations", tabName = "data_visualisations"),  # New tab for data visualizations
      menuItem("About", tabName = "about")
    )
  ),
  dashboardBody(
    # District Info tab
    tabItems(
      tabItem(tabName = "district_info",
              h2("Property Info By Districts"),
              p("This tab shows information about property prices across districts."),
              tags$hr(style="border-color: black;"),
              fluidRow(
                column(width = 3,
                  tags$div(class="radio-inputs select-property-radio",
                    radioButtons("property_type_selector", label = "Select Property Type",
                    choices = c("HDB", "Condo"), selected = "HDB")
                  )
                ),
                column(width = 3,
                  selectInput("dropdown", label = "Select:", choices = c("Price Charts", "Nearby Amenities"))
                )
              ),
              uiOutput("price_chart_ui"),
              uiOutput("amenities_ui"),
              box(width = NULL, solidHeader = TRUE,
                  leafletOutput("propertyMap_property_finder", height = 500)
              )
      ),
      # Town Info tab
      tabItem(tabName = "town_info",
              h2("Property Info By Towns"),
              p("This tab shows information about property prices across towns."),
              tags$hr(style="border-color: black;"),
              fluidRow(style="margin-bottom:25px;",
                column( width = 3,
                  tags$div(class="radio-inputs select-property-radio",
                    radioButtons("property_type_selector_2", label = "Select Property Type",
                            choices = c("HDB", "Condo"), selected = "HDB")
                  )
                ),
                column( width = 4,
                  conditionalPanel(
                    condition = "input.property_type_selector_2 === 'HDB'",
                    tags$div(class="radio-inputs",
                             radioButtons("time_range_selector", label = "Select Time Range",
                                          choices = c("Year", "5 Years", "10 Years", "All"), selected = "All")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.property_type_selector_2 === 'Condo'",
                    tags$div(class="radio-inputs",
                             radioButtons("time_range_selector", label = "Select Time Range",
                                          choices = c("5 Years", "10 Years", "All"), selected = "All")
                    )
                  )
                ),
                column( width = 4,
                  conditionalPanel(
                    condition = "input.property_type_selector_2 === 'HDB'",
                    fluidRow(
                      column(width=10,
                        selectInput("townSelectHDB", "Select Town(s):", 
                          choices = c("All" = "All", unique(hdbdata$town)),selected = "All", multiple = TRUE),
                      ),
                      column(width=2,style="margin-top:25px; margin-left:-25px;",
                        actionButton("clearTownHDB", "Clear", style="margin-bottom:15px;")
                      )
                    ),
                    fluidRow(
                      column(width=10,
                        selectInput("storyRangeSelect", "Select Story Range(s):", 
                          choices = c("All"="All", orderedStoryRange),selected = "All", multiple = TRUE)
                      ),
                      column(width=2,style="margin-top:25px; margin-left:-25px;",
                        actionButton("clearStoryRange", "Clear")
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.property_type_selector_2 === 'Condo'",
                    selectInput("townSelectCondo", "Select Town(s):", 
                                choices = c("All" = "All", unique(condodata$district)),selected = "All", multiple = TRUE),
                    actionButton("clearTownCondo", "Clear Town Selection"),
                  )
                )
              ),
              box(width = NULL, solidHeader = TRUE,
                conditionalPanel(
                  condition = "input.property_type_selector_2 === 'HDB'",
                  plotlyOutput("hdbPricePlot")
                ),
                conditionalPanel(
                  condition = "input.property_type_selector_2 === 'Condo'",
                  plotlyOutput("condoPricePlot")
                )
              )
      ),
      # Calculator tab
      tabItem(tabName = "calculator",
              h2("Purchase/Sale Calculator"),
              p("This tab helps you calculate the balance from selling a condo and purchasing a HDB. Input the respective values on the right, and the balance will be automatically calculated and displayed on the left."),
              tags$hr(style="border-color: black;"),
              fluidRow(
                column(width=4,
                       h5(style="font-weight:bold;", "Potential Cash Proceeds from Sale of Condo"),
                       verbatimTextOutput("cash_proceeds"),
                       h5(style="font-weight:bold;","Amount Available for Purchasing a HDB"),
                       verbatimTextOutput("amount_available")
                ),
                column(width=4,
                       box(width = NULL, solidHeader = TRUE,
                         h4("Condo"),
                         numericInput("sale_price", "Condo Sale Price (SGD)", value = 0),
                         numericInput("outstanding_loan", "Outstanding Loan Amount (SGD)", value = 0),
                         numericInput("cpf_refund", "CPF Refund (SGD)", value = 0),
                         numericInput("other_fees", "Other Fees (Legal, Property Agent, Taxes, etc.) (SGD)", value = 0)
                       )
                ),
                column(width=4,
                       box(width = NULL, solidHeader = TRUE,
                         h4("HDB"),
                         numericInput("cpf_balance", "Total CPF Balance Amount (SGD)", value = 0),
                         numericInput("loan", "Loan (SGD)", value = 0),
                         numericInput("hdb_grant", "HDB Grant (SGD)", value = 0),
                         numericInput("bsd", "Buyer's Stamp Duty (BSD) (SGD)", value = 0),
                         numericInput("absd", "Additional Buyer's Stamp Duty (ABSD) (SGD)", value = 0),
                         numericInput("other_commitments", "Other Financial Commitments (SGD)", value = 0)
                       )
                )
              )
      ),
      # # Amenities Tab
      # tabItem(tabName = "amenities",
      #         radioButtons("amenityType", "Choose Amenity Type:",
      #              choices = c("All", "Bus Stops", "MRT Stops", "Malls", "Hospitals")),
      #         leafletOutput("map")
      # ),
      # Data Visualisations tab
      tabItem(tabName = "data_visualisations",
              h2("Data Visualisations"),
              p("This tab contains different analyses of property prices. Choose the analysis type to see more details."),
              tags$hr(style="border-color: black;"),
              fluidRow(
                column(width=4,
                  actionButton("HDBPriceByAreaBarButton", "HDB Prices By District & Region (Bar)", class="custom-button-1"),
                  actionButton("CondoPriceByAreaBarButton", "Condo Prices By District & Region (Bar)", class="custom-button-1"),
                  actionButton("CondoPriceByAreaBoxButton", "Condo Prices By District & Region (Box)", class="custom-button-1"),
                  actionButton("HDBPriceByDistrictBarButton", "HDB Prices By District (Bar)", class="custom-button-1"),
                  # actionButton("ResalePriceByRegionDensityButton", "Resale Prices By Region (Density)", class="custom-button-1"),
                  actionButton("RelLeaseDurResalePriceButton", "Remaining Lease Duration vs Resale Price", class="custom-button-1"),
                  actionButton("PriceByRegionAndSizeButton", "Mean Price by Region and Size", class="custom-button-1"),
                  actionButton("TransactionByYearButton", "Mean Transaction Price by Year", class="custom-button-1")
                ),
                column(width=8, box(width = NULL, solidHeader = TRUE, plotOutput("plotArea")))
              )
      ),
      # About tab
      tabItem(tabName = "about",
              h2("About"),
              p("Our app aims to revolutionize property transactions in Singapore by providing transparent and comprehensive market data. With insights into property prices and trends, buyers and sellers can make informed decisions, reducing reliance on costly intermediaries."),
              tags$hr(style="border-color: black;"),
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
      )
    )
  )
)


# rsconnect::setAccountInfo(name='jetrz', token='5AB974D84D655BB4DCB1669B981058E9', secret='wmQnFWP4AOiWzYGzs6viid8E2JVskRENTMQztoRw')
# rsconnect::deployApp(appName='DBA3702')