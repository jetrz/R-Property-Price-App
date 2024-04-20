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
  dashboardHeader(title = "Property Insight SG"),
  dashboardSidebar(
    tags$head(includeCSS(("styles.css"))),
    sidebarMenu(
      menuItem("District Info", tabName = "district_info"),
      menuItem("Town Info", tabName = "town_info"),
      menuItem("Purchase/Sale Calculator", tabName = "calculator"),
      # menuItem("Amenities", tabName = "amenities"),
      menuItem("Data Visualisations", tabName = "data_visualisations"),  # New tab for data visualizations
      menuItem("About", tabName = "about"),
      tags$div(class="district_reference",
        p("District/Town Reference", style="font-weight:bold; font-size:17px; text-decoration:underline;"),
        p("District 1: Raffles Place, Cecil, Marina, People's Park"),
        p("District 2: Anson, Tanjong Pagar"),
        p("District 3: Queenstown, Tiong Bahru"),
        p("District 4: Telok Blangah, Harbourfront"),
        p("District 5: Pasir Panjang, Hong Leong Garden, Clementi New Town"),
        p("District 6: High Street, Beach Road (part)"),
        p("District 7: Middle Road, Golden Mile"),
        p("District 8: Little India"),
        p("District 9: Orchard, Cairnhill, River Valley"),
        p("District 10: Ardmore, Bukit Timah, Holland Road, Tanglin"),
        p("District 11: Watten Estate, Novena, Thomson"),
        p("District 12: Balestier, Toa Payoh, Serangoon"),
        p("District 13: Macpherson, Braddell"),
        p("District 14: Geylang, Eunos"),
        p("District 15: Katong, Joo Chiat, Amber Road"),
        p("District 16: Bedok, Upper East Coast, Eastwood, Kew Drive"),
        p("District 17: Loyang, Changi"),
        p("District 18: Tampines, Pasir Ris"),
        p("District 19: Serangoon Garden, Hougang, Punggol"),
        p("District 20: Bishan, Ang Mo Kio"),
        p("District 21: Upper Bukit Timah, Clementi Park, Ulu Pandan"),
        p("District 22: Jurong"),
        p("District 23: Hillview, Dairy Farm, Bukit Panjang, Choa Chu Kang"),
        p("District 24: Lim Chu Kang, Tengah"),
        p("District 25: Kranji, Woodgrove"),
        p("District 26: Upper Thomson, Springleaf"),
        p("District 27: Yishun, Sembawang"),
        p("District 28: Seletar")
      )
    )
  ),
  dashboardBody(
    # District Info tab
    tabItems(
      tabItem(tabName = "district_info",
              h2("Property Info By Districts"),
              p("This tab shows information about property prices across districts."),
              tags$span(style="font-weight:bold;", "To View Price Charts within a Region"),
              tags$br(),
              tags$span("- Select Property Type: In the dashboard, choose the property type (e.g., HDB or condominium) you're interested in."),
              tags$br(),
              tags$span("- Access Price Charts: Go to the Price Charts section and select the region you want to explore."),
              tags$br(),
              tags$span("- Analyze Boxplot Chart: Once you've chosen a region, you'll see a boxplot chart displaying mean property prices alongside quartile ranges. Review the chart to understand average prices and price distribution in the area."),
              tags$br(),
              tags$span("- Explore Time Trend Chart: Below the boxplot chart, find the time trend chart. This chart illustrates the latest year records of property prices, allowing you to observe market trends and patterns over time."),
              tags$br(),tags$br(),
              tags$span(style="font-weight:bold;", "To View Nearby Amenities"),
              tags$br(),
              tags$span("- Select Amenities: Click on the radio button next to the amenity you want to view (e.g., Hospitals, Bus Stops, MRT Stations, Shopping Malls)."),
              tags$br(),
              tags$span("- View on Map: The selected amenities will appear on the map below for easy reference."),
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
              tags$span("- Select Property Type: In the dashboard, choose the property type (e.g., HDB or condominium) you're interested in."),
              tags$br(),
              tags$span("- Choose Time Range: Select the time range you want to analyze (1 Year, 5 Years, 10 Years, All Time)."),
              tags$br(),
              tags$span("- Choose Towns: Select the towns you're interested in exploring."),
              tags$br(),
              tags$span("- Select Story Range: Specify the story range (Only applicable for HDB)."),
              tags$br(),
              tags$span("- View Resale Price Distribution: After applying your filters, a chart will display the resale price distribution, showing a box plot of prices within the selected towns and filters added."),
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
      # Data Visualisations tab
      tabItem(tabName = "data_visualisations",
              h2("Data Visualisations"),
              p("This tab contains different analyses of property prices. Choose the analysis type to see more details."),
              tags$hr(style="border-color: black;"),
              fluidRow(
                column(width=4,
                  actionButton("HDBPriceByAreaBarButton", "HDB Prices By District & Region", class="custom-button-1"),
                  actionButton("CondoPriceByAreaBarButton", "Condo Prices By District & Region (Bar)", class="custom-button-1"),
                  actionButton("CondoPriceByAreaBoxButton", "Condo Prices By District & Region (Box)", class="custom-button-1"),
                  actionButton("HDBPriceByDistrictBarButton", "HDB vs Condo By Region", class="custom-button-1"),
                  actionButton("RelLeaseDurResalePriceButton", "Remaining Lease Duration vs Resale Price", class="custom-button-1"),
                  actionButton("PriceByRegionAndSizeButton", "Mean Price by Region and Size", class="custom-button-1"),
                  actionButton("TransactionByYearButton", "Mean Transaction Price by Year", class="custom-button-1")
                ),
                column(width=8, 
                  box(width = NULL, solidHeader = TRUE, 
                    uiOutput("graph_desc"),
                    plotOutput("plotArea")
                  )
                )
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
