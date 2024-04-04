library(shiny)

# Define UI for application
shinyUI(navbarPage(title = "Basic Shiny App",
                   tabPanel("Main",
                            fluidPage(
                              h1("Main Page"),
                              p("This is the content of the main page.")
                            )),
                   tabPanel("Analysis",
                            fluidPage(
                              h1("Analysis Page"),
                              p("Click the button to display the HDB Mean Price by Area graph."),
                              actionButton("plotButton", "Plot Graph"),
                              plotOutput("plotArea")
                            ))
))


# rsconnect::setAccountInfo(name='jetrz', token='5AB974D84D655BB4DCB1669B981058E9', secret='wmQnFWP4AOiWzYGzs6viid8E2JVskRENTMQztoRw')
# rsconnect::deployApp(appName='DBA3711')