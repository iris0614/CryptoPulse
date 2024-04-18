library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)

# Read data from CSV files
df_BTC <- read.csv("../data/raw/BTC.csv", stringsAsFactors = FALSE)
df_BTC$Date <- as.Date(df_BTC$Date) # Ensure 'Date' is in the Date format
df_ETH <- read.csv("../data/raw/ETH.csv", stringsAsFactors = FALSE)
df_ETH$Date <- as.Date(df_ETH$Date) # Ensure 'Date' is in the Date format

# Define the UI for the application
ui <- dashboardPage(
  dashboardHeader(title = "CryptoPulse"),
  dashboardSidebar(
    selectInput("cryptoSelect", "Choose Cryptocurrency:", choices = c("BTC" = "Bitcoin", "ETH" = "Ethereum")),
    uiOutput("dateSliderUI")
  ),
  dashboardBody(
    tags$style(HTML("
        .box.box-solid {
            border: none; /* Remove borders */
            box-shadow: none; /* Remove shadow if any */
        }
        .box.box-solid>.box-header {
            color: #3f609e;
            font-size: 18px; /* Make the title bigger */
        }
        .box-primary>.box-header {
            background: #9896df;
        }
        .box-info>.box-header {
            background: #7c7bd4;
        }
        .box-warning>.box-header {
            background: #98BDFF;
        }
        /* Adjust the size of the number in the value box */
        .value-box .value {
            font-size: 16px; /* Make the number smaller */
            white-space: nowrap;
            overflow: hidden;
            text-overflow: ellipsis;
        }
        /* Adjust the size of the title in the value box */
        .value-box .value-title {
            font-size: 18px; /* Make the title bigger */
        }
        .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {
    background-color: #367fa9;
        }
        .small-box p {
    font-size: 25px;
}
    ")),
    fluidRow(
      valueBoxOutput("highValue", width = 4),
      valueBoxOutput("volume", width = 4),
      valueBoxOutput("priceChange", width = 4)
    ),
    fluidRow(
      box(title = "Cryptocurrency Closing Prices", status = "primary", solidHeader = TRUE,
          plotlyOutput("cryptoPlot"), width = 12)
    )
  )
)

# Define server logic required for the dashboard
server <- function(input, output, session) {
  # Update UI for date input based on cryptocurrency selection
  output$dateSliderUI <- renderUI({
    df <- if(input$cryptoSelect == "Bitcoin") df_BTC else df_ETH
    dateRangeInput("dateRange", "Select Date Range:",
                   start = min(df$Date), end = max(df$Date),
                   min = min(df$Date), max = max(df$Date))
  })
  
  # Reactive data based on selected cryptocurrency
  reactive_data <- reactive({
    df <- if(input$cryptoSelect == "Bitcoin") df_BTC else df_ETH
    df %>% filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
  })
  
  # Outputs for value boxes and plot
  output$highValue <- renderValueBox({
    data <- reactive_data()
    if(nrow(data) > 0) {
      valueBox(format(max(data$High), big.mark = ",", scientific = FALSE), "High", icon = icon("line-chart"))
    } else {
      valueBox("N/A", "High", icon = icon("line-chart"))
    }
  })
  
  output$volume <- renderValueBox({
    data <- reactive_data()
    if(nrow(data) > 0) {
      valueBox(format(mean(data$Volume), big.mark = ",", scientific = FALSE), "Volume", icon = icon("bar-chart"))
    } else {
      valueBox("N/A", "Volume", icon = icon("bar-chart"))
    }
  })
  
  output$priceChange <- renderValueBox({
    data <- reactive_data()
    if(nrow(data) > 0) {
      valueBox(format(sum(data$Close - data$Open), big.mark = ",", scientific = FALSE), "Price Change", icon = icon("dollar"))
    } else {
      valueBox("N/A", "Price Change", icon = icon("dollar"))
    }
  })
  
  output$cryptoPlot <- renderPlotly({
    data <- reactive_data()
    plot_ly(data, x = ~Date, y = ~Close, type = 'scatter', mode = 'lines', line = list(color = 'deepskyblue'))
  })
}

# Run the application
shinyApp(ui = ui, server = server)







