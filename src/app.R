library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)

# Read data from CSV file
df_BTC <- read.csv("../data/raw/BTC.csv", stringsAsFactors = FALSE)
df_BTC$Date <- as.Date(df_BTC$Date) # Ensure 'Date' is in the Date format

# Define the UI for the application
ui <- dashboardPage(
  dashboardHeader(title = "Bitcoin Dashboard"),
  dashboardSidebar(
    # Sidebar with a date range input
    dateRangeInput("dateRange", "Select Date Range:",
                   start = min(df_BTC$Date), end = max(df_BTC$Date)),
    # Sidebar with a slider input for selecting date
    sliderInput("sliderDate", "Select Date:",
                min = min(df_BTC$Date), max = max(df_BTC$Date), value = c(min(df_BTC$Date), max(df_BTC$Date)))
  ),
  dashboardBody(
    tags$style(HTML("
      .box.box-solid>.box-header {
        color: #F3797E;
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
    ")),
    fluidRow(
      box(width = 12, title = "High Value", status = "primary", solidHeader = TRUE, 
          valueBoxOutput("highValue")),
      box(width = 12, title = "Volume", status = "info", solidHeader = TRUE, 
          valueBoxOutput("volume")),
      box(width = 12, title = "Price Change Per Day", status = "warning", solidHeader = TRUE, 
          valueBoxOutput("priceChange")),
      box(title = "BTC Closing Prices", status = "primary", solidHeader = TRUE,
          plotlyOutput("btcPlot"), width = 12) 
    )
  )
)

# Define server logic required for the dashboard
server <- function(input, output, session) {
  
  observe({
    updateSliderInput(session, "sliderDate", value = c(input$dateRange[1], input$dateRange[2]))
  })
  
  observe({
    updateDateRangeInput(session, "dateRange", start = input$sliderDate[1], end = input$sliderDate[2])
  })
  
  # Filter data based on selected date from the inputs
  reactive_data <- reactive({
    df_BTC %>% 
      filter(Date >= as.Date(input$sliderDate[1]) & Date <= as.Date(input$sliderDate[2]))
  })
  
  # Output for High Value
  output$highValue <- renderValueBox({
    data <- reactive_data()
    if(nrow(data) > 0) {
      valueBox(
        format(max(data$High), big.mark = ",", scientific = FALSE),
        "High", icon = icon("line-chart")
      )
    } else {
      valueBox("N/A", "High", icon = icon("line-chart"))
    }
  })
  
  # Output for Volume
  output$volume <- renderValueBox({
    data <- reactive_data()
    if(nrow(data) > 0) {
      valueBox(
        format(sum(data$Volume), big.mark = ",", scientific = FALSE),
        "Volume", icon = icon("bar-chart")
      )
    } else {
      valueBox("N/A", "Volume", icon = icon("bar-chart"))
    }
  })
  
  # Output for Price Change Per Day
  output$priceChange <- renderValueBox({
    data <- reactive_data()
    if(nrow(data) > 0) {
      valueBox(
        format(sum(data$Close - data$Open), big.mark = ",", scientific = FALSE),
        "Price Change Per Day", icon = icon("dollar")
      )
    } else {
      valueBox("N/A", "Price Change Per Day", icon = icon("dollar"))
    }
  })
  
  # Output for the BTC closing prices plot
  output$btcPlot <- renderPlotly({
    # Use the reactive_data() that filters based on the inputs
    data <- reactive_data()
    fig_BTC <- plot_ly(data, x=~Date, y=~Close, mode='lines')
    fig_BTC
  })
}

# Run the application
shinyApp(ui = ui, server = server)






