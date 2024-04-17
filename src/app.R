library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)

# Read data from CSV file
df_BTC <- read.csv("../data/raw/BTC.csv", stringsAsFactors = FALSE)
df_BTC$Date <- as.Date(df_BTC$Date) # Ensure 'Date' is in the Date format

# Define the UI for the application
ui <- dashboardPage(
  dashboardHeader(title = "Bitcoin Pulse"),
  dashboardSidebar(
    # Sidebar with a date range input
    dateRangeInput("dateRange", "Select Date Range:",
                   start = min(df_BTC$Date), end = max(df_BTC$Date)),
    # Sidebar with a slider input for selecting date
    sliderInput("sliderDate", "Select Date Range:",
                min = min(df_BTC$Date), max = max(df_BTC$Date), 
                value = c(min(df_BTC$Date), max(df_BTC$Date)),
                step = NULL, ticks = FALSE, animate = FALSE)
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
      box(title = "BTC Closing Prices", status = "primary", solidHeader = TRUE,
          plotlyOutput("btcPlot"), width = 12)
    )
  )
)

# Define server logic required for the dashboard
server <- function(input, output, session) {
  
  observeEvent(input$dateRange, {
    updateSliderInput(session, "sliderDate", value = c(input$dateRange[1], input$dateRange[2]))
  })
  
  observeEvent(input$sliderDate, {
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
    fig_BTC <- plot_ly(data, x=~Date, y=~Close, type='scatter', mode='lines')
    fig_BTC
  })
}

# Run the application
shinyApp(ui = ui, server = server)






