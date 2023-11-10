library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)


# Load data
data <- readRDS("OFR_data_base.rds")



# Define UI
ui <- navbarPage(
  theme = shinythemes::shinytheme("flatly"),
  title = "Order Fulfillment Report (OFR)",  # This sets the app title
  tabPanel("Data View", # This is the first page
           tags$head(
             tags$style(HTML("
               #logo {
                 float: right;
                 height: 60px; /* You can adjust the size of the logo here */
               }
             "))
           ),
           titlePanel(div(img(src = "VenturaFoodsLogo.png", id="logo"), "Order Fulfillment Report (OFR)")),
           fluidRow(
             column(4, dateRangeInput("dateRange", "Shortage Date Range:",
                                      start = min(data$ShortageDate),
                                      end = max(data$ShortageDate))),
             column(8, DTOutput("datatable"))
           )
  )
)

server <- function(input, output) {
  
  # Reactive expression to read and preprocess data
  reactive_data <- reactive({
    data <- readRDS("OFR_data_base.rds")
    data <- data %>%
      mutate(ShortageDate = as.Date(shortage_date, format="%Y-%m-%dT%H:%M:%SZ")) %>%
      dplyr::rename(
        Location = location,
        LegacySalesOrder = legacy_sales_order,
        ShortageDate = shortage_date
      )
    # Return the processed data
    data
  })
  
  # Reactive data that responds to the date range filter
  filtered_data <- reactive({
    # Access the reactive data
    df <- reactive_data()
    # Apply the filter if dateRange is not null
    if (!is.null(input$dateRange)) {
      df <- df %>% 
        filter(ShortageDate >= input$dateRange[1] & ShortageDate <= input$dateRange[2])
    }
    # Return the filtered or original data
    df
  })
  
  # Render datatable
  output$datatable <- renderDT({
    datatable(filtered_data(),
              extensions = "Buttons",
              options = list(
                pageLength = 100,
                scrollX = TRUE,
                dom = 'Blfrtip',
                buttons = c('copy', 'csv', 'excel'),
                fixedColumns = list(leftColumns = 2)
              ))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
