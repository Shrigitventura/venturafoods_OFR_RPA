library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)


# Load data
data <- readRDS("OFR_data_base.rds")

# Define UI
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  titlePanel("Data Display", windowTitle = "Data Overview"),
  tags$head(tags$link(rel = "shortcut icon", href = "www/VenturaFoodsLogo.png")),
  DTOutput("datatable")
)

# Define server logic
server <- function(input, output) {
  
  # Rename - replace with actual column names
  renamed_data <- data %>%
    dplyr::rename(
      Location = location,
      "Legacy Sales Order" = legacy_sales_order,
    )
  
  # Render datatable
  output$datatable <- renderDT({
    DT::datatable(renamed_data,
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
