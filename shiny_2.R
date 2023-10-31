# Required Libraries
library(shiny)
library(shinythemes)
library(readxl)
library(readr)
library(dplyr)
library(janitor)
library(writexl)

#
# UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),  # Bootstrap theme
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")  # Optional custom CSS
  ),
  navbarPage("Ventura Foods OFR Process Automation App",
             tabPanel("Home",
                      div(id = "loading-content",
                          tags$img(src = 'VenturaFoodsLogo.png', height = 72, width = 250),  # Adjust size as needed
                          fileInput('ofr_file', 'Choose OFR Excel File'),
                          fileInput('csv_data_file', 'Choose CSV Data File'),
                          downloadButton('downloadData', 'Download'),
                          tags$p("For inquiries, feel free to reach out to ", 
                                 tags$a(href = "mailto:slee@venturafoods.com", "Stan Lee (slee@venturafoods.com)")),
                          shiny::tags$script("$('#loading-content').shinyjs.pageLoading = function(){};")  # Optional loading animation
                      )
             )
  )
)

# Server
server <- function(input, output) {
  output$downloadData <- downloadHandler(
    filename = function() { paste('compared_data.xlsx') },
    content = function(file) {
      ofr <- read_excel(input$ofr_file$datapath)
      csv_data <- read_csv(input$csv_data_file$datapath)
      
      ofr %>% 
        data.frame() %>% 
        janitor::clean_names() -> ofr_data
      
      csv_data[-1:-2, ] -> csv_data
      
      csv_data %>% 
        data.frame() %>% 
        janitor::clean_names() -> csv_data_compare
      
      csv_data_compare %>% 
        dplyr::mutate(ref = paste0(order_location, "_", order_number, "_", product, label)) %>%
        dplyr::mutate(item_to_compare_csv = paste0(order_location, "_", product, label)) %>% 
        dplyr::select(item_to_compare_csv, ref, shipq_qty) -> csv_data_compare
      
      ofr_data %>% 
        dplyr::mutate(product_label_sku = gsub("-", "", product_label_sku)) %>% 
        dplyr::mutate(ref = paste0(location, "_", legacy_sales_order, "_", product_label_sku)) %>% 
        dplyr::mutate(item_to_compare_ofr = paste0(location, "_", product_label_sku)) %>% 
        dplyr::left_join(csv_data_compare) %>% 
        dplyr::mutate(match = ifelse(shipq_qty == projected_to_ship_case_no, "matching", "not_matching")) %>% 
        dplyr::filter(!is.na(item_to_compare_csv)) %>% 
        dplyr::relocate(projected_to_ship_case_no, .before = shipq_qty) %>% 
        dplyr::rename("shipped qty(OFR)" = projected_to_ship_case_no,
                      "shipped qty(CSV)" = shipq_qty) %>% 
        dplyr::select(-ref)-> compared_data
      
      
      writexl::write_xlsx(compared_data, file)
    }
  )
}

# Run the Shiny App
shinyApp(ui, server)
