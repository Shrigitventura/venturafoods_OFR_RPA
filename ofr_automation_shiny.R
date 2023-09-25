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
        janitor::clean_names() -> ofr_data
      
      csv_data[-1:-2, ] -> csv_data
      csv_data %>% 
        janitor::clean_names() -> csv_data_compare
      
      ofr_data %>% 
        mutate(product_label_sku = gsub("-", "", product_label_sku),
               ref = paste0(location, "_", legacy_sales_order, "_", product_label_sku),
               item_to_compare_ofr = paste0(location, "_", product_label_sku)) %>%
        select(ref, projected_to_ship_case_no, item_to_compare_ofr) -> ofr_data_ship_case
      
      csv_data_compare %>% 
        mutate(ref = paste0(order_location, "_", order_number, "_", product, label),
               item_to_compare_csv = paste0(order_location, "_", product, label)) %>% 
        left_join(ofr_data_ship_case, by = "ref") %>%
        mutate(match = ifelse(shipq_qty == projected_to_ship_case_no, "matching", "not_matching")) %>%
        relocate(shipq_qty, .before = projected_to_ship_case_no) %>% 
        select(-ref, -item_to_compare_csv) %>% 
        mutate(projected_to_ship_case_no = ifelse(is.na(projected_to_ship_case_no), "Not in OFR", projected_to_ship_case_no),
               item_to_compare_ofr = ifelse(is.na(item_to_compare_ofr), "Not in OFR", item_to_compare_ofr),
               match = ifelse(is.na(match), "Not in OFR", match)) -> compared_data
      
      writexl::write_xlsx(compared_data, file)
    }
  )
}

# Run the Shiny App
shinyApp(ui, server)
