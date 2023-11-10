library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(lubridate)

# Load data
data <- readRDS("OFR_data_base.rds")

data %>% 
  dplyr::mutate(shortage_date = ymd(shortage_date),
                sales_order_date = ymd(sales_order_date),
                back_order_date = ymd(back_order_date)) %>% 
  dplyr::mutate(match = gsub("not_matching", "Not Matching", match),
                match = gsub("matching", "Matching", match)) %>% 
  dplyr::rename(Shortage_Date = shortage_date,
                Location = location, 
                "Legacy Sales Order" = legacy_sales_order,
                "Sales Order Date" = sales_order_date,
                "JDE Sales Order" = jde_sales_order,
                "Back Order Date" = back_order_date,
                "Reference Order Date" = reference_order_date,
                "Customer PO" = customer_po,
                "Customer Ship To Number" = customer_ship_to_8,
                "Customer Ship To Name" = customer_ship_to_9,
                "Make Buy Transfer" = make_buy_transfer,
                "Label Owner" = label_owner,
                "Priority SKU" = priority_sku,
                Item = product_label_sku,
                Description = description,
                "Customer Profile Owner" = customer_profile_owner,
                "Shortage Reason" = shortage_reason,
                "Next Available Date" = next_available_date,
                "Followup Comments" = followup_comments,
                "Type of Sale Number" = type_of_sale,
                "Type of Sale Name" = type_of_sale_2,
                "Total SKU Beginning Inventory" = total_sku_beginning_inventory,
                "Open Order Cases" = oo_cases,
                "Branch Transfer Open Order Cases" = b_t_open_order_cases,
                "Order Shortage Case" = order_shortage_case_no,
                "Total SKU Shortage Qty" = total_sku_shortage_qty,
                "Inventory Soft Hold Release" = inventory_soft_hold_release,
                "Useable Inventory" = inventory_usable,
                "Production Schedule" = production_schedule,
                "Production Soft Hold Release" = production_soft_hold_release,
                "Purchase Order & Transfer In" = purchase_order_and_transfer_in,
                "Sales Order & Transfer Out" = sales_order_and_transfer_out,
                "Item to Compare (OFR)" = item_to_compare_ofr,
                "Item to Compare (CSV)" = item_to_compare_csv,
                "Shipped Qty (OFR)" = "shipped qty(OFR)",
                "Shipped Qty (CSV)" = "shipped qty(CSV)",
                Match = match) -> data

# Define UI
ui <- navbarPage("Order Fulfillment Report (OFR)", # Name of the app
                 theme = shinythemes::shinytheme("flatly"),
                 tabPanel("Main Page", 
                          fluidPage(
                            titlePanel(
                              div(class = "row", 
                                  div(class = "col-sm-8",
                                      "Order Fulfillment Report (OFR)"), # Left side: title
                                  div(class = "col-sm-4",
                                      img(src = "VenturaFoodsLogo.png", height = "60px", align = "right")) # Right side: logo
                              )
                            ),
                            tags$head(tags$link(rel = "shortcut icon", href = "www/VenturaFoodsLogo.png")),
                            
                            # Date range input
                            dateRangeInput("dateRange",
                                           label = "Select Date Range",
                                           start = min(data$Shortage_Date),
                                           end = max(data$Shortage_Date)),
                            
                            selectInput("matchFilter", 
                                        label = "Filter by Match",
                                        choices = unique(data$Match),
                                        selected = "Not Matching", # Set default selection
                                        multiple = TRUE),
                            
                            DTOutput("datatable")
                          )
                 )
                 # You can add more tabPanel() here for additional pages
)


# Define server logic
server <- function(input, output) {
  
  # Render datatable
  output$datatable <- renderDT({
    # Apply both filters sequentially
    filtered_data <- data %>%
      filter(Shortage_Date >= input$dateRange[1] & Shortage_Date <= input$dateRange[2]) %>%
      filter(Match %in% input$matchFilter)
    
    DT::datatable(filtered_data,
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