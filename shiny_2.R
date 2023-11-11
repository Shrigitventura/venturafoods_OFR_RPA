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
                Product = product_label_sku,
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
ui <- navbarPage("Order Fulfillment Report (OFR)", 
                 theme = shinythemes::shinytheme("flatly"),
                 tabPanel("Main Page", 
                          fluidPage(
                            titlePanel(
                              div(class = "row", 
                                  div(class = "col-sm-8",
                                      "Order Fulfillment Report (OFR)"), 
                                  div(class = "col-sm-4",
                                      img(src = "VenturaFoodsLogo.png", height = "60px", align = "right"))
                              )
                            ),
                            tags$head(tags$link(rel = "shortcut icon", href = "www/VenturaFoodsLogo.png")),

                            dateRangeInput("dateRange",
                                           label = "Select Date Range",
                                           start = min(data$Shortage_Date),
                                           end = max(data$Shortage_Date)),
                            
                            selectInput("matchFilter", 
                                        label = "Filter by Match",
                                        choices = unique(data$Match),
                                        selected = "Not Matching", 
                                        multiple = TRUE),
                            
                            DTOutput("datatable")
                          )
                 ),
                 
                 tabPanel("Not matchings", 
                          tabsetPanel(
                            tabPanel("By Profile Owner",
                                   fluidPage(
                                     dateRangeInput("dateRangeTab1",
                                                    label = "Select Date Range",
                                                    start = min(data$Shortage_Date),
                                                    end = max(data$Shortage_Date)),
                                    splitLayout( 
                                     DTOutput("pivotTableByProfileOwner"),
                                     plotOutput("plotByProfileOwner")
                            )),
                          ),
                          tabPanel("By Product",
                                   fluidPage(
                                     dateRangeInput("dateRangeTab2",
                                                    label = "Select Date Range",
                                                    start = min(data$Shortage_Date),
                                                    end = max(data$Shortage_Date)),
                                     splitLayout(
                                       DTOutput("pivotTableByProduct"),
                                       plotOutput("plotByProduct")
                                     )
                                   ))
                 )

))



# Define server logic
server <- function(input, output) {

  output$datatable <- renderDT({

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
                    fixedColumns = list(leftColumns = 2)),
                  rownames = FALSE)
  })
  
  summarizedData <- reactive({
    data %>%
      filter(Match == "Not Matching") %>%
      filter(Shortage_Date >= input$dateRangeTab1[1] & Shortage_Date <= input$dateRangeTab1[2]) %>%
      group_by(`Customer Profile Owner`) %>%
      summarize(Count = n()) %>%
      ungroup()
  })
  
  output$pivotTableByProfileOwner <- renderDT({
    DT::datatable(summarizedData(),
                  options = list(
                    pageLength = 100,
                    scrollX = TRUE,
                    dom = 'Blfrtip',
                    buttons = c('copy', 'csv', 'excel'),
                    fixedColumns = list(leftColumns = 2)),
                  rownames = FALSE)
  })
  
  output$plotByProfileOwner <- renderPlot({
    summarized_plot_data <- summarizedData() %>%
      arrange(desc(Count)) 
    
    ggplot(summarized_plot_data, aes(x = reorder(`Customer Profile Owner`, -Count), y = Count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = Count), vjust = -0.3, fontface = "bold") + 
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 12), 
            axis.text.y = element_text(face = "bold", size = 12)) +
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank())
  })
  
  
  summarizedData_2 <- reactive({
    data %>%
      filter(Match == "Not Matching") %>%
      filter(Shortage_Date >= input$dateRangeTab2[1] & Shortage_Date <= input$dateRangeTab2[2]) %>%
      group_by(Product) %>%
      summarize(Count = n()) %>%
      ungroup()
  })
  
  
  output$pivotTableByProduct <- renderDT({
    DT::datatable(summarizedData_2(),
                  options = list(
                    pageLength = 100,
                    scrollX = TRUE,
                    dom = 'Blfrtip',
                    buttons = c('copy', 'csv', 'excel'),
                    fixedColumns = list(leftColumns = 2)),
                  rownames = FALSE)
  })
  
  output$plotByProduct <- renderPlot({
    summarized_plot_data_2 <- summarizedData_2() %>%
      arrange(desc(Count)) 
    
    ggplot(summarized_plot_data_2, aes(x = reorder(Product, -Count), y = Count)) +
      geom_bar(stat = "identity", fill = "darkgoldenrod") +
      geom_text(aes(label = Count), vjust = -0.3, fontface = "bold") + 
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 12), 
            axis.text.y = element_text(face = "bold", size = 12)) +
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank())
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)