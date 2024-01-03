library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(lubridate)
library(shinyWidgets)

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
                Match = match) %>% 
  dplyr::mutate(Month = lubridate::month(Shortage_Date),
                Year = lubridate::year(Shortage_Date),
                Year_Month = paste0(Year, "/", Month))-> data

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
                            
                            DTOutput("datatable"),
                            downloadButton("downloadData", "Download Full Data")
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
                                       ))
                            ),
                            tabPanel("By Profile Owner (Monthly View)",
                                     fluidPage(
                                       pickerInput("monthYearFilter", "Select Month/Year",
                                                   choices = sort(unique(data$Year_Month)),  
                                                   selected = unique(data$Year_Month),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE),
                                       pickerInput("profileOwnerFilter", "Filter by Customer Profile Owner",
                                                   choices = sort(unique(data$`Customer Profile Owner`)), 
                                                   selected = unique(data$`Customer Profile Owner`),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE),
                                       fluidRow(
                                         column(6, DTOutput("pivotTableByProfileOwnerMonthly"),
                                                br(),
                                                br(),
                                                br(),
                                                DTOutput("pivotTableByProfileOwnerMonthly_2")),
                                         column(6, plotOutput("plotByProfileOwnerMonthly"))
                                       )
                                     )
                            ),
                            tabPanel("By Ship Location",
                                     fluidPage(
                                       dateRangeInput("dateRangeTab3",
                                                      label = "Select Date Range",
                                                      start = min(data$Shortage_Date),
                                                      end = max(data$Shortage_Date)),
                                       splitLayout(
                                         DTOutput("pivotTableByLocation"),
                                         plotOutput("plotByLocation")
                                       ))
                            ),
                            tabPanel("By Ship Location (Monthly View)",
                                     fluidPage(
                                       pickerInput("monthYearFilter_2", "Select Month/Year",
                                                   choices = sort(unique(data$Year_Month)), 
                                                   selected = unique(data$Year_Month),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE),
                                       pickerInput("shipLocationFilter", "Filter by Ship Location",
                                                   choices = sort(unique(data$Location)),  
                                                   selected = unique(data$Location),
                                                   options = list(`actions-box` = TRUE),
                                                   multiple = TRUE),
                                       fluidRow(
                                         column(6, DTOutput("pivotTableByShipLocationMonthly"),
                                                br(),
                                                br(),
                                                br(),
                                                DTOutput("pivotTableByShipLocationMonthly_2")),
                                         column(6, plotOutput("plotByShipLocationMonthly"))
                                       )
                                     )
                            )
                          )
                 )
)





# Define server logic
server <- function(input, output) {
  
  output$datatable <- renderDT({
    
    filtered_data <- data %>%
      filter(Shortage_Date >= input$dateRange[1] & Shortage_Date <= input$dateRange[2]) %>%
      filter(Match %in% input$matchFilter)
    
    DT::datatable(filtered_data,
                  extensions = c('Buttons', 'FixedHeader'), 
                  options = list(
                    pageLength = 100,
                    scrollX = TRUE,
                    scrollY = "500px", 
                    dom = 'Blfrtip',
                    buttons = c('copy', 'csv', 'excel'),
                    fixedHeader = TRUE, 
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
    
    ggplot(summarized_plot_data, aes(x = reorder(`Customer Profile Owner`, Count), y = Count)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = Count), hjust = 1.3, vjust = 0.5, fontface = "bold", color = "white") + 
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 12), 
            axis.text.y = element_text(face = "bold", size = 12)) +
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank()) +
      coord_flip()
  })
  
  
  summarizedData_2 <- reactive({
    filtered_data <- data %>%
      filter(Match == "Not Matching") %>%
      mutate(Month = lubridate::month(Shortage_Date),
             Year = lubridate::year(Shortage_Date),
             Year_Month = paste0(Year, "/", Month)) %>%
      filter(Year_Month %in% input$monthYearFilter) %>%
      filter(`Customer Profile Owner` %in% input$profileOwnerFilter)
    
    filtered_data %>%  
      group_by(Year_Month, `Customer Profile Owner`) %>%
      summarize(Count = n()) %>%
      ungroup()
  })
  
  summarizedData_2_2 <- reactive({
    filtered_data <- data %>%
      filter(Match == "Not Matching") %>%
      mutate(Month = lubridate::month(Shortage_Date),
             Year = lubridate::year(Shortage_Date),
             Year_Month = paste0(Year, "/", Month)) %>%
      filter(Year_Month %in% input$monthYearFilter) %>%
      filter(`Customer Profile Owner` %in% input$profileOwnerFilter)
    
    filtered_data %>%  
      group_by(Year_Month) %>%
      summarize(Count = n()) %>%
      ungroup()
  })
  
  
  output$pivotTableByProfileOwnerMonthly <- renderDT({
    DT::datatable(summarizedData_2(),
                  extensions = c('FixedHeader'), 
                  options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    scrollY = "300px", 
                    fixedHeader = TRUE, 
                    fixedColumns = list(leftColumns = 2)),
                  rownames = FALSE)
  })
  
  output$pivotTableByProfileOwnerMonthly_2 <- renderDT({
    DT::datatable(summarizedData_2_2(),
                  extensions = c('FixedHeader'), 
                  options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    scrollY = "300px", 
                    fixedHeader = TRUE, 
                    fixedColumns = list(leftColumns = 2)),
                  rownames = FALSE)
  })
  
  output$plotByProfileOwnerMonthly <- renderPlot({
    summarized_plot_data_2 <- summarizedData_2_2() %>%
      arrange(desc(Count))
    
    ggplot(summarized_plot_data_2, aes(x = Year_Month, y = Count)) +
      geom_bar(stat = "identity", fill = "#003f5c") +
      geom_text(aes(label = Count), vjust = -0.5, color = "#003f5c", size = 5, fontface = "bold") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 12),
            axis.text.y = element_text(face = "bold", size = 12)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
  })
  
  
  
  
  
  
  summarizedData_3 <- reactive({
    data %>%
      filter(Match == "Not Matching") %>%
      filter(Shortage_Date >= input$dateRangeTab3[1] & Shortage_Date <= input$dateRangeTab3[2]) %>%
      group_by(Location) %>%
      summarize(Count = n()) %>%
      ungroup()
  })
  
  output$pivotTableByLocation <- renderDT({
    DT::datatable(summarizedData_3(),
                  options = list(
                    pageLength = 100,
                    scrollX = TRUE,
                    dom = 'Blfrtip',
                    buttons = c('copy', 'csv', 'excel'),
                    fixedColumns = list(leftColumns = 2)),
                  rownames = FALSE)
  })
  
  
  output$plotByLocation <- renderPlot({
    summarized_plot_data_3 <- summarizedData_3() %>%
      arrange(desc(Count)) 
    
    ggplot(summarized_plot_data_3, aes(x = reorder(Location, Count), y = Count)) +
      geom_bar(stat = "identity", fill = "chocolate3") +
      geom_text(aes(label = Count), hjust = 1.3, vjust = 0.5, fontface = "bold", color = "white") + 
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 12), 
            axis.text.y = element_text(face = "bold", size = 12)) +
      theme(axis.title.x = element_blank(), 
            axis.title.y = element_blank()) +
      coord_flip()
  })
  
  
  
  summarizedData_4 <- reactive({
    filtered_data <- data %>%
      filter(Match == "Not Matching") %>%
      mutate(Month = lubridate::month(Shortage_Date),
             Year = lubridate::year(Shortage_Date),
             Year_Month = paste0(Year, "/", Month)) %>%
      filter(Year_Month %in% input$monthYearFilter) %>%
      filter(Location %in% input$shipLocationFilter)
    
    filtered_data %>%  
      group_by(Year_Month, Location) %>%
      summarize(Count = n()) %>%
      ungroup()
  })
  
  summarizedData_4_2 <- reactive({
    filtered_data <- data %>%
      filter(Match == "Not Matching") %>%
      mutate(Month = lubridate::month(Shortage_Date),
             Year = lubridate::year(Shortage_Date),
             Year_Month = paste0(Year, "/", Month)) %>%
      filter(Year_Month %in% input$monthYearFilter_2) %>%
      filter(Location %in% input$shipLocationFilter)
    
    filtered_data %>%  
      group_by(Year_Month) %>%
      summarize(Count = n()) %>%
      ungroup()
  })
  
  
  output$pivotTableByShipLocationMonthly <- renderDT({
    DT::datatable(summarizedData_4(),
                  extensions = c('FixedHeader'), 
                  options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    scrollY = "300px", 
                    fixedHeader = TRUE, 
                    fixedColumns = list(leftColumns = 2)),
                  rownames = FALSE)
  })
  
  output$pivotTableByShipLocationMonthly_2 <- renderDT({
    DT::datatable(summarizedData_4_2(),
                  extensions = c('FixedHeader'), 
                  options = list(
                    pageLength = 10,
                    scrollX = TRUE,
                    scrollY = "300px", 
                    fixedHeader = TRUE, 
                    fixedColumns = list(leftColumns = 2)),
                  rownames = FALSE)
  })
  
  output$plotByShipLocationMonthly <- renderPlot({
    summarized_plot_data_4 <- summarizedData_4_2() %>%
      arrange(desc(Count))
    
    ggplot(summarized_plot_data_4, aes(x = Year_Month, y = Count)) +
      geom_bar(stat = "identity", fill = "#003f5c") +
      geom_text(aes(label = Count), vjust = -0.5, color = "#003f5c", size = 5, fontface = "bold") +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 12),
            axis.text.y = element_text(face = "bold", size = 12)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank())
  })
  
  
  
  
  # Reactive expression for filtered data
  filteredData <- reactive({
    data %>% 
      filter(Shortage_Date >= input$dateRange[1] & Shortage_Date <= input$dateRange[2]) %>%
      filter(Match %in% input$matchFilter)
  })
  
  # Download handler for filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)