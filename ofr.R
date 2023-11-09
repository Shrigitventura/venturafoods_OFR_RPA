library(tidyverse)
library(magrittr)
library(openxlsx)
library(readxl)
library(writexl)
library(reshape2)
library(skimr)
library(janitor)
library(lubridate)
library(rio)

#################################################################### Read Files ####################################################################
ofr <- read_excel(input$ofr_file$datapath)
csv_data <- read_csv(input$csv_data_file$datapath)

####################################################################################################################################################

# Clean Data
ofr %>% 
  data.frame() %>% 
  janitor::clean_names() -> ofr_data

csv_data[-1:-2, ] -> csv_data

csv_data %>% 
  data.frame() %>% 
  janitor::clean_names() -> csv_data_compare

#######################################
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


###################################################################################################################################################
####################################################      export to your directory    #############################################################
###################################################################################################################################################




