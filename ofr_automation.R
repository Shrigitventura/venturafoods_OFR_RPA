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
ofr <- read_excel("C:/Users/slee/OneDrive - Ventura Foods/Ventura Work/SCE/Project/FY 23/OFR/OFR Master List for Review 08.07.23.xlsx")
csv_data <- read_csv("C:/Users/slee/OneDrive - Ventura Foods/Ventura Work/SCE/Project/FY 23/OFR/OE630CR_025216_20230807_.csv")

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

# projected_to_ship_case_no in OFR should match with shipq_qty in csv file

ofr_data %>% 
  dplyr::mutate(product_label_sku = gsub("-", "", product_label_sku)) %>% 
  dplyr::mutate(ref = paste0(location, "_", legacy_sales_order, "_", product_label_sku)) %>% 
  dplyr::mutate(item_to_compare_ofr = paste0(location, "_", product_label_sku)) %>% 
  dplyr::select(ref, projected_to_ship_case_no, item_to_compare_ofr) -> ofr_data_ship_case

csv_data_compare %>% 
  dplyr::mutate(ref = paste0(order_location, "_", order_number, "_", product, label)) %>%
  dplyr::mutate(item_to_compare_csv = paste0(order_location, "_", product, label)) %>% 
  dplyr::left_join(ofr_data_ship_case) %>%
  dplyr::mutate(match = ifelse(shipq_qty == projected_to_ship_case_no, "matching", "not_matching")) %>% 
  dplyr::relocate(shipq_qty, .before = projected_to_ship_case_no) %>% 
  dplyr::select(-ref, -item_to_compare_csv) -> compared_data





###################################################################################################################################################
####################################################      export to your directory    #############################################################
###################################################################################################################################################

writexl::write_xlsx(compared_data, "C:/Users/slee/OneDrive - Ventura Foods/Ventura Work/SCE/Project/FY 23/OFR/compared_data.xlsx")

