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


### Daily Processing ###
#################################################################### Read Files ####################################################################
ofr <- read_excel("C:/Users/slee/OneDrive - Ventura Foods/Ventura Work/SCE/Project/FY 23/OFR/Daily Updates/2023/11.08.2023/ofr.xlsx")
csv_data <- read_csv("C:/Users/slee/OneDrive - Ventura Foods/Ventura Work/SCE/Project/FY 23/OFR/Daily Updates/2023/11.08.2023/csv.csv")
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
  dplyr::select(-ref) %>% 
  dplyr::mutate(next_available_date = as.double(next_available_date),
                next_available_date = as.Date(next_available_date, origin = "1899-12-30")) -> compared_data


###################################################################################################################################################
###############################################################    Save it to DB  #################################################################
###################################################################################################################################################

# saveRDS(compared_data, "OFR_data_base.rds")
saveRDS(compared_data, "OFR_data_base_11.08.2023.rds")
ofr_data_base <- readRDS("OFR_data_base.rds")


compared_data$back_order_date <- as.Date(compared_data$back_order_date, origin = "1899-12-30")
# Now you can use rbind
ofr_data_base_2 <- rbind(ofr_data_base, compared_data)

rbind(ofr_data_base, compared_data) -> ofr_data_base_2

saveRDS(ofr_data_base_2, "OFR_data_base.rds")

file.rename(from = "OFR_data_base_11.08.2023.rds", to = "C:/Users/slee/OneDrive - Ventura Foods/Ventura Work/SCE/Project/FY 23/OFR/Daily Updates/2023/11.08.2023/OFR_data_base_11.08.2023.rds")

################### OFR_data_base.rds is the main resource for the shiny #####################

OFR_data_base %>% str()
