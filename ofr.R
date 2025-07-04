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


specific_date <- as.Date("2025-06-20")

### Daily Processing ###


#Shri local folder locations
ofr <- read_excel("C:/Users/SPoudel/Ventura Foods/SC Analytics Team - General/Stan Report Files/OFR/Daily Updates/2025/06.20.2025/ofr.xlsx")
csv_data <- read_csv("C:/Users/SPoudel/Ventura Foods/SC Analytics Team - General/Stan Report Files/OFR/Daily Updates/2025/06.20.2025/csv.csv")

####################################################################################################################################################

# Clean Data
ofr %>% 
  data.frame() %>% 
  janitor::clean_names() %>% 
  select(1:33) -> ofr_data

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
  dplyr::mutate(ref = paste0(short_location, "_", legacy_sales_order, "_", product_label_sku)) %>% 
  dplyr::mutate(item_to_compare_ofr = paste0(short_location, "_", product_label_sku)) %>% 
  dplyr::left_join(csv_data_compare) %>% 
  dplyr::mutate(match = ifelse(shipq_qty == projected_to_ship_qty_case_no, "matching", "not_matching")) %>% 
  dplyr::filter(!is.na(item_to_compare_csv)) %>% 
  dplyr::relocate(projected_to_ship_qty_case_no, .before = shipq_qty) %>% 
  dplyr::rename("shipped qty(OFR)" = projected_to_ship_qty_case_no,
                "shipped qty(CSV)" = shipq_qty) %>% 
  dplyr::select(-ref) %>% 
  dplyr::mutate(next_available_date = as.double(next_available_date),
                next_available_date = as.Date(next_available_date, origin = "1899-12-30")) -> compared_data


###################################################################################################################################################
###############################################################    Save it to DB  #################################################################
###################################################################################################################################################

compared_data <- compared_data %>%
  dplyr::mutate(back_order_date = as.double(back_order_date),
                shortage_date = as.double(shortage_date)) %>%
  
  dplyr::mutate(shortage_date = ifelse(is.na(shortage_date), specific_date, shortage_date)) %>% 
  
  #dplyr::mutate(back_order_date = as.Date(back_order_date, origin = "1899-12-30"),
  #              shortage_date = as.Date(shortage_date, origin = "1899-12-30")) 

  dplyr::mutate(back_order_date = as.Date(back_order_date, origin = "1899-12-30"),
              shortage_date = as.Date(shortage_date) )

##Change column names from new type to match old type
colnames(compared_data) <- c("location","legacy_sales_order","sales_order_date","jde_sales_order",
                             "reference_order_date","back_order_date","customer_po","customer_ship_to_8","customer_ship_to_9","make_buy_transfer",
                             "label_owner","priority_sku","product_label_sku","description","customer_profile_owner","shortage_reason",
                             "shortage_date","next_available_date","followup_comments","type_of_sale","type_of_sale_2",
                             "total_sku_beginning_inventory","oo_cases","b_t_open_order_cases","order_shortage_case_no",
                             "total_sku_shortage_qty","inventory_soft_hold_release","inventory_usable","production_schedule",
                             "production_soft_hold_release","purchase_order_and_transfer_in","sales_order_and_transfer_out","item_to_compare_ofr",
                             "item_to_compare_csv","shipped qty(OFR)","shipped qty(CSV)","match")


# saveRDS(compared_data, "OFR_data_base.rds")

saveRDS(compared_data, "OFR_data_base_06.20.2025.rds")

ofr_data_base <- readRDS("OFR_data_base.rds")


# Now you can use rbind
ofr_data_base_2 <- rbind(ofr_data_base, compared_data)

rbind(ofr_data_base, compared_data) -> ofr_data_base_2

# prevent duplicated save
ofr_data_base_2[!duplicated(ofr_data_base_2[,c("location", "legacy_sales_order", "sales_order_date",
                                               "jde_sales_order", "back_order_date", "reference_order_date",
                                               "customer_po", "customer_ship_to_8", "customer_ship_to_9",
                                               "make_buy_transfer", "label_owner", "priority_sku", "product_label_sku",
                                               "description", "customer_profile_owner", "shortage_reason",
                                               "shortage_date", "next_available_date", "followup_comments",
                                               "type_of_sale", "type_of_sale_2", "total_sku_beginning_inventory",
                                               "oo_cases", "b_t_open_order_cases", "order_shortage_case_no",
                                               "total_sku_shortage_qty", "inventory_soft_hold_release",
                                               "inventory_usable", "production_schedule", "production_soft_hold_release",
                                               "purchase_order_and_transfer_in", "sales_order_and_transfer_out",
                                               "item_to_compare_ofr", "item_to_compare_csv", "shipped qty(OFR)",
                                               "shipped qty(CSV)", "match")]),] -> ofr_data_base_2


# Get unique dates
unique_dates <- unique(ofr_data_base_2$shortage_date)
unusual_date <- unique_dates[which.max(unique_dates)]



# Filter out the unusual date
ofr_data_base_2 %>% 
  filter(shortage_date != unusual_date)  -> ofr_data_base_3


ofr_data_base_2 %>% 
  filter(shortage_date == unusual_date) %>% 
  dplyr::mutate(shortage_date = specific_date) -> ofr_data_base_4


rbind(ofr_data_base_3, ofr_data_base_4) -> ofr_data_base_final


saveRDS(ofr_data_base_final, "OFR_data_base.rds")


file.rename(from = "OFR_data_base_06.20.2025.rds", to = "rds/OFR_data_base_06.20.2025.rds")


################### OFR_data_base.rds is the main resource for the shiny #####################





#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################


####################### if you are seeing an error in date.. do below again #################

ofr_data_base_2 <- readRDS("OFR_data_base.rds")


# prevent duplicated save
ofr_data_base_2[!duplicated(ofr_data_base_2[,c("location", "legacy_sales_order", "sales_order_date",
                                               "jde_sales_order", "back_order_date", "reference_order_date",
                                               "customer_po", "customer_ship_to_8", "customer_ship_to_9",
                                               "make_buy_transfer", "label_owner", "priority_sku", "product_label_sku",
                                               "description", "customer_profile_owner", "shortage_reason",
                                               "shortage_date", "next_available_date", "followup_comments",
                                               "type_of_sale", "type_of_sale_2", "total_sku_beginning_inventory",
                                               "oo_cases", "b_t_open_order_cases", "order_shortage_case_no",
                                               "total_sku_shortage_qty", "inventory_soft_hold_release",
                                               "inventory_usable", "production_schedule", "production_soft_hold_release",
                                               "purchase_order_and_transfer_in", "sales_order_and_transfer_out",
                                               "item_to_compare_ofr", "item_to_compare_csv", "shipped qty(OFR)",
                                               "shipped qty(CSV)", "match")]),] -> ofr_data_base_2


# Get unique dates
unique_dates <- unique(ofr_data_base_2$shortage_date)
unusual_date <- unique_dates[which.max(unique_dates)]



# Filter out the unusual date
ofr_data_base_2 %>% 
  filter(shortage_date != unusual_date)  -> ofr_data_base_3


ofr_data_base_2 %>% 
  filter(shortage_date == unusual_date) %>% 
  dplyr::mutate(shortage_date = specific_date) -> ofr_data_base_4


rbind(ofr_data_base_3, ofr_data_base_4) -> ofr_data_base_final


ofr_data_base_final %>%
  mutate(shortage_date = as.Date(shortage_date)) %>%
  filter(shortage_date >= as.Date("2023-11-01")) -> ofr_data_base_final

saveRDS(ofr_data_base_final, "OFR_data_base.rds")


################### OFR_data_base.rds is the main resource for the shiny #####################

write.csv(ofr_data_base_final,"OFRDataBase.csv",row.names = FALSE)
  


