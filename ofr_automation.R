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
sarah <- read_csv("C:/Users/slee/OneDrive - Ventura Foods/Ventura Work/SCE/Project/FY 23/OFR/OE630CR_025216_20230807_.csv")

####################################################################################################################################################

# Clean Data
ofr %>% 
  data.frame() %>% 
  janitor::clean_names() -> ofr_data

sarah[-1:-2, ] -> sarah

sarah %>% 
  data.frame() %>% 
  janitor::clean_names() -> sarah_data

#######################################

# projected_to_ship_case_no in OFR should match with shipq_qty in Sarah's file

ofr_data %>% 
  dplyr::mutate(product_label_sku = gsub("-", "", product_label_sku)) %>% 
  dplyr::mutate(ref = paste0(location, "_", legacy_sales_order, "_", product_label_sku)) %>% 
  dplyr::select(ref, projected_to_ship_case_no) -> ofr_data_ship_case

sarah_data %>% 
  dplyr::mutate(ref = paste0(order_location, "_", order_number, "_", product, label)) %>% 
  dplyr::left_join(ofr_data_ship_case) %>%
  dplyr::mutate(match = ifelse(shipq_qty == projected_to_ship_case_no, "matching", "not_matching")) %>% 
  dplyr::mutate(shipq_qty = ifelse(match == "matching" | is.na(match), shipq_qty, projected_to_ship_case_no)) %>%
  dplyr::select(-match, -projected_to_ship_case_no, -ref) -> final_matched_data



colnames(final_matched_data)[1]<-"Order location"
colnames(final_matched_data)[2]<-"Order number"
colnames(final_matched_data)[3]<-"Invoice location"
colnames(final_matched_data)[4]<-"Invoice number"
colnames(final_matched_data)[5]<-"Super account number"
colnames(final_matched_data)[6]<-"Super account name"
colnames(final_matched_data)[7]<-"Sold to number"
colnames(final_matched_data)[8]<-"Sold name"
colnames(final_matched_data)[9]<-"Ship to number"
colnames(final_matched_data)[10]<-"Ship name"
colnames(final_matched_data)[11]<-"Order date"
colnames(final_matched_data)[12]<-"Ship date"
colnames(final_matched_data)[13]<-"Sequence"
colnames(final_matched_data)[14]<-"Product"
colnames(final_matched_data)[15]<-"Label"
colnames(final_matched_data)[16]<-"Original order qty"
colnames(final_matched_data)[17]<-"Order qty"
colnames(final_matched_data)[18]<-"Shipq qty"
colnames(final_matched_data)[19]<-"Short ship code"
colnames(final_matched_data)[20]<-"Short ship desc"
colnames(final_matched_data)[21]<-"Region"
colnames(final_matched_data)[22]<-"Region name"
colnames(final_matched_data)[23]<-"Sub region"
colnames(final_matched_data)[24]<-"Sub region name"
colnames(final_matched_data)[25]<-"Sales rep"
colnames(final_matched_data)[26]<-"Sales rep name"
colnames(final_matched_data)[27]<-"Master sales rep"
colnames(final_matched_data)[28]<-"Master sales rep nam"
colnames(final_matched_data)[29]<-"Sales manager"
colnames(final_matched_data)[30]<-"Sales manager name"




###################################################################################################################################################
####################################################      export to your directory    #############################################################
###################################################################################################################################################

writexl::write_xlsx(final_matched_data, "C:/Users/slee/OneDrive - Ventura Foods/Ventura Work/SCE/Project/FY 23/OFR/final_matched_data.xlsx")

