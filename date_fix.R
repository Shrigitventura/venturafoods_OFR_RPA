##################### Check ############################

readRDS("OFR_data_base.rds") -> date_fix

date_fix$shortage_date %>% unique()

#####################################################################
date_fix %>% 
  dplyr::mutate(shortage_date = as.character(shortage_date)) %>% 
  dplyr::filter(shortage_date != "4790319-08-30") -> date_fix

saveRDS(date_fix, "OFR_data_base.rds")
