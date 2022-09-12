library(dplyr)
library(readr)
library(tidyr)

# Source scripts for data sets 
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-DOC.R")
source("./R_data-cleaning/flow-reconstruction/code/clean-flows.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-TP.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-SRP.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-sulphate.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-organic-N.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-nitrate.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-ammonia-N.R")

wavelet_data <- function() {
  
  # Read in data sets 
  bp_doc_raw <- bp_DOC_monthly()
  bp_flow_raw <- station_flow_monthly()
  bp_tp_raw <- bp_TP_monthly()
  bp_srp_raw <- bp_SRP_monthly()
  bp_so4_raw <- bp_sulphate_monthly()
  bp_orgn_raw <- bp_organicN_monthly() %>% rename(DON_mg.L = orgN_mg.L)
  bp_no3_raw <- bp_nitrate_monthly()
  bp_nh3_raw <- bp_ammonia_monthly()
  bp_flow <- bp_flow_raw %>%select(year = Year, month = Month, SK05JG004_combined_cms, SK05JG006_cms, RC_IC_cms)
  
  # Join data sets 
  bp_drivers <- bp_doc_raw %>% 
    right_join(bp_tp_raw) %>% 
    right_join(bp_srp_raw) %>% 
    right_join(bp_so4_raw) %>% 
    right_join(bp_orgn_raw) %>% 
    right_join(bp_no3_raw) %>% 
    right_join(bp_nh3_raw) %>% 
    right_join(bp_flow) 
  
  return(bp_drivers)

}

wavelet_data()



