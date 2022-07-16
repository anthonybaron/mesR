library(readxl)
library(readr) 
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(janitor)
library(patchwork)

bp_wq <- function() {
  
  source("./R_data-cleaning/EEMs/code/site_coordinates.R")
  bp_coords <- site_coords_wip() 
  
  bpwq_raw <- read_csv("./R_data-cleaning/EEMs/data/raw/other/BuffaloPoundTransects_WSAData_2015-18_clean-data.csv") 
  
  bpwq <- bpwq_raw %>% 
    mutate(latitude = ifelse(station.number == "SK05JG0104", 50.57263, latitude),
           longitude = ifelse(station.number == "SK05JG0104", -105.33030, longitude),
           site_code_long = ifelse(station.number == "SK05JG0104", "Above Outlet", NA),
           site_code_long = ifelse(station.number == "SK05JG0102", "WTP Intake", site_code_long),
           site_code_long = ifelse(station.number == "SK05JG0098", "Opposite Parkview", site_code_long),
           site_code_long = ifelse(station.number == "SK05JG0096", "Opposite Sun Valley", site_code_long),
           site_code_long = ifelse(station.number == "SK05JG0095", "Opposite South Lake", site_code_long),
           site_code_long = ifelse(station.number == "SK05JG0094", "Below Causeway", site_code_long),
           site_code_long = ifelse(station.number == "SK05JG0222", "Upstream Causeway Centre", site_code_long),
           site_code_long = ifelse(station.number == "SK05JG0223", "Upstream Causeway East", site_code_long),
           site_code_long = ifelse(station.number == "SK05JG0224", "Inflow West", site_code_long),
           site_code_long = ifelse(station.number == "SK05JG0220", "Inflow Centre", site_code_long),
           site_code_long = ifelse(station.number == "SK05JG0225", "Inflow East", site_code_long),
          site_name = ifelse(site_code_long == "Above Outlet", "0.5 mi Above Outlet", NA),
          site_name = ifelse(site_code_long == "WTP Intake", "Opposite WTP Intake", site_name),
          site_name = ifelse(site_code_long == "Opposite Parkview", "Opposite Parkview", site_name),
          site_name = ifelse(site_code_long == "Opposite Sun Valley", "Opposite Sun Valley", site_name),
          site_name = ifelse(site_code_long == "Opposite South Lake", "Opposite South Lake", site_name),
          site_name = ifelse(site_code_long == "Below Causeway", "0.5 mi Below Causeway", site_name),
          site_name = ifelse(site_code_long == "Upstream Causeway Centre", "Upstream of Causeway Centre", site_name),
          site_name = ifelse(site_code_long == "Upstream Causeway East", "Upstream of Causeway East", site_name),
          site_name = ifelse(site_code_long == "Inflow West", "1.5 km below Qu'Appelle R. Inflow West", site_name),
          site_name = ifelse(site_code_long == "Inflow Centre", "1.5 km below Qu'Appelle R. Inflow Centre", site_name),
          site_name = ifelse(site_code_long == "Inflow East", "1.5 km below Qu'Appelle R. Inflow East", site_name)) %>%
    select(-c(latitude, longitude)) %>% 
    select(site_name, station.number, station.name, site_code_long, everything()) %>% 
    rename(date_ymd = date.time_yyyy.mm.dd.hh.mm.ss) %>% 
    rename_with(~ str_replace(str = .x, pattern = "\\.", replacement = "\\_")) %>% 
    rename(chla_ug.L = chlorophyll_a_ug.L,
           ext_coeff_m = PAR_extinction_per.m,
           TSS_mg.L = TSS_mg_L,
           turb_field_NTU = turbidity_field_NTU,
           turb_lab_NTU = turbidity_lab_NTU) %>% 
    separate(date_ymd, into = c("date_ymd", "time"), sep = " ") %>% 
    select(-c(station_number, station_name, time, lake_depth_m)) %>% 
    filter(!is.na(site_code_long) & !site_code_long == "Upstream Causeway West") 
  
  bpwq <- subset(bpwq, !is.na(site_code_long))
  bpwq <- select(bpwq, -site_code_long)
  
  return(bpwq)
  
}
bp_2015_2016_2017 <- function() {
  
  eems_doc_2015_2016_2017_raw <- read_excel("./R_data-cleaning/EEMs/data/raw/doc-eems/BuffalloPound_DOM_AllData.xlsx",
                                            sheet = "BP WSA Spatiotemporal")
  
  # Can remove PARAFAC model variables
  eems_doc_2015_2016_2017 <- eems_doc_2015_2016_2017_raw %>% 
    arrange(desc(Date)) %>% 
    select(-c(`Depth (m)`, Time, ends_with("(pc)"), ends_with("(RU)"))) %>%
    rename(site_altname = `Location; Depth`,
           latitude = Lat,
           longitude = Long,
           date_ymd = Date,
           DOC_mg.L = `DOC (mg/L)`,
           HIX_Ohno = HIX.ohno,
           PeakA_percent = `%PeakA`,
           PeakB_percent = `%PeakB`,
           PeakC_percent = `%PeakC`,
           PeakD_percent = `%PeakD`,
           PeakE_percent = `%PeakE`,
           PeakM_percent = `%PeakM`,
           PeakN_percent = `%PeakN`,
           PeakP_percent = `%PeakP`,
           PeakT_percent = `%PeakT`) %>% 
    mutate(site_name = site_altname,
           site_name = str_replace(site_name, "Buffalo Pound Lake ", ""),
           site_name = str_replace(site_name, "BPL ", ""),
           site_name = str_replace(site_name, "BP ", ""),
           site_name = ifelse(site_name %in% c("Opp WTP", "opp WTP intake", "opp. WTP (water treatment plant) intake", "opposite treatment plant intake", "opposite treatment plant", "Opp Treatment Plant Intake", "treatment plant intake"), "Opposite WTP Intake",
                              ifelse(site_name %in% c("Opp Parkview", "opposite of park view", "opp. park view", "opp Park View", "opposite park view"), "Opposite Parkview",
                                     ifelse(site_name %in% c("Opp Sun Valley", "opposite Sun Valley", "opp. Sun Valley", "opp Sun Valley"), "Opposite Sun Valley",
                                            ifelse(site_name %in% c("opposite south lake", "opp. South lake", "opposite S lake", "opp South Lake", "Opp South Lake"), "Opposite South Lake", 
                                                   ifelse(site_name %in% c("0.5 miles above outlet", "0.5 mi above Outlet"), "0.5 mi Above Outlet", 
                                                          ifelse(site_name == "Opp Sun Valley (1m)", "Opposite Sun Valley (1 m)",
                                                                 ifelse(site_name == "Opp Sun Valley (3m)", "Opposite Sun Valley (3 m)",
                                                                        ifelse(site_name %in% c("upstream Causeway", "US Causeway", "Upstream of Causeway", "upstream causeway", "u/s (upstream) causeway", "u/s (upstream) of causeway"), "Upstream of Causeway Centre",
                                                                               ifelse(site_name %in% c("US Causeway - WEST", "Upstream of Causeway - WEST", "upstream causeway West", "u/s (upstream) causeway west", "u/s (upstream) causeway westside", "u/s (upstream) causeway WS (west side)", "u/s (upstream) causeway W (west side)"), "Upstream of Causeway West",
                                                                                      ifelse(site_name %in% c("US Causeway - EAST", "Upstream of Causeway - EAST", "u/s (upstream) causeway east side", "uptream causeway East", "u/s (upstream) causeway east", "u/s (upstream) causeway ES (east side)", "u/s (upstream) causeway (E - east side)"), "Upstream of Causeway East", 
                                                                                             ifelse(site_name %in% c("1.5 km below Qu’Appelle", "1.5 km below Qu’Appelle R. Inflow", "1.5 km below Qu'ap.", "1.5 km below Qu'Appelle", "below Qu'ap inflow", "Buffalo Pound below Qu’Appelle"), "1.5 km below Qu'Appelle R. Inflow Centre", 
                                                                                                    ifelse(site_name %in% c("1.5 km below Qu’Appelle - WEST", "West 1.5 km below Qu'ap", "1.5 km below Qu’Appelle R. Inflow- WEST", "below Qu'ap. inflow (W)", "lake below Qu'appelle inflow WS (west side)", "1.5 km below Qu'ap West", "1.5 km below Qu'Ap W"), "1.5 km below Qu'Appelle R. Inflow West", 
                                                                                                           ifelse(site_name %in% c("1.5 km below Qu’Appelle - EAST", "1.5 km below Qu’Appelle R. Inflow- EAST", "East 1.5 km below Qu'ap.", "lake below Qu'ap inflow ES (east side)", "1.5 km below Qu'Appelle E", "1.5 km below Qu'ap East", "below Qu'ap inflow (E - east side)"), "1.5 km below Qu'Appelle R. Inflow East", 
                                                                                                                  ifelse(site_name %in% c("D/s Causeway", "0.5 miles below causeway"), "0.5 mi Below Causeway", 
                                                                                                                         ifelse(site_name %in% c("0.5 mi Below Causeway - WEST", "0.5 mi ds Causeway - WEST"), "0.5 mi Below Causeway West", 
                                                                                                                                ifelse(site_name %in% c("Qu'Appelle River @ Marquis", "Qu'appelle River at Marquis", "Qu’Appelle River at Marquis", "Qu’Appelle R @ Marquis"), "Qu'Appelle River at Marquis", 
                                                                                                                                       ifelse(site_name %in% c("Qu’Appelle R @ Hwy#19", "Qu’Appelle River at  Hwy#19"), "Qu'Appelle River at Hwy 19", 
                                                                                                                                              ifelse(site_name %in% c("Pelican Lake S (south) basin", "Pelican Lake (1 of 2)"), "Pelican Lake", 
                                                                                                                                                     ifelse(site_name %in% c("Buffalo Pound outlet", "outlet"), "0.5 mi Below Outlet", site_name))))))))))))))))))),
           site_name = factor(site_name),
           site_altname = factor(site_altname)) %>% 
    mutate_if(is.character, as.numeric) %>% 
    mutate(date_ymd = as.character(date_ymd),
           file_name = "BuffalloPound_DOM_AllData.xlsx",
           sheet_name = "BP WSA Spatiotemporal",
           source = "JM") %>% 
    mutate_if(is.factor, as.character) %>% 
    select(source, site_name, site_altname, everything())
  
  return(eems_doc_2015_2016_2017)
  
}
bp_2016 <- function() {
  
  eems_doc_2016_raw <- read_excel("./R_data-cleaning/EEMs/data/raw/doc-eems/Helen_2016_DOC&DOM.xlsx",
                                  sheet = "Combined Needs Verification")
  
  eems_doc_2016 <- eems_doc_2016_raw %>%
    select(-c("...7", `Time Stamp`, `Sample Rep`)) %>% 
    rename(sample_num = `#`,
           sample_id = `Sample ID`,
           site_altname = `Site Location Information`,
           date_ymd = Date,
           sample_id_DOC = `DOC Sample ID`,
           sample_id_EEM = `EEM Sample ID`,
           DOC_mg.L = `DOC_mg/L`,
           SUVA254 = SUVA,
           HIX_Ohno = HIX.ohno,
           PeakA_percent = `PeakA_%`,
           PeakB_percent = `PeakB_%`,
           PeakC_percent = `PeakC_%`,
           PeakD_percent = `PeakD_%`,
           PeakE_percent = `PeakE_%`,
           PeakM_percent = `PeakM_%`,
           PeakN_percent = `PeakN_%`,
           PeakP_percent = `PeakP_%`,
           PeakT_percent = `PeakT_%`) %>% 
    mutate(site_name = site_altname,
           site_name = ifelse(site_name == "Buffalo Pound Lake 0.5 mi above Outlet", "0.5 mi Above Outlet",
                              ifelse(site_name %in% c("Buffalo Pound Lake 0.5 mi Below Causeway - WEST", "Buffalo Pound Lake 0.5 mi ds Causeway - WEST"),  "0.5 mi Below Causeway West",
                                     ifelse(site_name == "Buffalo Pound Lake 1.5 km below Qu’Appelle R. Inflow", "1.5 km below Qu'Appelle R. Inflow Centre",
                                            ifelse(site_name == "Buffalo Pound Lake 1.5 km below Qu’Appelle R. Inflow- EAST", "1.5 km below Qu'Appelle R. Inflow East",
                                                   ifelse(site_name == "Buffalo Pound Lake 1.5 km below Qu’Appelle R. Inflow- WEST", "1.5 km below Qu'Appelle R. Inflow West",
                                                          ifelse(site_name ==	"Buffalo Pound Outlet", "0.5 mi Below Outlet",
                                                                 ifelse(site_name == "Buffalo Pound Lake Opp Parkview", "Opposite Parkview",
                                                                        ifelse(site_name == "Buffalo Pound Lake Opp South Lake", "Opposite South Lake",
                                                                               ifelse(site_name == "Buffalo Pound Lake Opp Sun Valley", "Opposite Sun Valley",
                                                                                      ifelse(site_name == "Buffalo Pound Lake Opp Sun Valley (1m)", "Opposite Sun Valley (1 m)",
                                                                                             ifelse(site_name == "Buffalo Pound Lake Opp Sun Valley (3m)", "Opposite Sun Valley (3 m)",
                                                                                                    ifelse(site_name ==	"Buffalo Pound Lake Opp Treatment Plant Intake", "Opposite WTP Intake",
                                                                                                           ifelse(site_name == "Qu’Appelle R @ Hwy#19", "Qu'Appelle River at Hwy 19", 
                                                                                                                  ifelse(site_name == "Qu’Appelle R @ Marquis", "Qu'Appelle River at Marquis",
                                                                                                                         ifelse(site_name == "Buffalo Pound Lake Upstream of Causeway - EAST", "Upstream of Causeway East",
                                                                                                                                ifelse(site_name == "Buffalo Pound Lake Upstream of Causeway - WEST", "Upstream of Causeway West", 
                                                                                                                                       ifelse(site_name == "Buffalo Pound Lake 0.5 mi above Outlet", "0.5 mi Above Outlet", 
                                                                                                                                              ifelse(site_name == "Buffalo Pound Lake 0.5 mi Below Causeway", "0.5 mi Below Causeway", 
                                                                                                                                                     ifelse(site_name == "Buffalo Pound Lake Upstream of Causeway", "Upstream of Causeway Centre", site_name))))))))))))))))))),
           S350to400 = as.numeric(S350to400),
           SR = as.numeric(SR),
           date_ymd = as.character(date_ymd),
           file_name = "Helen_2016_DOC&DOM.xlsx",
           sheet_name = "Combined Needs Verification",
           source = "CJW") %>% 
    select(source, site_name, site_altname, date_ymd, DOC_mg.L, A254:PeakT_percent)
  
  return(eems_doc_2016)
  
}
bp_2017 <- function() {
  
  clay2017_eems_raw <- read_excel("./R_data-cleaning/EEMs/data/raw/doc-eems/Copy of Results_DOC samples shipped Jan22 2018.xlsx",
                                  sheet = "Combined")
  
  clay2017_eems <- clay2017_eems_raw %>%
    select(-c(Time)) %>% 
    rename(bottle_id1 = `Bottle ID...1`,
           sample_id = `Bottle ID...2`,
           site_altname = "Sample Description",
           date_ymd = "Date",
           EEMs = `3DEEM`,
           DOC_mg.L = "DOC (mg/L)",
           TDN_mg.L = "TDN (mg/L)",
           EEM_contaminated = "EEM_Contami=NA()ted",
           HIX_Ohno = HIX.ohno) %>% 
    rename_at(vars(contains("Peak")), ~ paste0(.x, "_RU")) %>% 
    mutate(site_name = site_altname,
           site_name = ifelse(site_name %in% c("BPL Below Qu'Appelle East", "Buffalo Pound Lake below Qu'Appelle East"), "1.5 km below Qu'Appelle R. Inflow East",
                              ifelse(site_name %in% c("Buffalo Pound Lake below Qu'Appelle", "BPL Below Qu'Appelle"), "1.5 km below Qu'Appelle R. Inflow Centre",
                                     ifelse(site_name %in% c("Buffalo Pound Lake below Qu'Appelle West", "BPL Below Qu'Appelle West"), "1.5 km below Qu'Appelle R. Inflow West",
                                            ifelse(site_name %in% c("BPL Us Causeway West", "Buffalo Pound Lake US Causeway West"), "Upstream of Causeway West",
                                                   ifelse(site_name %in% c("BPL Upstream Causeway", "Buffalo Pound Lake US Causeway"), "Upstream of Causeway Centre",
                                                          ifelse(site_name %in% c("BPL Us Causeway East", "Buffalo Pound Lake US Causeway East"), "Upstream of Causeway East",
                                                                 ifelse(site_name %in% c("Buffalo Pound Lake 0.5 mi below Causeway", "BPL 0.5 mi Below Causeway"), "0.5 mi Below Causeway",
                                                                        ifelse(site_name %in% c("Buffalo Pound Lake Opp Parkview", "BPL Opp. Parkview"), "Opposite Parkview",
                                                                               ifelse(site_name %in% c("Buffalo Pound Lake Opp Sun Valley", "BPL Opp. Sun Valley"), "Opposite Sun Valley",
                                                                                      ifelse(site_name %in% c("Buffalo Pound Lake Opp South Lake", "BPL Opp. South Lake"), "Opposite South Lake",
                                                                                             ifelse(site_name %in% c("Buffalo Pound Lake Opp WTP", "BPL Opp. WTP"), "Opposite WTP Intake",
                                                                                                    ifelse(site_name %in% c("BPL 0.5 mi Abore Oulet", "Buffalo Pound Lake 0.5 mi above outlet"), "0.5 mi Above Outlet",
                                                                                                           ifelse(site_name == "Buffalo Pound Outlet", "0.5 mi Below Outlet",
                                                                                                                  ifelse(site_name %in% c("Qu'Appelle at Marquis", "Qu'Appelle R at Marquis"), "Qu'Appelle River at Marquis",
                                                                                                                         ifelse(site_name %in% c("Qu'Appelle R at Hwy 19", "Qu'Appelle at Highway 19", "Qu'Appelle River at Highway 19"), "Qu'Appelle River at Hwy 19",
                                                                                                                                ifelse(sample_id == "WSA 001", "Qu'Appelle River at Hwy 19",
                                                                                                                                       ifelse(site_name == "Raw", "BPWTP Raw",
                                                                                                                                              ifelse(site_name == "BPTWP Q5", "BPWTP Q5", site_name)))))))))))))))))),
           DOC = "X",
           S350to400 = as.numeric(S350to400),
           SR = as.numeric(SR),
           date_ymd = as.character(date_ymd),
           file_name = "Copy of Results_DOC samples shipped Jan22 2018.xlsx",
           sheet_name = "Combined",
           source = "CJW") %>% 
    select(source, site_name, site_altname, date_ymd, DOC_mg.L, TDN_mg.L, A254:PeakT_RU)
  
  return(clay2017_eems)
  
}
bp_2018 <- function() {
  
  # 2018
  # DOM column names are slightly different than the rest of the data set:
  # HIX Ohno is called "HIX.Ohno" rather than "HIX_Ohno" // ab 
  # Peaks are missing "_RU" // ab
  # 
  # Clay did not provide Peaks e.g. vars ending with "_pc" for 2018 (which we don't need)
  
  bp_2018_raw <- read_excel("R_data-cleaning/EEMs/data/raw/doc-eems/2018 - 3D-EEM DOC -WSA sample list.xlsx",
                            col_types = "text")
  
  bp_2018_sample_list_raw <- bp_2018_raw %>% 
    select(`BOTTLE ID`, `SAMPLE DESCRIPTION`, `TIME`, `DATE SAMPLED`, `DATE PROCESSED`, `PROCESSED BY:`)
  
  bp_2018_eems_raw <- bp_2018_raw %>% 
    select(-c(`BOTTLE ID`, `SAMPLE DESCRIPTION`, `TIME`, `DATE SAMPLED`, `DATE PROCESSED`, `PROCESSED BY:`, "...7"))
  
  bp_2018_sample_list <- bp_2018_sample_list_raw %>% 
    select(-TIME) %>% 
    rename(sample_id = `BOTTLE ID`,
           site_altname = `SAMPLE DESCRIPTION`,
           date_ymd = `DATE SAMPLED`,
           date_processed = `DATE PROCESSED`,
           processed_by = `PROCESSED BY:`) %>% 
    mutate_at(vars(date_ymd, date_processed), as.numeric) %>% 
    mutate_at(vars(date_ymd, date_processed), excel_numeric_to_date) %>% 
    mutate_at(vars(date_ymd, date_processed), as.character)
  
  bp_2018_eems <- bp_2018_eems_raw %>%
    select(-c(Time, Depth)) %>% 
    rename(lab = Lab,
           site_altname = Location,
           sample_id = `Sample ID`,
           date_ymd = Date, 
           UVM_id = `UVM ID`,
           Aqualog_id = AqualogID,
           exp_num = `Exp#`,
           notes = Notes,
           DOC_mg.L = `DOC (mg-C/L)`,
           TDN_mg.L = `TDN (mg-N/L)`,
           HIX_Ohno = `HIX.ohno`) %>% 
    rename_with(~ paste0(.x, "_RU"), starts_with("Peak")) %>% 
    select(lab:sample_id, V1, V2, everything()) %>% 
    mutate_at(vars(DOC_mg.L:PeakT_RU), as.numeric) %>% 
    mutate(notes = ifelse(DOC_mg.L == "broke", "vial broke", notes),
           lab = ifelse(is.na(lab), "WSA", lab),
           date_ymd = as_date(date_ymd),
           date_ymd = as.character(date_ymd),
           site_altname = ifelse(grepl("M", sample_id), "Method Blank", site_altname),
           site_altname = ifelse(is.na(site_altname), bp_2018_sample_list$site_altname, site_altname),
           sample_id = ifelse(is.na(sample_id), bp_2018_sample_list$sample_id, sample_id),
           date_ymd = ifelse(is.na(date_ymd), bp_2018_sample_list$date_ymd, date_ymd),
           rep_num = ifelse(sample_id %in% c("WSA 122", "WSA 126"), 1,
                            ifelse(sample_id %in% c("WSA 124", "WSA 125", "WSA 127"), 2, NA)),
           site_name = ifelse(site_altname %in% c("BP below causeway", "BP 0.5 d/s causeway", "BPL below causeway", "BPL downstream causeway", "BPL 0.5 mi below causeway", "BP 0.5m d/s Causeway"), "0.5 mi Below Causeway",
                              ifelse(site_altname %in% c("BPL u/s causeway west", "BPL ups Causeway west"), "Upstream of Causeway West",
                                     ifelse(site_altname %in% c("BP u/s causeway east", "BPL u/s causeway east", "BP u/s Causeway East"), "Upstream of Causeway East",
                                            ifelse(site_altname %in% c("BP u/s upstream causeway", "BPL u/s causeway", "BPL u/s causeway middle", "BP u/s Causeway", "BPL u/s Causeway", "BPL u/s Courseway", "BP u/s courseway"), "Upstream of Causeway Centre", 
                                                   ifelse(site_altname %in% c("BPL opp WTP", "BP opp WTP", "BPL WTP"), "Opposite WTP Intake",
                                                          ifelse(site_altname %in% c("BP 0.5m above outlet", "BPL above Outlet"), "0.5 mi Above Outlet",
                                                                 ifelse(site_altname %in% c("BPL Qu'Appelle Inflow mid", "BP 1.5m Bellow Qu'Appelle inflow", "BPL 1.5 mi below Qu'Appelle inflow", "BPL bellow Qu'Appelle", "BPL below Qu'Appelle", "BPL 1.5 Bellow QuAR", "BP below Qu'Appelle"), "1.5 km below Qu'Appelle R. Inflow Centre",
                                                                        ifelse(site_altname %in% c("BPL Qu'Appelle Inflow West", "BPL 1.5 mi below Qu'Appelle inflow W", "BPL below Qu'Appelle west"), "1.5 km below Qu'Appelle R. Inflow West",
                                                                               ifelse(site_altname %in% c("BPL Qu'Appelle Inflow East", "BPL 1.5 below Qu'Appelle inflow E", "BPL Below Qu'Appelle East", "BP 1.5m Bellow Qu'Appelle in flow east"), "1.5 km below Qu'Appelle R. Inflow East", 
                                                                                      ifelse(site_altname %in% c("BP opp South Lake", "BPL opp South lake", "BPL opp South Lake"), "Opposite South Lake",
                                                                                             ifelse(site_altname %in% c("BP opp Parkview", "BPL opp Parkview", "BP opp Partview"), "Opposite Parkview",
                                                                                                    ifelse(site_altname %in% c("BPL opp Sun Valley", "BP opp Sun Valley", "BPL Sun Valley"), "Opposite Sun Valley", 
                                                                                                           ifelse(grepl("Marquis", site_altname), "Qu'Appelle River at Marquis",
                                                                                                                  ifelse(grepl("19", site_altname), "Qu'Appelle River at Hwy 19", 
                                                                                                                         ifelse(grepl("BP @ Outlet", site_altname), "0.5 mi Below Outlet", site_altname))))))))))))))),
           file_name = "2018 - 3D-EEM DOC -WSA sample list.xlsx",
           sheet_name = "Sheet1", 
           source = "CJW") %>%  
    select(source, site_name, site_altname, date_ymd, DOC_mg.L:PeakT_RU)
  
  return(bp_2018_eems)
  
} 
bp_2019 <- function() {
  
  # 2019 -- issues to fix/check and other comments
  # 
  # 1. A254 from JM and A254 from Clay don't match; JM numbers need to be
  #    multiplied by 100. However, some places have two rows for the same 
  #    sampling date, so when we have DOM and JM data for the same date/site we 
  #    should use A254 from Clay
  #    // ab 2022-01-19 -- removed JM A254 for now 
  # 
  # 2. If/when we get all the data from JM and A254 is provided there, we can
  #    add a new column for JM, and have A254_JM and A254_CJW
  #    // ab 2022-01-19
  #
  # 3. Clay could not double check that the sites and dates were matched
  #    correctly for 2019 b/c he did not have site names, only WSA number codes
  #    // ab 2022-01-19
  #
  # 4. Clay was able to verify that all data he sent me has the correct A254
  #    units. Samples where A254 < 1 are confirmed to be blanks.
  #    // ab 2022-01-19
  #    
  # 5. From Clay: Sample "BPL below Qu'Appelle East (4/22/2019)" looks like a
  #    contaminated blank. So this should be removed, assuming it was matched
  #    correctly from the parent data files. The last three rows of the selected
  #    data file are also likely blanks with some contamination and can be 
  #    removed.
  #    // ab 2022-02-23
  #    
  # 6. From Clay: Based on your plots (pa254 and pdoc below), I support only 
  #    using my A254 and DOM results. it looks like DOC patterns are similar 
  #    between sources. Is it possible to merge the DOC and WQ from JM into the
  #    matching rows with the DOM? In the spreadsheet they are not yet paired.
  #    
  #    Once the WQ and DOM are matched within the same row and the odd DOM 
  #    samples I flagged aboved are investigated and removed if their odd values
  #    are confirmed in the original data, please send the data again and I'll
  #    take one last look.
  #    // ab 2022-03-07
  #    
  #    The data are looking good and almost ready for the fun part.
  
  # Use this sample list to match samples in jm_doc_wq & clay_eems
  wsa_sample_list_raw <- read_excel("./R_data-cleaning/EEMs/data/raw/doc-eems/2019_3D-EEM_DOC_WSA_fromCam.xlsx", # 97x6
                                    sheet = "Sheet1") 
  
  # Use this file for DOC
  # and Chl a, turbidity, Secchi depth, extinction coef 
  jm_doc_wq_raw <- read_excel("./R_data-cleaning/EEMs/data/raw/doc-eems/JM-WSA_BP-data-2019.xlsx", # 72x14
                              sheet = "Sheet1")
  
  # Use this for EEMs 
  clay_eems_raw <- read_excel("./R_data-cleaning/EEMs/data/raw/doc-eems/Clay_DOC_BuffaloPound_2019_USaskatchewan_BaulchLabandWSA.xlsx", # 131x25
                              sheet = "DOC_BuffaloPound_2019_USaskatch", skip = 7) 
  
  # Sample list for matching WQ and EEMs 
  wsa_sample_list <- wsa_sample_list_raw %>% # 97x3
    select(1, 2, `DATE SAMPLED`) %>%
    rename(sample_id = `BOTTLE ID`,
           date_ymd = `DATE SAMPLED`,
           site_altname = `SAMPLE DESCRIPTION`) %>% 
    mutate(row_num = row_number(sample_id),
           date_ymd = ymd(date_ymd)) %>% 
    select(row_num, everything())
  
  # Water quality from JM spaatial sampling
  # DOC, Chl a, turbidity, Secchi depth, extinction coef
  jm_doc_wq <- jm_doc_wq_raw %>% 
    rename(sample_num = sample.number,
           datetime_ymdhms = date.time,
           sample_id = sample.id,
           station_id = station.number,
           site_altname = station.name,
           sample_depth_m = sampling.depth_m,
           turb_lab_NTU = lab.turbidity_NTU,
           chla_ug.L = chlorohyl.a_ug.L,
           secchi_depth_m = secchi.depth_m,
           ext_coeff_m = extinction.coeff_.m,
           turb_field_NTU = field.turbidity_NTU,
           DOC_mg.L = DOC_mg.L,
           station_num = station.number) %>% 
    mutate(turb_lab_NTU = as.numeric(turb_lab_NTU),
           chla_ug.L = str_remove(chla_ug.L, "<"),
           secchi_depth_m = str_remove(secchi_depth_m, ">"),
           date_ymd = as.character(datetime_ymdhms)) %>% 
    mutate_at(vars(chla_ug.L, secchi_depth_m), as.numeric) %>% 
    separate(date_ymd, into = c("date_ymd", "time"), sep = " ") %>% 
    select(-c(`abs254...4`, `abs254...14`, time)) %>%
    mutate(site_name = site_altname,
           site_name = ifelse(site_name %in% c("Buffalo Pound Lake US Causeway West", "Buffalo Pound Lake Upstream of Causeway- WEST"), "Upstream of Causeway West", # 0222
                              ifelse(site_name %in% c("Buffalo Pound Lake US Causeway East", "Buffalo Pound Lake Upstream of Causeway- EAST"), "Upstream of Causeway East", # 0223
                                     ifelse(site_name %in% c("Buffalo Pound Lake US Causeway Center", "Buffalo Pound Lake Upstream of Causeway"), "Upstream of Causeway Centre", # 0219
                                            ifelse(site_name %in% c("Buffalo Pound Lake Opp Treatment Plant Intake", "Buffalo Pound Lake Opp WTP"), "Opposite WTP Intake", # 0102
                                                   ifelse(site_name %in% c("Buffalo Pound Lake Above Outlet", "Buffalo Pound Lake 0.5mi above Outlet"), "0.5 mi Above Outlet", # 0104
                                                          ifelse(site_name %in% c("Buffalo Pound Lake 0.5mi Below Causeway", "Buffalo Pound Lake DS Causeway"), "0.5 mi Below Causeway", # 0094
                                                                 ifelse(site_name %in% c("Buffalo Pound Lake 1.5 km below Qu'Appelle R. Inflow", "Buffalo Pound Lake below Qu'Appelle Center"), "1.5 km below Qu'Appelle R. Inflow Centre", # 0220
                                                                        ifelse(site_name %in% c("Buffalo Pound Lake 1.5 km below Qu'Appelle R. Inflow - WEST", "Buffalo Pound Lake below Qu'Appelle West"), "1.5 km below Qu'Appelle R. Inflow West", # 0224
                                                                               ifelse(site_name %in% c("Buffalo Pound Lake 1.5 km below Qu'Appelle R. Inflow - EAST", "Buffalo Pound Lake below Qu'Appelle East"), "1.5 km below Qu'Appelle R. Inflow East", # 0025
                                                                                      ifelse(site_name == "Buffalo Pound Lake Opp South Lake", "Opposite South Lake", # 0095
                                                                                             ifelse(site_name == "Buffalo Pound Lake Opp Sun Valley", "Opposite Sun Valley", # 0096
                                                                                                    ifelse(site_name == "Buffalo Pound Lake Opp Parkview", "Opposite Parkview", site_name)))))))))))), # 0098
           date_ymd = ymd(date_ymd)) %>% 
    filter(!grepl("Buoy", site_name)) %>% 
    select(sample_id, site_name, site_altname, date_ymd, DOC_mg.L, turb_lab_NTU, turb_field_NTU, chla_ug.L:ext_coeff_m)
  
  # EEMS from Clay's lab
  clay_eems <- clay_eems_raw %>% # 131x25
    rename(sample_num = sample.num,
           sample_id = sample.id,
           date_ymd = sample.date_ymd,
           DOC_mg.L = `DOC(mg/L)`,
           TDN_mg.L = `TDN(mg/L)`,
           HIX_Ohno = HIX.ohno) %>% 
    filter(!grepl("m_BP_2", sample_id)) %>%
    select(-DOC_mg.L) %>% 
    mutate(date_ymd = excel_numeric_to_date(as.numeric(date_ymd), date_system = "modern"),
           row_num = row_number(sample_id)) %>% 
    select(-sample_num) %>% 
    select(row_num, everything())
  
  # Join EEMs data with sample list 
  clay_eems_clean <- wsa_sample_list %>%
    left_join(clay_eems, by = c("sample_id", "row_num", "date_ymd")) %>% 
    select(-c(row_num))
  
  # Join EEMs, sample list with JM WQ 
  bp_doc_eems_2019 <- clay_eems_clean %>% 
    # left_join(jm_doc_wq, by = "sample_id") %>%
    full_join(jm_doc_wq, by = "sample_id") %>% 
    rename(date_ymd_clay = date_ymd.x, 
           date_ymd_jm = date_ymd.y) %>% 
    mutate(site_name = ifelse(site_altname.x == "BP Outlet", "0.5 mi Below Outlet",
                              ifelse(site_altname.x == "Method Blank", "Method Blank",
                                     ifelse(site_altname.x == "Qu'Appelle @ HWY19", "Qu'Appelle River at Hwy 19",
                                            ifelse(site_altname.x == "Qu'Appelle @ Marquis", "Qu'Appelle River at Marquis", site_name)))),
           site_name = ifelse(sample_id == "WSA 9991", "Opposite WTP Intake",
                              ifelse(sample_id == "WSA 9992", "Opposite Parkview",
                                     ifelse(sample_id == "WSA 9993", "Opposite South Lake",
                                            ifelse(sample_id == "WSA 9994", "0.5 mi Below Causeway", site_name)))),
           date_ymd_clay = as.character(date_ymd_clay),
           date_ymd_clay = ifelse(is.na(date_ymd_clay), "2019-04-22", date_ymd_clay),
           date_ymd_clay = ifelse(date_ymd_clay == "2019-05-02", "2019-04-22", 
                                  ifelse(date_ymd_clay == "2019-05-29", "2019-05-28", 
                                         ifelse(date_ymd_clay %in% c("2019-06-10", "2019-06-24"), "2019-06-27", 
                                                ifelse(date_ymd_clay == "2019-07-08", "2019-07-22", 
                                                       ifelse(date_ymd_clay == "2019-08-06", "2019-08-15", 
                                                              ifelse(date_ymd_clay == "2019-08-20", "2019-09-23", 
                                                                     ifelse(is.na(date_ymd_clay) & site_name == "Upstream of Causeway Centre", "2019-07-22", date_ymd_clay))))))),
           date_ymd_clay = as.character(date_ymd_clay)) %>%
    mutate_at(vars("S275to295", "S350to400", "SR", "PeakP"), as.numeric) %>% 
    rename_at(vars(contains("Peak")), ~ paste0(.x, "_RU")) %>% 
    select(site_name, date_ymd = date_ymd_clay, DOC_mg.L, A254, chla_ug.L, 
           A280:PeakT_RU, turb_lab_NTU, turb_field_NTU, secchi_depth_m, ext_coeff_m)
  
  
  return(bp_doc_eems_2019)
  
}
buoy_2019 <- function() { 
  
  # Use this sample list to match buoy samples in clay_eems
  wsa_sample_list_raw <- read_excel("./R_data-cleaning/EEMs/data/raw/doc-eems/2019_3D-EEM_DOC_WSA_fromCam.xlsx", # 97x6
                                    sheet = "Sheet1") 
  
  # Use this for buoy EEMs 
  clay_eems_raw <- read_excel("./R_data-cleaning/EEMs/data/raw/doc-eems/Clay_DOC_BuffaloPound_2019_USaskatchewan_BaulchLabandWSA.xlsx", # 131x25
                              sheet = "DOC_BuffaloPound_2019_USaskatch", skip = 7) 
  
  wsa_sample_list <- wsa_sample_list_raw %>%
    select(1, 2, `DATE SAMPLED`) %>%
    rename(sample_id = `BOTTLE ID`,
           date_ymd = `DATE SAMPLED`,
           site_altname = `SAMPLE DESCRIPTION`) %>% 
    mutate(row_num = row_number(sample_id),
           date_ymd = ymd(date_ymd)) %>% 
    select(row_num, everything())
  
  buoy_eems <- clay_eems_raw %>% # 34x25
    rename(sample_num = sample.num,
           sample_id = sample.id,
           date_ymd = sample.date_ymd,
           DOC_mg.L = `DOC(mg/L)`,
           TDN_mg.L = `TDN(mg/L)`,
           HIX_Ohno = HIX.ohno) %>% 
    filter(grepl("m_BP_2", sample_id)) %>%
    mutate(date_ymd = excel_numeric_to_date(as.numeric(date_ymd), date_system = "modern"),
           row_num = row_number(sample_id)) %>% 
    select(-sample_num) %>% 
    select(row_num, everything())
  
  # Join EEMs data with sample list 
  buoy_eems_clean <- buoy_eems %>%
    left_join(wsa_sample_list, by = c("sample_id", "row_num", "date_ymd")) %>% 
    select(-c(row_num, site_altname)) %>% 
    mutate(site_name = ifelse(grepl("0.8", sample_id), "Buoy 0.8 m",
                              ifelse(grepl("2.8", sample_id), "Buoy 2.8 m", NA)),
           date_ymd = as.character(date_ymd)) %>% 
    mutate_at(vars("DOC_mg.L", "TDN_mg.L", "S275to295", "S350to400", "SR", "PeakP"), as.numeric) %>% 
    rename_at(vars(contains("Peak")), ~ paste0(.x, "_RU")) %>% 
    select(site_name, date_ymd:PeakT_RU)
  
  return(buoy_eems_clean)
  
}
lat_long <- function() {
  
  source("./R_data-cleaning/EEMs/code/site-names.R")
  
  lat_long_incomplete <- bp_2015_2016_2017 %>% 
    select(site_name, latitude, longitude) %>% 
    filter(!is.na(latitude) & !is.na(longitude)) %>%
    distinct(.)
  
  site_names <- site_names_all() %>% 
    select(site_name) %>% 
    distinct(.) 
  
  lat_long_complete <- site_names %>% full_join(lat_long_incomplete) %>% 
    mutate(latitude = ifelse(grepl("Iskwao", site_name), 50.97,
                             ifelse(grepl("Ridge", site_name), 50.95,
                                    ifelse(grepl("1 m", site_name), 50.65615,
                                           ifelse(grepl("3 m", site_name), 50.65615, latitude)))),
           longitude = ifelse(grepl("Iskwao", site_name), -105.95,
                              ifelse(grepl("Ridge", site_name), -106.32,
                                     ifelse(grepl("1 m", site_name), -105.5206,
                                            ifelse(grepl("3 m", site_name), -105.5206, latitude)))))
  
}

# Can ignore all warning messages
bp_wq <- bp_wq()
bp_2015_2016_2017 <- bp_2015_2016_2017()
bp_2016 <- bp_2016()
bp_2017 <- bp_2017()
bp_2018 <- bp_2018() 
bp_2019 <- bp_2019()
buoy_2019 <- buoy_2019()
latitude_longitude <- lat_long()

bp_all <- function(write = FALSE, outdir = "./R_data-cleaning/EEMs/data/processed/") {
  
  lat_long <- latitude_longitude
  
  bp_all <- bind_rows(bp_2015_2016_2017, bp_2016, bp_2017, bp_2018, bp_2019, buoy_2019) %>% 
    full_join(bp_wq, by = c("site_name", "date_ymd")) %>% 
    # mutate(chla_ug.L = paste0(chla_ug.L.x, chla_ug.L.y)) %>% 
    mutate(chla_ug.L = ifelse(is.na(chla_ug.L.x), chla_ug.L.y, NA),
           chla_ug.L = ifelse(is.na(chla_ug.L.y), chla_ug.L.x, chla_ug.L),
           turb_field_NTU = ifelse(is.na(turb_field_NTU.x), turb_field_NTU.y, NA),
           turb_field_NTU = ifelse(is.na(turb_field_NTU.y), turb_field_NTU.x, turb_field_NTU),
           turb_lab_NTU = ifelse(is.na(turb_lab_NTU.x), turb_lab_NTU.y, NA),
           turb_lab_NTU = ifelse(is.na(turb_lab_NTU.y), turb_lab_NTU.x, turb_lab_NTU),
           secchi_depth_m = ifelse(is.na(secchi_depth_m.x), secchi_depth_m.y, NA),
           secchi_depth_m = ifelse(is.na(secchi_depth_m.y), secchi_depth_m.x, secchi_depth_m),
           ext_coeff_m = ifelse(is.na(ext_coeff_m.x), ext_coeff_m.y, NA),
           ext_coeff_m = ifelse(is.na(ext_coeff_m.y), ext_coeff_m.x, ext_coeff_m)) %>% 
    select(-c(latitude, longitude)) %>% 
    # mutate(site_name = as.factor(site_name),
    #        date_ymd = ymd(date_ymd),
    #        latitude = NA,
    #        longitude = NA,
    #        latitude = ifelse(is.na(latitude), lat_long$latitude, latitude),
    #        longitude = ifelse(is.na(longitude), lat_long$longitude, longitude)) %>%
    select(source, site_name, site_altname, date_ymd, TDN_mg.L, DOC_mg.L, 
           A254, SUVA:Fmax, chla_ug.L, turb_field_NTU, turb_lab_NTU, TSS_mg.L,
           ext_coeff_m, secchi_depth_m) %>%
    arrange(-desc(date_ymd))
  
  outname <- paste0(Sys.Date(), "_", "bp_DOC_EEMs_processed.csv")
  outpath <- file.path(outdir, outname)
  
  if (isTRUE(write)) {
    write.csv(bp_all, file = outpath)
  }
  
  return(bp_all)
  
}
bp_doc_eems <- function(df = bp_all(), write = FALSE, outdir = "./R_data-cleaning/EEMs/data/processed/") {
  
  bpde <- df %>% 
    mutate(Year = factor(year(date_ymd)),
           DOY = yday(date_ymd),
           site_name = factor(site_name)) %>% 
    filter(!grepl("BPWTP", site_name), 
           !site_name %in% c("Method Blank", "Iskwao Creek", "Iskwao Creek 1", 
                             "Ridge Creek", "Ridge Creek 1", "Pelican Lake", 
                             "Opposite Sun Valley (1 m)", "Opposite Sun Valley (3 m)", 
                             "Buffalo Pound Outlet", "Moose Jaw Creek at TWP RD 184",
                             "Qu'Appelle River at Hwy 19", "Upstream of Causeway West",
                             "0.5 mi Below Causeway West", "0.5 mi Below Outlet", 
                             "Qu'Appelle River at Marquis"),
           !is.na(date_ymd),
           !Year == 2015) %>% 
    select(-c(SUVA)) %>% 
    mutate(SUVA = A254 / DOC_mg.L) %>% 
    select(site_name, date_ymd, Year, DOY, TDN_mg.L, DOC_mg.L, SUVA, A254, 
           A280, A350, A440, S275to295, S350to400, SR, BA, FI, HIX, HIX_Ohno, Fmax, 
           PeakA_RU:PeakT_RU,  turb_lab_NTU, turb_field_NTU, chla_ug.L, secchi_depth_m,
           ext_coeff_m, everything()) 
  
  
  # // remove duplicates from 2016
  bpde16 <- bpde %>% filter(Year == 2016) %>% distinct() 
  
  bpde16$remove <- ifelse(bpde16$DOC_mg.L == 7.119 & is.na(bpde16$Fmax), "REMOVE", NA)
  bpde16$remove <- ifelse(bpde16$DOC_mg.L == 7.118 & is.na(bpde16$Fmax), "REMOVE", bpde16$remove)
  bpde16$remove <- ifelse(bpde16$DOC_mg.L == 6.786 & is.na(bpde16$Fmax), "REMOVE", bpde16$remove)
  bpde16$remove <- ifelse(bpde16$DOC_mg.L == 6.768 & is.na(bpde16$Fmax), "REMOVE", bpde16$remove)
  bpde16$remove <- ifelse(bpde16$DOC_mg.L == 6.758 & is.na(bpde16$Fmax), "REMOVE", bpde16$remove)
  bpde16$remove <- ifelse(bpde16$DOC_mg.L == 6.752 & is.na(bpde16$Fmax), "REMOVE", bpde16$remove)
  bpde16$remove <- ifelse(bpde16$DOC_mg.L == 6.717 & is.na(bpde16$Fmax), "REMOVE", bpde16$remove)
  bpde16$remove <- ifelse(bpde16$DOC_mg.L == 6.650 & is.na(bpde16$Fmax), "REMOVE", bpde16$remove)
  bpde16$remove <- ifelse(bpde16$DOC_mg.L == 6.639 & is.na(bpde16$Fmax), "REMOVE", bpde16$remove)
  bpde16$remove <- ifelse(bpde16$DOC_mg.L == 6.631 & is.na(bpde16$Fmax), "REMOVE", bpde16$remove)
  bpde16$remove <- ifelse(bpde16$DOC_mg.L == 6.578 & is.na(bpde16$Fmax), "REMOVE", bpde16$remove)
  bpde16$remove <- ifelse(bpde16$DOC_mg.L == 6.571 & is.na(bpde16$Fmax), "REMOVE", bpde16$remove)
  bpde16$remove <- ifelse(bpde16$DOC_mg.L == 6.570 & is.na(bpde16$Fmax), "REMOVE", bpde16$remove)
  bpde16$remove <- ifelse(bpde16$DOC_mg.L == 6.512 & is.na(bpde16$Fmax), "REMOVE", bpde16$remove)
  bpde16$remove <- ifelse(bpde16$DOC_mg.L == 6.368 & is.na(bpde16$Fmax), "REMOVE", bpde16$remove)
  bpde16$remove <- ifelse(bpde16$DOC_mg.L == 6.339 & is.na(bpde16$Fmax), "REMOVE", bpde16$remove)
  bpde16$remove <- ifelse(bpde16$DOC_mg.L == 6.249 & is.na(bpde16$Fmax), "REMOVE", bpde16$remove)
  bpde16$remove <- ifelse(bpde16$DOC_mg.L == 6.198 & is.na(bpde16$Fmax), "REMOVE", bpde16$remove)
  bpde16$remove <- ifelse(bpde16$DOC_mg.L == 6.155 & is.na(bpde16$Fmax), "REMOVE", bpde16$remove)
  bpde16$remove <- ifelse(bpde16$DOC_mg.L == 5.592 & is.na(bpde16$Fmax), "REMOVE", bpde16$remove)
  
  bpde16 <- subset(bpde16, is.na(remove)); bpde16 <- select(bpde16, -remove)
  
  # // create factors and order them for sites
  site_facs_df <- c(
    "1.5 km below Qu'Appelle R. Inflow East",
    "1.5 km below Qu'Appelle R. Inflow Centre",
    "1.5 km below Qu'Appelle R. Inflow West",
    "Upstream of Causeway Centre",
    "Upstream of Causeway East",
    "0.5 mi Below Causeway",
    "Opposite South Lake",
    "Opposite Sun Valley",
    "Opposite Parkview",
    "Opposite WTP Intake",
    "0.5 mi Above Outlet"
  ) %>% 
    as_tibble() %>% 
    rename(site_name = value) 
  
  site_codes <- tribble(
    ~site_code_long,            ~site_abbr,  
    "Inflow East",              "IE",
    "Inflow Centre",            "IC",
    "Inflow West",              "IW",
    "Upstream Causeway Centre", "CC",
    "Upstream Causeway East",   "CE",
    "Below Causeway",           "CB",
    "Opposite South Lake",      "SL",
    "Opposite Sun Valley",      "SV",
    "Opposite Parkview",        "PK", 
    "WTP Intake",               "TP",
    "Above Outlet",             "AO"  
  )
  
  site_codes <- site_facs_df %>% bind_cols(site_codes)
  site_codes_c <- site_codes[["site_code_long"]]
  site_abbrs_c <- site_codes[["site_abbr"]]
  
  # // source site coordinates to add to data set
  source("./R_data-cleaning/EEMs/code/site_coordinates.R")
  bp_coords <- site_coords_wip() 
  
  # // add factor levels, site coordinates, and calculate peak ratios
  eems <- bpde %>% 
    filter(!Year == 2016) %>% 
    # filter(!Year == 2016 & !is.na(site_code_long)) %>% 
    bind_rows(bpde16) %>% 
    arrange(date_ymd) %>% 
    left_join(bp_coords) %>%
    left_join(site_codes) %>% 
    select(site_name, site_code_long, site_abbr, everything()) %>% 
    mutate(site_code_long = forcats::fct_relevel(site_code_long, site_codes_c),
          site_abbr = forcats::fct_relevel(site_abbr, site_abbrs_c),
          AT_ratio = PeakA_RU / PeakT_RU,
          CA_ratio = PeakC_RU / PeakA_RU,
          CM_ratio = PeakC_RU / PeakM_RU,
          CT_ratio = PeakC_RU / PeakT_RU,
          date_ymd = as.character(date_ymd),
          date_ymd = ifelse(date_ymd == "2018-05-25", "2018-05-23", date_ymd),
          date_ymd = ymd(date_ymd),
          Month = month(date_ymd, label = TRUE, abbr = TRUE),
          turb_field_NTU = ifelse(turb_field_NTU < 0, NA, turb_field_NTU)) %>% 
    filter(DOC_mg.L < 9 & !grepl("Buoy", site_name)) %>% 
    select(site_name, site_code_long, site_abbr, date_ymd, Year, Month, DOY, latitude, longitude,
           distHaversine_km, TDN_mg.L, DOC_mg.L, chla_ug.L, SUVA, A254, A280, A350, A440, 
           BA, FI, HIX, HIX_Ohno, S275to295, S350to400, SR, Fmax, 
           PeakA_RU, PeakB_RU, PeakC_RU, PeakD_RU, PeakE_RU, PeakM_RU, PeakN_RU, PeakP_RU, PeakT_RU,
           AT_ratio, CA_ratio, CM_ratio, CT_ratio, 
           turb_field_NTU, turb_lab_NTU, secchi_depth_m, ext_coeff_m, TSS_mg.L, everything())
  
  outname <- paste0(Sys.Date(), "_", "bp_DOC_EEMs_processed-select-sites.csv")
  outpath <- file.path(outdir, outname)
  
  if (isTRUE(write)) {
    write.csv(bpde, file = outpath)
  }
  
  return(eems)
  
}

ee <- bp_doc_eems()

ee %>% 
  group_by(site_abbr, distHaversine_km) %>% 
  summarise(m)
  ggplot(aes(distHaversine_km, chla_ug.L, col = site_abbr)) + 
  facet_wrap(~ Year) +
  geom_point(size = 3)

  
df <- ee %>% 
  mutate(distHaversine_km = distHaversine_km + 1.5) %>% 
  select(site_code_long, site_abbr, date_ymd, DOY, Year, distHaversine_km, chla_ug.L, turb_field_NTU:ext_coeff_m) %>% 
  pivot_longer(cols = c(chla_ug.L:ext_coeff_m), 
               names_to = "parameter", 
               values_to = "result") %>% 
  mutate(parameter = factor(parameter),
         Site = site_code_long) %>% 
  group_by(Site, site_abbr, Year, distHaversine_km, parameter) %>% 
  summarise(mean_result = mean(result, na.rm = TRUE),
            sd_result = sd(result, na.rm = TRUE)) %>% 
  mutate(lower = mean_result - sd_result,
         upper = mean_result + sd_result) %>% 
  ungroup() 
  
df %>%
  filter(parameter == "chla_ug.L" & !Year == "2018") %>% 
  ggplot(aes(distHaversine_km, mean_result)) + 
  facet_wrap(~ Year) +
  xlim(c(0, 30)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper, col = Site), width = 0.33) +
  geom_point(aes(col = Site), size = 3) + 
  scale_color_viridis_d(begin = 0, end = 0.8) +
  theme(legend.position = 'bottom') +
  labs(x = "Distance from Buffalo Pound Lake Inflow", y = "Chl a")


ee %>% 
  filter(!is.na(chla_ug.L)) %>% 
  ggplot(aes(distHaversine_km, chla_ug.L, col = Year)) +
  facet_wrap(~ DOY) + 
  geom_point(size = 3, alpha = 3/4) +
  lims(x = c(0, 30), y = c(0, 150)) +
  scale_color_viridis_d(end = 0.8)

ee %>% 
  filter(!is.na(turb_field_NTU) & turb_field_NTU < 80) %>% 
  ggplot(aes(distHaversine_km, turb_field_NTU, col = Year)) +
  facet_wrap(~ DOY) + 
  geom_point(size = 3, alpha = 3/4) +
  lims(x = c(0, 30), y = c(0, 40)) +
  scale_color_viridis_d(end = 0.8)

ee %>% 
  filter(!is.na(turb_field_NTU)) %>% 
  ggplot(aes(distHaversine_km, turb_lab_NTU, col = Year)) +
  facet_wrap(~ DOY) + 
  geom_point(size = 3, alpha = 3/4) +
  lims(x = c(0, 30), y = c(0, 40)) +
  scale_color_viridis_d(end = 0.8)

ee %>% 
  filter(!is.na(secchi_depth_m)) %>% 
  ggplot(aes(distHaversine_km, secchi_depth_m, col = Year)) +
  facet_wrap(~ DOY) + 
  geom_point(size = 3, alpha = 3/4) +
  lims(x = c(0, 30), y = c(0, 4)) +
  scale_color_viridis_d(end = 0.8)

ee %>% 
  filter(!is.na(ext_coeff_m)) %>% 
  ggplot(aes(distHaversine_km, ext_coeff_m, col = Year)) +
  facet_wrap(~ DOY) + 
  geom_point(size = 3, alpha = 3/4) +
  lims(x = c(0, 30), y = c(0, 5)) +
  scale_color_viridis_d(end = 0.8)

ee %>% 
  filter(!is.na(DOC_mg.L)) %>% 
  ggplot(aes(distHaversine_km, DOC_mg.L, col = Year)) +
  facet_wrap(~ DOY) + 
  geom_point(size = 3, alpha = 3/4) +
  lims(x = c(0, 30)) +
  scale_color_viridis_d(end = 0.8)

eet <- ee %>% mutate(turb_frac = turb_lab_NTU / turb_field_NTU) 

eet %>% 
  ggplot(aes(x = turb_frac)) +
  geom_histogram()

ee %>% select(site_code_long, date_ymd, turb_field_NTU, turb_lab_NTU) %>% View()

eet %>% 
  filter(turb_field_NTU < 60) %>% 
  ggplot(aes(turb_field_NTU, turb_lab_NTU)) + 
  geom_point(alpha = 1/2, size = 3) + 
  geom_abline(intercept = 0) +
  lims(x = c(0, 40), y = c(0, 40))
