library(readxl)
library(readr) 
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(janitor)
library(patchwork)

bp_2015_2016_2017 <- function() {

  eems_doc_2015_2016_2017_raw <- read_excel("./R_EEMs/data/raw/doc-eems/BuffalloPound_DOM_AllData.xlsx",
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
    select(file_name, sheet_name, source, site_name, site_altname, everything())
  
  return(eems_doc_2015_2016_2017)

}

bp_2016 <- function() {
  
  eems_doc_2016_raw <- read_excel("./R_EEMs/data/raw/doc-eems/Helen_2016_DOC&DOM.xlsx",
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
    select(file_name, sheet_name, source, site_name, site_altname, sample_num:sample_id_EEM, everything())
  
  return(eems_doc_2016)
  
}

bp_2017 <- function() {
  
  clay2017_eems_raw <- read_excel("./R_EEMs/data/raw/doc-eems/Copy of Results_DOC samples shipped Jan22 2018.xlsx",
                                  sheet = "Combined")
  
  clay2017_eems1 <- clay2017_eems_raw %>%
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
    select(file_name, sheet_name, source, site_name, site_altname, everything())
  
  return(clay2017_eems1)
  
}

bp_2018 <- function() {
  
  # 2018
  # DOM column names are slightly different than the rest of the data set:
  # HIX Ohno is called "HIX.Ohno" rather than "HIX_Ohno" // ab 
  # Peaks are missing "_RU" // ab
  # 
  # Clay did not provide Peaks e.g. vars ending with "_pc" for 2018 (which we don't need)
  
  bp_2018_raw <- read_excel("R_EEMs/data/raw/doc-eems/2018 - 3D-EEM DOC -WSA sample list.xlsx",
                            col_types = "text")
  
  bp_2018_sample_list_raw <- bp_2018_raw %>% select(1:6)
  
  bp_2018_eems_raw <- bp_2018_raw %>% select(Lab:PeakT)
  
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
    select(lab, site_name, site_altname, everything()) 
  
  return(bp_2018_eems)
  
} 

bp_2019 <- function() {
  
  # 2019 -- issues to fix/check
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
  
  bp_2019_doc_raw <- read_excel("./R_EEMs/data/raw/doc-eems/JM-WSA_BP-data-2019.xlsx",
                                sheet = "Sheet1")
  bp_2019_eems_raw <- read_excel("./R_EEMs/data/raw/doc-eems/Clay_DOC_BuffaloPound_2019_USaskatchewan_BaulchLabandWSA.xlsx",
                                 sheet = "DOC_BuffaloPound_2019_USaskatch", skip = 7) 
  bp_2019_eems_sample_list_raw <- read_excel("./R_EEMs/data/raw/doc-eems/2019_3D-EEM_DOC_WSA_fromCam.xlsx",
                                             sheet = "Sheet1")
  
  # Use A254 from EEMs file
  bp_2019_doc <- bp_2019_doc_raw %>% 
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
           DOC_mg.L = DOC_mg.L) %>% 
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
           file_name = "JM-WSA_BP-data-2019.xlsx",
           sheet_name = "Sheet1", 
           source = "JM") %>% 
  select(file_name, sheet_name, source, site_name, site_altname, date_ymd, DOC_mg.L, turb_lab_NTU, turb_field_NTU, chla_ug.L:ext_coeff_m)
  
  bp_2019_eems_sample_list <- bp_2019_eems_sample_list_raw %>% 
    select(1:2) %>% 
    rename(sample_id = `BOTTLE ID`,
           site_altname = `SAMPLE DESCRIPTION`) %>% 
    mutate(sample_id = factor(sample_id))
  
  bp_2019_eems <- bp_2019_eems_raw %>% 
    rename(sample_num = sample.num,
           sample_id = sample.id,
           date_ymd = sample.date_ymd,
           DOC_mg.L = `DOC(mg/L)`,
           TDN_mg.L = `TDN(mg/L)`,
           HIX_Ohno = HIX.ohno) %>% 
    rename_with(~ paste0(.x, "_RU"), starts_with("Peak")) %>% 
    mutate(date_ymd = excel_numeric_to_date(as.numeric(date_ymd), date_system = "modern"),
           date_ymd = as.character(date_ymd),
           sample_id = factor(sample_id),
           site_altname = ifelse(grepl("0.8m", sample_id), "Buoy 0.8 m", 
                                 ifelse(grepl("2.8m", sample_id), "Buoy 2.8 m", NA)),
           DOC_mg.L = as.numeric(ifelse(DOC_mg.L == "ND", NA, DOC_mg.L)),
           TDN_mg.L = as.numeric(ifelse(TDN_mg.L == "ND", NA, TDN_mg.L)),
           site_altname = ifelse(.$sample_id %in% bp_2019_eems_sample_list$sample_id, bp_2019_eems_sample_list$site_altname, site_altname),
           site_altname = as.character(site_altname),
           site_name = site_altname,
           site_name = ifelse(site_name == "BPL below Qu'Appelle East", "1.5 km below Qu'Appelle R. Inflow East",
                              ifelse(site_name == "BPL below Qu'Appelle Centre", "1.5 km below Qu'Appelle R. Inflow Centre",
                                     ifelse(site_name == "BPL below Qu'Appelle West", "1.5 km below Qu'Appelle R. Inflow West",
                                            ifelse(site_name == "BPL u/s causeway West", "Upstream of Causeway West",
                                                   ifelse(site_name == "BPL u/s causeway Centre", "Upstream of Causeway Centre",
                                                          ifelse(site_name == "BPL u/s causeway East", "Upstream of Causeway East",
                                                                 ifelse(site_name == "BPL below causeway", "0.5 mi Below Causeway",
                                                                        ifelse(site_name == "BPL Parkview", "Opposite Parkview",
                                                                               ifelse(site_name == "BPL Sun Valley", "Opposite Sun Valley",
                                                                                      ifelse(site_name == "BPL South Lake", "Opposite South Lake",
                                                                                             ifelse(site_name == "BPL WTP", "Opposite WTP Intake",
                                                                                                    ifelse(site_name == "BPL above Outlet", "0.5 mi Above Outlet",
                                                                                                           ifelse(site_name == "BP Outlet", "0.5 mi Below Outlet",
                                                                                                                  ifelse(site_name == "Qu'Appelle @ Marquis", "Qu'Appelle River at Marquis",
                                                                                                                         ifelse(site_name == "Qu'Appelle @ HWY19", "Qu'Appelle River at Hwy 19", site_name))))))))))))))),
           file_name = "Clay_DOC_BuffaloPound_2019_USaskatchewan_BaulchLabandWSA.xlsx",
           sheet_name = "DOC_BuffaloPound_2019_USaskatch",
           source = "CJW") %>% 
    mutate_at(vars(S275to295, S350to400, SR, PeakP_RU), as.numeric) %>% 
    select(file_name, sheet_name, source, site_name, site_altname, date_ymd, DOC_mg.L:PeakT_RU) 

  bp_2019 <- bind_rows(bp_2019_doc, bp_2019_eems) %>% 
    mutate(date_ymd_orig = as.character(date_ymd),
           date_ymd = ifelse(date_ymd_orig == "2019-05-02", "2019-04-22", 
                             ifelse(date_ymd_orig == "2019-05-29", "2019-05-28", 
                                    ifelse(date_ymd_orig %in% c("2019-06-10", "2019-06-24"), "2019-06-27", 
                                           ifelse(date_ymd_orig == "2019-07-08", "2019-07-22", 
                                                  ifelse(date_ymd_orig == "2019-08-06", "2019-08-15", 
                                                         ifelse(date_ymd_orig == "2019-08-20", "2019-09-23", 
                                                                ifelse(is.na(date_ymd_orig) & site_name == "Upstream of Causeway Centre", "2019-07-22", date_ymd_orig)))))))) 
  
  return(bp_2019)
  
} 

bp_2015_2016_2017 <- bp_2015_2016_2017()
bp_2016 <- bp_2016()
bp_2017 <- bp_2017()
bp_2018 <- bp_2018() 
bp_2019 <- bp_2019()

# check to see if 2019 DOC and A254 dates match up now... 

# dates
# 1. 0.5 mi Above Outlet // 3 dates ()
# -- 2019-05-02 to 1 ; 
# -- 2019-05-29 to 2 ; 
# -- 2019-08-06 to 5 ;
# 
# 2. 1.5 km below Qu'Appelle River Inflow Centre // 1 date 
# -- 2019-06-24 to 3 ;
# 
# 3. 1.5 km below Qu'Appelle River Inflow East // 2 dates
# -- 2019-06-24 to 3 ;
# -- 2019-08-20 to 6 ;
# 
# 4. Upstream of Causeway Centre // 2 dates
# -- NA to 4 
# -- 2019-08-20 to 6 ;
# 
# 5. Opposite South Lake // 2 dates
# -- 2019-05-02 to 1 ; 
# -- 2019-07-08 to 4 ;
# 
# 6. Opposite Sun Valley // 2 or 3 dates
# -- 2019-05-29 to 2 ; 
# -- 2019-08-06 to 5 ;
# 
# 7. Opposite Parkview // 3 dates 
# -- 2019-05-29 to 2 ; 
# -- 2019-07-08 to 4 ;
# -- 2019-08-20 to 6 ;
# 
# 8. Opposite WTP Intake // 2 dates
# -- 2019-05-02 to 1 ;
# -- 2019-06-10 to 3 ;

# 1. ifelse(date_ymd == "2019-05-02", "2019-04-22", )
# 2. ifelse(date_ymd == "2019-05-29", "2019-05-28", )
# 3. ifelse(date_ymd %in% c("2019-06-10", "2019-06-24"), "2019-06-27", )
# 4. ifelse(date_ymd == "2019-07-08", "2019-07-22", )
# 5. ifelse(date_ymd == "2019-08-06", "2019-06-15", )
# 6. ifelse(date_ymd == "2019-08-20", "2019-09-23", )
# 7. ifelse(is.na(date_ymd) & site_name == "Upstream of Causeway Centre", "2019-07-22")
pa254 <- bp_2019 %>% 
  filter(!grepl("BPWTP", site_name), !grepl("Buoy", site_name),
         !site_name %in% c("Method Blank", "Iskwao Creek", "Iskwao Creek 1", 
                           "Ridge Creek", "Ridge Creek 1", "Pelican Lake", 
                           "Opposite Sun Valley (1 m)", "Opposite Sun Valley (3 m)", 
                           "Buffalo Pound Outlet", "Moose Jaw Creek at TWP RD 184",
                           "Qu'Appelle River at Hwy 19", "Upstream of Causeway West",
                           "0.5 mi Below Causeway West")) %>% 
  mutate(date_ymd = ymd(date_ymd), site_name = factor(site_name),
         A254 = ifelse(is.na(A254), 4, A254)) %>% 
  filter(!site_name %in% c("0.5 mi Below Outlet", "Qu'Appelle River at Marquis")) %>% 
  ggplot(aes(date_ymd, A254, col = source, shape = source)) + 
  facet_wrap(~ site_name) + 
  geom_vline(aes(xintercept = date_ymd), col = "grey10", alpha = 1/2) +
  geom_line() + 
  geom_point(size = 2) + 
  theme(legend.position = "bottom") +
  labs(x = NULL)

pdoc <- bp_2019 %>% 
  filter(!grepl("BPWTP", site_name), !grepl("Buoy", site_name),
         !site_name %in% c("Method Blank", "Iskwao Creek", "Iskwao Creek 1", 
                           "Ridge Creek", "Ridge Creek 1", "Pelican Lake", 
                           "Opposite Sun Valley (1 m)", "Opposite Sun Valley (3 m)", 
                           "Buffalo Pound Outlet", "Moose Jaw Creek at TWP RD 184",
                           "Qu'Appelle River at Hwy 19", "Upstream of Causeway West",
                           "0.5 mi Below Causeway West")) %>% 
  filter(!site_name %in% c("0.5 mi Below Outlet", "Qu'Appelle River at Marquis"),
         source == "JM") %>% 
  mutate(date_ymd = ymd(date_ymd), site_name = factor(site_name)) %>% 
  ggplot(aes(date_ymd, DOC_mg.L)) + 
  facet_wrap(~ site_name) + 
  geom_vline(aes(xintercept = date_ymd), col = "grey10", alpha = 1/2) +
  geom_line(col = "#00BFC4") +
  geom_point(shape = 17, col = "#00BFC4", size = 2) +
  theme(legend.position = "none") +
  labs(x = NULL, y = "DOC",
       subtitle = "2019 DOC and A254 with CJW A254 dates matched to JM DOC dates")

pdoc / pa254


lat_long <- function() {
  
  source("./R_EEMs/code/site-names.R")
  
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



bp_all <- function(write = FALSE, outdir = "./R_EEMs/data/processed/") {
  
  lat_long <- lat_long()
  
  bp_all <- bind_rows(bp_2015_2016_2017, bp_2016, bp_2017, bp_2018, bp_2019) %>% 
    select(-c(latitude, longitude)) %>% 
    mutate(site_name = as.factor(site_name),
           date_ymd = ymd(date_ymd),
           latitude = NA,
           longitude = NA,
           latitude = ifelse(is.na(latitude), lat_long$latitude, latitude),
           longitude = ifelse(is.na(longitude), lat_long$longitude, longitude)) %>%
    select(file_name, sheet_name, source, lab, UVM_id, Aqualog_id, V1, V2, exp_num, 
           DF, sample_num, rep_num, sample_id, sample_id_DOC, sample_id_EEM, 
           bottle_id1, EEMs, Rep, DOC, EEM_contaminated, notes, site_name, 
           site_altname, date_ymd, date_ymd_orig, latitude, longitude, DOC_mg.L, 
           A254, everything()) %>%
    arrange(-desc(date_ymd))
  
  outname <- paste0(Sys.Date(), "_", "bp_DOC_EEMs_processed.csv")
  outpath <- file.path(outdir, outname)
  
  if (isTRUE(write)) {
    write.csv(bp_all, file = outpath)
  }
    
  return(bp_all)
  
}


bp_doc_eems <- function(df = bp_all(), write = FALSE, outdir = "./R_EEMs/data/processed/") {
  
  bpde <- df %>%
    mutate(Year = factor(year(date_ymd)),
           Month = month(date_ymd, label = TRUE, abbr = TRUE),
           DOY = yday(date_ymd),
           site_name = factor(site_name)) %>% 
    filter(!grepl("BPWTP", site_name), 
           !site_name %in% c("Method Blank", "Iskwao Creek", "Iskwao Creek 1", 
                             "Ridge Creek", "Ridge Creek 1", "Pelican Lake", 
                             "Opposite Sun Valley (1 m)", "Opposite Sun Valley (3 m)", 
                             "Buffalo Pound Outlet", "Moose Jaw Creek at TWP RD 184",
                             "Qu'Appelle River at Hwy 19", "Upstream of Causeway West",
                             "0.5 mi Below Causeway West")) %>% 
    select(-c(SUVA, SUVA254)) %>% 
    mutate(A254 = ifelse(file_name == "JM-WSA_BP-data-2019.xlsx", NA, A254)) %>% 
    # mutate(A254_JM = ifelse(source == "JM", A254, NA),
    #        A254_CJW = ifelse(source == "CJW", A254, NA)) %>% 
    # mutate(SUVA = A254 / DOC_mg.L) %>% 
    select(file_name, sheet_name, source, lab, UVM_id, Aqualog_id, V1, V2, exp_num, 
           DF, sample_num, rep_num, sample_id, sample_id_DOC, sample_id_EEM, 
           bottle_id1, EEMs, Rep, DOC, EEM_contaminated, notes, source, site_name,
           site_altname, date_ymd, date_ymd_orig, Year, Month, DOY, latitude, 
           longitude, TDN_mg.L, DOC_mg.L, A254, A280, A350, A440, # A254_CJW, A254_JM,
           S275to295, S350to400, SR, BA, FI, HIX, HIX_Ohno, Fmax, PeakA_RU:PeakT_RU, 
           PeakA_percent:PeakT_percent, turb_lab_NTU, turb_field_NTU, chla_ug.L, 
           secchi_depth_m, ext_coeff_m)
  
  outname <- paste0(Sys.Date(), "_", "bp_DOC_EEMs_processed-select-sites.csv")
  outpath <- file.path(outdir, outname)
  
  if (isTRUE(write)) {
    write.csv(bpde, file = outpath)
  }
  
  return(bpde)
  
}





# Where is there missing data? --------------------------------------------

eems <- bp_doc_eems()

# DOC 
eems %>% 
  group_by(Year) %>% 
  filter(is.na(DOC_mg.L))

# lat/long
eems %>% filter(is.na(latitude)) 



# Preliminary plots -------------------------------------------------------

eems <- bp_doc_eems()

# HIX <0.6 may be sign of issues, filter >0.6
eems %>% 
  filter(HIX_Ohno > 0.6 & !is.na(date_ymd)) %>% 
  ggplot(aes(BA, HIX_Ohno, colour = Year, shape = Year)) +
  geom_point(size = 2.5) +
  facet_wrap(~ Year, ncol = 5) + 
  theme(legend.position = "bottom")

# HIX_Ohno with time
eems %>% 
  filter(HIX_Ohno > 0.6) %>% 
  ggplot(aes(DOY, HIX_Ohno, colour = source)) +
  geom_point() +
  facet_wrap(~ Year)


eems %>% 
  filter(A254 > 1, A254 < 25, !is.na(A254), !is.na(DOC_mg.L), !is.na(date_ymd)) %>% 
  ggplot(aes(DOC_mg.L, A254, colour = site_name, shape = Year)) + 
  geom_point(size = 2.5) +
  facet_wrap(~ Year, ncol = 4) + 
  theme(legend.position = "bottom")

eems %>% 
  filter(A254 < 1) %>% 
  ggplot(aes(date_ymd, A254)) +
  geom_point()

# 3 instruments used, Iowa State, Brockport, UVM
eems %>% 
  filter(A254 > 5, A254 < 15, !is.na(date_ymd)) %>% 
  ggplot(aes(A254, A280, colour = Year, shape = Year)) + 
  geom_point(size = 2.5) +
  facet_wrap(~ Year, ncol = 5) + 
  theme(legend.position = "bottom")

eems %>% 
  filter(!is.na(date_ymd), !Year == "2015", S275to295 < 0.04) %>% 
  ggplot(aes(DOC_mg.L, S275to295, colour = Year, shape = Year)) + 
  geom_point(size = 2.5) +
  facet_wrap(~ Year, ncol = 5) + 
  theme(legend.position = "bottom")

# remove > 0.04
eems %>% 
  filter(S275to295 < 0.04) %>% 
  ggplot(aes(HIX, S275to295)) +
  geom_point()

eems %>% 
  filter(S275to295 < 0.04) %>% 
  ggplot(aes(S350to400, S275to295, colour = Year, shape = Year)) + 
  geom_point(size = 2.5) +
  facet_wrap(~ Year, ncol = 5) + 
  theme(legend.position = "bottom")


# could be useful to separate tributaries from lake samples
# SUVA normal if <5 and >1 
eems %>% 
  filter(HIX_Ohno > 0.6) %>% 
  ggplot(aes(HIX_Ohno, SUVA, colour = Year, shape = Year)) + 
  geom_point(size = 2.5) +
  facet_wrap(~ Year, ncol = 5) + 
  theme(legend.position = "bottom")

eems %>% 
  filter(Year == 2019) %>% 
  ggplot(aes(date_ymd, A254)) + 
  geom_point() 

