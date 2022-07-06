library(readxl)
library(readr) 
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(janitor)
library(patchwork)


# Cleaning part 1 ---------------------------------------------------------

# Use this sample list to match samples in ab_doc_wq & clay_eems
wsa_sample_list_raw <- read_excel("./R_data-cleaning/EEMs/data/raw/doc-eems/2019_3D-EEM_DOC_WSA_fromCam.xlsx", # 97x6
                                  sheet = "Sheet1") 

# Use this file for DOC
# and Chl a, turbidity, Secchi depth, extinction coef 
ab_doc_wq_raw <- read_excel("./R_data-cleaning/EEMs/data/raw/doc-eems/JM-WSA_BP-data-2019.xlsx", # 72x14
                            sheet = "Sheet1")

# Use this for EEMs 
clay_eems_raw <- read_excel("./R_data-cleaning/EEMs/data/raw/doc-eems/Clay_DOC_BuffaloPound_2019_USaskatchewan_BaulchLabandWSA.xlsx",
                                sheet = "DOC_BuffaloPound_2019_USaskatch", skip = 7) 


ab_doc_wq <- ab_doc_wq_raw %>% 
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

wsa_sample_list <- wsa_sample_list_raw %>% # 97x3
  select(1, 2, `DATE SAMPLED`) %>%
  rename(sample_id = `BOTTLE ID`,
         date_ymd = `DATE SAMPLED`,
         site_altname = `SAMPLE DESCRIPTION`) %>% 
  mutate(row_num = row_number(sample_id),
         date_ymd = ymd(date_ymd)) %>% 
  select(row_num, everything())

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

# buoy_doc_eems <- clay_eems %>% # 34x25
#   filter(grepl("m_BP_", sample_id))
# 
# spatial_doc_eems <- clay_eems %>% # 97x25
#   filter(!grepl("m_BP_", sample_id)) %>%
#   mutate(row_num = row_number(sample_id)) %>% 
#   select(-sample_num) %>% 
#   select(row_num, everything())
#
# spatial_doc_eems %>% left_join(wsa_sample_list, by = "row_num") 
# 
# nspatial <- spatial_doc_eems %>% 
#   group_by(date_ymd) %>% 
#   summarise(n_spatial = n())
# 
# nlist <- wsa_sample_list %>% 
#   group_by(date_ymd) %>% 
#   summarise(n_sample_list = n())
# 
# nlist %>% left_join(nspatial, by = "date_ymd")
# 
# spatial_doc_eems %>% filter(is.na(date_ymd)) # 4 NAs + 1 more should be a method blank 
# wsa_sample_list %>% filter(is.na(date_ymd)) # 5 NAs = 5 Method Blank
# 
# # WSA 199 should me labeled as a method blank 
# spatial_doc_eems %>% 
#   filter(sample_id %in% c("WSA 199", "WSA 234", "WSA 253", "WSA 266", "WSA 297"))
#
# spatial_doc_eems %>% 
#   ggplot(aes(date_ymd, A254, group = date_ymd)) + 
#   geom_boxplot()
# 
# spatial_doc_eems_cc <- clay_eems %>% # 97x25
#   filter(!grepl("m_BP_", sample_id)) %>%
#   mutate(row_num = row_number(sample_id)) %>% 
#   select(-c(sample_num, DOC_mg.L)) %>% # use JM DOC
#   select(row_num, everything()) %>% 
#   mutate(date_ymd = as.character(date_ymd),
#          date_ymd = ifelse(sample_id == "WSA 199", NA, date_ymd),
#          date_ymd = ymd(date_ymd))

clay_eems_clean <- wsa_sample_list %>%
  left_join(clay_eems, by = c("sample_id", "row_num", "date_ymd")) %>% 
  select(-c(row_num))

bp_doc_eems_2019 <- clay_eems_clean %>% 
  # left_join(ab_doc_wq, by = "sample_id") %>%
  full_join(ab_doc_wq, by = "sample_id") %>% 
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
         date_ymd_clay = ymd(date_ymd_clay)) %>%
  select(sample_id, site_name, date_ymd = date_ymd_clay, DOC_mg.L, A254, chla_ug.L, 
         A280:PeakT, turb_lab_NTU, turb_field_NTU, secchi_depth_m, ext_coeff_m) 

# clay_eems_clean %>% filter(sample_id == "WSA 213")
# ab_doc_wq %>% filter(sample_id == "WSA 213")
# tmp %>% filter(date_ymd.x == "2019-05-02")
# 
# tmp3 %>% filter(sample_id == "WSA 9991")
# tmp3 %>% filter(is.na(site_name))
# tmp3 %>% filter(is.na(date_ymd) & !site_name == "Method Blank") # so date_ymd_clay = ifelse(is.na(date_ymd_clay, "2019-04-22", date_ymd_clay))
# tmp3 %>% filter(is.na(date_ymd) & !site_name == "Method Blank")
# unique(tmp3$date_ymd)

# bp_doc_eems_2019 %>% 
#   filter(!site_name == "Method Blank" & !is.na(DOC_mg.L)) %>% 
#   ggplot(aes(date_ymd, DOC_mg.L)) + 
#   facet_wrap(~ site_name) +
#   geom_line() + 
#   geom_point()
# 
# bp_doc_eems_2019 %>% 
#   filter(!site_name == "Method Blank" & !is.na(A254)) %>% 
#   ggplot(aes(date_ymd, A254)) + 
#   facet_wrap(~ site_name) +
#   geom_line() + 
#   geom_point()


