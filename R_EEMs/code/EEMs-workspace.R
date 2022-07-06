# 2021-10-01


library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(lubridate)
library(ggplot2)
library(janitor)

theme_set(theme_bw())


# Raw data ----------------------------------------------------------------

wsa2019_raw <- read_excel("./data/raw/2019/JM-WSA_BP-data-2019.xlsx")

clay2019_eems_raw <- read_excel("./data/raw/2019/Clay_DOC_BuffaloPound_2019_USaskatchewan_BaulchLabandWSA.xlsx",
                                skip = 7) 

clay2019_eems_sample_list_raw <- read_excel("./data/raw/2019/2019_3D-EEM_DOC_WSA_fromCam.xlsx")  

clay2017_eems_raw <- read_excel("./data/raw/2017/Copy of Results_DOC samples shipped Jan22 2018.xlsx",
                                sheet = "Combined")

eems_doc_2016_raw <- read_excel("./data/raw/2016/Helen_2016_DOC&DOM.xlsx",
                                sheet = "Combined Needs Verification")



# Site names --------------------------------------------------------------

site_names_all <- tribble(
  ~site_name,                                  ~site_altname,                        
  "Upstream of Causeway West",                 "US Causeway West",                    
  "Upstream of Causeway West",                 "Upstream of Causeway- WEST",          
  "Upstream of Causeway West",                 "Upstream of Causeway - WEST",  
  "Upstream of Causeway West",                 "u/s causeway West",    
  "Upstream of Causeway West",                 "Us Causeway West",
  "Upstream of Causeway East",                 "Upstream of Causeway- EAST",
  "Upstream of Causeway East",                 "Upstream of Causeway - EAST",
  "Upstream of Causeway East",                 "Us Causeway East",
  "Upstream of Causeway East",                 "US Causeway East",
  "Upstream of Causeway East",                 "u/s causeway East",
  "Upstream of Causeway Centre",               "Upstream of Causeway",
  "Upstream of Causeway Centre",               "Upstream Causeway",
  "Upstream of Causeway Centre",               "US Causeway",
  "Upstream of Causeway Centre",               "US Causeway",
  "Upstream of Causeway Centre",               "u/s causeway Centre",
  "0.5 mi Below Causeway",                     "below causeway",
  "0.5 mi Below Causeway",                     "0.5mi Below Causeway",
  "0.5 mi Below Causeway",                     "DS Causeway",
  "0.5 mi Below Causeway West",                "0.5 mi Below Causeway - WEST",  
  "0.5 mi Below Causeway West",                "0.5 mi ds Causeway - WEST",
  "Opposite WTP Intake",                       "Opp Treatment Plant Intake",
  "Opposite WTP Intake",                       "WTP",
  "Opposite WTP Intake",                       "Opp WTP",
  "Opposite WTP Intake",                       "Opp. WTP",
  "0.5 mi Above Outlet",                       "above Outlet",
  "0.5 mi Above Outlet",                       "Above Outlet",
  "0.5 mi Above Outlet",                       "0.5mi above Outlet",
  "0.5 mi Above Outlet",                       "0.5 mi above Outlet",
  "0.5 mi Above Outlet",                       "0.5 mi Abore Oulet",
  "0.5 mi Above Outlet",                       "0.5 mi above Oulet",
  "0.5 mi Above Outlet",                       "0.5 mi above outlet",
  "0.5 mi Below Outlet",                       "Buffalo Pound Outlet",
  "0.5 mi Below Outlet",                       "BP Outlet",
  "1.5 km below Qu'Appelle R. Inflow Centre",  "1.5 km below Qu'Appelle R. Inflow",
  "1.5 km below Qu'Appelle R. Inflow Centre",  "below Qu'Appelle Center",
  "1.5 km below Qu'Appelle R. Inflow Centre",  "Below Qu'Appelle",
  "1.5 km below Qu'Appelle R. Inflow Centre",  "below Qu'Appelle",
  "1.5 km below Qu'Appelle R. Inflow Centre",  "below Qu'Appelle Centre",
  "1.5 km below Qu'Appelle R. Inflow West",    "Below Qu'Appelle West",
  "1.5 km below Qu'Appelle R. Inflow West",    "below Qu'Appelle West",
  "1.5 km below Qu'Appelle R. Inflow West",    "1.5 km below Qu'Appelle R. Inflow - WEST",
  "1.5 km below Qu'Appelle R. Inflow West",    "1.5 km below Qu’Appelle R. Inflow- WEST",
  "1.5 km below Qu'Appelle R. Inflow East",    "Below Qu'Appelle East",
  "1.5 km below Qu'Appelle R. Inflow East",    "below Qu'Appelle East",
  "1.5 km below Qu'Appelle R. Inflow East",    "1.5 km below Qu'Appelle R. Inflow - EAST",
  "1.5 km below Qu'Appelle R. Inflow East",    "1.5 km below Qu’Appelle R. Inflow- EAST",
  "Opposite South Lake",                       "South Lake",
  "Opposite South Lake",                       "Opp South Lake",
  "Opposite South Lake",                       "Opp. South Lake",
  "Opposite Sun Valley",                       "Sun Valley",
  "Opposite Sun Valley",                       "Opp Sun Valley",
  "Opposite Sun Valley",                       "Opp. Sun Valley",
  "Opposite Parkview",                         "Parkview",
  "Opposite Parkview",                         "Opp Parkview",
  "Opposite Parkview",                         "Opp. Parkview",
  "Qu'Appelle River at Marquis",               "Qu'Appelle at Marquis",
  "Qu'Appelle River at Marquis",               "Qu'Appelle R at Marquis",
  "Qu'Appelle River at Marquis",               "Qu'Appelle R @ Marquis",
  "Qu'Appelle River at Marquis",               "Qu'Appelle @ Marquis",
  "Qu'Appelle River at Hwy 19",                "Qu'Appelle at Highway 19",
  "Qu'Appelle River at Hwy 19",                "Qu'Appelle R at Hwy 19",
  "Qu'Appelle River at Hwy 19",                "Qu'Appelle @ HWY19", 
  "Qu'Appelle River at Hwy 19",                "Qu’Appelle R @ Hwy#19", 
  "Iskwao Creek",                              "Iskwao Creek", 
  "Iskwao Creek 1",                            "Iskwao Creek 1", 
  "Ridge Creek",                               "Ridge Creek ",
  "Ridge Creek 1",                             "Ridge Creek 1",
  "Moose Jaw Creek at TWP RD 184",             "Moose Jaw Creek at TWP RD 184",
  "Opposite Sun Valley (1 m)",                 "Opp Sun Valley (1m)",
  "Opposite Sun Valley (3 m)",                 "Opp Sun Valley (3m)"
)

site_names_clean <- site_names_all %>% 
  distinct(site_name) %>% 
  mutate(station_id = ifelse(site_name == "Upstream of Causeway West", "SK05JG0222",
                             ifelse(site_name == "Upstream of Causeway East", "SK05JG0223",
                                    ifelse(site_name == "Upstream of Causeway Centre", "SK05JG0219",
                                           ifelse(site_name == "0.5 mi Below Causeway", "SK05JG0094",
                                                  ifelse(site_name == "0.5 mi Below Causeway West", NA,
                                                         ifelse(site_name == "Opposite WTP Intake", "SK05JG0102",
                                                                ifelse(site_name == "0.5 mi Above Outlet", "SK05JG0104",
                                                                       ifelse(site_name == "0.5 mi Below Outlet", NA,
                                                                              ifelse(site_name == "1.5 km below Qu'Appelle R. Inflow Centre", "SK05JG0220",
                                                                                     ifelse(site_name == "1.5 km below Qu'Appelle R. Inflow West", "SK05JG0224",
                                                                                            ifelse(site_name == "1.5 km below Qu'Appelle R. Inflow East", "SK05JG0225",
                                                                                                   ifelse(site_name == "Opposite South Lake", "SK05JG0095",
                                                                                                          ifelse(site_name %in% c("Opposite Sun Valley", "Opposite Sun Valley (1 m)", "Opposite Sun Valley (3 m)"), "SK05JG0096",
                                                                                                                 ifelse(site_name == "Opposite Parkview", "SK05JG0098",
                                                                                                                        ifelse(site_name == "Qu'Appelle River at Marquis", "SK05JG0120",
                                                                                                                               ifelse(site_name == "Qu'Appelle River at Hwy 19", "SK05HF0206",
                                                                                                                                      ifelse(site_name %in% c("Iskwao Creek", "Iskwao Creek 1"), "SK05JG014",
                                                                                                                                             ifelse(site_name %in% c("Ridge Creek", "Ridge Creek 1"), "SK05JG013",
                                                                                                                                                    ifelse(site_name == "Moose Jaw Creek at TWP RD 184", NA, NA))))))))))))))))))))


# wsa2019_raw -------------------------------------------------------------

wsa2019_1 <- wsa2019_raw %>% 
  rename(sample_num = sample.number,
         datetime_ymdhms = date.time,
         sample_id = sample.id,
         a254_unit1 = abs254...4,
         station_id = station.number,
         site_altname = station.name,
         sample_depth_m = sampling.depth_m,
         turb_lab_NTU = lab.turbidity_NTU,
         chla_ug.L = chlorohyl.a_ug.L,
         secchi_depth_m = secchi.depth_m,
         ext_coeff_m = extinction.coeff_.m,
         turb_field_NTU = field.turbidity_NTU,
         DOC_mg.L = DOC_mg.L,
         a254_unit2 = abs254...14) %>% 
  mutate(turb_lab_NTU = as.numeric(turb_lab_NTU)) %>% 
  mutate_if(is.character, as.factor)

# chla_ug.L and secchi_depth_m each have rows with > or < 
# For now I'll make a separate df with the flagged rows and just remove them 
# from the wsa2019_1 df.
# Clean up site_name col. Remove "Buffalo Pound Lake" 

wsa2019_flagged <- wsa2019_1 %>% filter(grepl("<", chla_ug.L) | grepl(">", secchi_depth_m))

wsa2019_2 <- wsa2019_1 %>% 
  mutate(chla_ug.L = ifelse(grepl("<", chla_ug.L), NA, chla_ug.L),
         secchi_depth_m = ifelse(grepl(">", secchi_depth_m), NA, secchi_depth_m)) %>% 
  mutate(site_altname = as.character(site_altname),
         site_altname = str_replace(site_altname, "Buffalo Pound Lake ", "")) 

# Check if both A254 are the same, and if so delete second one
wsa2019_3 <- wsa2019_2 %>% 
  mutate(a254_diff = a254_unit1 - a254_unit2) %>% # looks good
  select(-c(a254_unit2, a254_diff)) %>% 
  rename(A254 = a254_unit1)

# Plot DOC 
wsa2019_3 %>% 
  ggplot(aes(datetime_ymdhms, DOC_mg.L)) + 
  facet_wrap(~ station_id + site_altname) +
  geom_point()

wsa2019_4 <- wsa2019_3 %>% 
  mutate(site_name = ifelse(site_altname == "US Causeway West" | site_altname == "Upstream of Causeway- WEST", "Upstream of Causeway West", # 0222
                               ifelse(site_altname == "US Causeway East" | site_altname == "Upstream of Causeway- EAST", "Upstream of Causeway East", # 0223
                                      ifelse(site_altname == "US Causeway Center" | site_altname == "Upstream of Causeway", "Upstream of Causeway Centre", # 0219
                                             ifelse(site_altname == "Opp Treatment Plant Intake" | site_altname == "Opp WTP", "Opposite WTP Intake", # 0102
                                                    ifelse(site_altname == "Above Outlet" | site_altname == "0.5mi above Outlet", "0.5 mi Above Outlet", # 0104
                                                           ifelse(site_altname == "0.5mi Below Causeway" | site_altname == "DS Causeway", "0.5 mi Below Causeway", # 0094
                                                                         ifelse(site_altname == "1.5 km below Qu'Appelle R. Inflow" | site_altname == "below Qu'Appelle Center", "1.5 km below Qu'Appelle R. Inflow Centre", # 0220
                                                                                ifelse(site_altname == "1.5 km below Qu'Appelle R. Inflow - WEST" | site_altname == "below Qu'Appelle West", "1.5 km below Qu'Appelle R. Inflow West", # 0224
                                                                                       ifelse(site_altname == "1.5 km below Qu'Appelle R. Inflow - EAST" | site_altname == "below Qu'Appelle East", "1.5 km below Qu'Appelle R. Inflow East", # 0025
                                                                                              ifelse(site_altname == "Opp South Lake", "Opposite South Lake", # 0095
                                                                                                     ifelse(site_altname == "Opp Sun Valley", "Opposite Sun Valley", # 0096
                                                                                                            ifelse(site_altname == "Opp Parkview", "Opposite Parkview", site_altname))))))))))))) # 0098


wsa2019_4 %>% 
  ggplot(aes(datetime_ymdhms, DOC_mg.L, col = site_name)) + 
  geom_line() +
  geom_point(col = "white", size = 2.75) +
  geom_point() +
  labs(x = "Date (2019)", y = "DOC (mg/L)")

# Plot parameters                                                                                                          
wsa2019_4 %>% 
  ggplot(aes(datetime_ymdhms, DOC_mg.L)) + 
  facet_wrap(~ station_id + site_name) +
  geom_line() +
  geom_point(col = "white", size = 2.75) +
  geom_point() +
  labs(x = "Date (2019)", y = "DOC (mg/L)")
  
wsa2019_4 %>% 
  ggplot(aes(datetime_ymdhms, A254)) + 
  facet_wrap(~ station_id + site_name) +
  geom_line() +
  geom_point(col = "white", size = 2.75) +
  geom_point() +
  labs(x = "Date (2019)", y = "Absorbance at 254 nm")

wsa2019_4 %>% 
  ggplot(aes(datetime_ymdhms, turb_lab_NTU)) + 
  facet_wrap(~ station_id + site_name) +
  geom_line() +
  geom_point(col = "white", size = 2.75) +
  geom_point() +
  labs(x = "Date (2019)", y = "Lab turbidity (NTU)")

wsa2019_4 %>% 
  ggplot(aes(datetime_ymdhms, turb_field_NTU)) + 
  facet_wrap(~ station_id + site_name) +
  geom_line() +
  geom_point(col = "white", size = 2.75) +
  geom_point() +
  labs(x = "Date (2019)", y = "Field turbidity (NTU)")

wsa2019_4 %>% 
  ggplot(aes(datetime_ymdhms, chla_ug.L)) + 
  facet_wrap(~ station_id + site_name) +
  geom_line() +
  geom_point(col = "white", size = 2.75) +
  geom_point() +
  labs(x = "Date (2019)", y = "Chl a (µg/L)")

wsa2019_4 %>% 
  ggplot(aes(datetime_ymdhms, secchi_depth_m)) + 
  facet_wrap(~ station_id + site_name) +
  geom_line() +
  geom_point(col = "white", size = 2.75) +
  geom_point() +
  labs(x = "Date (2019)", y = "Secchi depth (m)")

wsa2019_4 %>% 
  ggplot(aes(datetime_ymdhms, ext_coeff_m)) + 
  facet_wrap(~ station_id + site_name) +
  geom_line() +
  geom_point(col = "white", size = 2.75) +
  geom_point() +
  labs(x = "Date (2019)", y = "Extinction coefficient (m)")



# clay2019_eems & clay2019_eems_sample_list -------------------------------

clay2019_eems1 <- clay2019_eems_raw %>% 
  rename(sample_num = sample.num,
         sample_id = sample.id,
         date_ymd = sample.date_ymd,
         DOC_mg.L = `DOC(mg/L)`,
         TDN_mg.L = `TDN(mg/L)`,
         HIX_Ohno = HIX.ohno) %>% 
  mutate(date_ymd = excel_numeric_to_date(as.numeric(date_ymd), date_system = "modern"),
         sample_id = factor(sample_id),
         site_altname = ifelse(grepl("0.8m", sample_id), "Buoy 0.8 m", 
                            ifelse(grepl("2.8m", sample_id), "Buoy 2.8 m", NA)),
         DOC_mg.L = as.numeric(ifelse(DOC_mg.L == "ND", NA, DOC_mg.L)),
         TDN_mg.L = as.numeric(ifelse(TDN_mg.L == "ND", NA, TDN_mg.L))) %>% 
  select(sample_num, sample_id, site_altname, everything()) 
  
clay2019_eems_sample_list <- clay2019_eems_sample_list_raw %>% 
  select(1:2) %>% 
  rename(sample_id = `BOTTLE ID`,
         site_altname = `SAMPLE DESCRIPTION`) %>% 
  mutate(sample_id = factor(sample_id))

clay2019_eems2 <- clay2019_eems1 %>% 
  mutate(site_altname = ifelse(clay2019_eems1$sample_id %in% clay2019_eems_sample_list$sample_id, clay2019_eems_sample_list$site_altname, site_altname),
         site_altname = str_replace(site_altname, "BPL ", ""),
         site_altname = as.character(site_altname),
         site_name = ifelse(site_altname == "below Qu'Appelle East", "1.5 km below Qu'Appelle R. Inflow East",
                            ifelse(site_altname == "below Qu'Appelle Centre", "1.5 km below Qu'Appelle R. Inflow Centre",
                                   ifelse(site_altname == "below Qu'Appelle West", "1.5 km below Qu'Appelle R. Inflow West",
                                          ifelse(site_altname == "u/s causeway West", "Upstream of Causeway West",
                                                 ifelse(site_altname == "u/s causeway Centre", "Upstream of Causeway Centre",
                                                        ifelse(site_altname == "u/s causeway East", "Upstream of Causeway East",
                                                               ifelse(site_altname == "below causeway", "0.5 mi Below Causeway",
                                                                      ifelse(site_altname == "Parkview", "Opposite Parkview",
                                                                             ifelse(site_altname == "Sun Valley", "Opposite Sun Valley",
                                                                                    ifelse(site_altname == "South Lake", "Opposite South Lake",
                                                                                           ifelse(site_altname == "WTP", "Opposite WTP Intake",
                                                                                                  ifelse(site_altname == "above Outlet", "0.5 mi Above Outlet",
                                                                                                         ifelse(site_altname == "BP Outlet", "0.5 mi Below Outlet",
                                                                                                                ifelse(site_altname == "Qu'Appelle @ Marquis", "Qu'Appelle River at Marquis",
                                                                                                                       ifelse(site_altname == "Qu'Appelle @ HWY19", "Qu'Appelle River at Hwy 19", site_altname))))))))))))))),
         site_name = factor(site_name)) %>% 
  mutate_if(is.character, as.numeric)


clay2019_eems2 %>% 
  filter(grepl("Buoy", site_name)) %>% 
  ggplot(aes(date_ymd, DOC_mg.L, colour = site_name)) +
  geom_line() +
  geom_point(col = "white", size = 2.75) +
  geom_point() +
  labs(x = "Date (2019)")

clay2019_eems2 %>% 
  filter(grepl("Buoy", site_name)) %>% 
  ggplot(aes(date_ymd, A254, colour = site_name)) +
  geom_line() +
  geom_point(col = "white", size = 2.75) +
  geom_point() +
  labs(x = "Date (2019)")

clay2019_eems2 %>% 
  filter(!grepl("Buoy", site_name)) %>% 
  ggplot(aes(date_ymd, A254)) +
  facet_wrap(~ site_name) +
  geom_line() +
  geom_point(col = "white", size = 2.75) +
  geom_point() +
  labs(x = "Date (2019)")

  
clay2019_eems2 %>% 
  filter(grepl("Buoy", site_name)) %>% 
  ggplot(aes(date_ymd, TDN_mg.L, colour = site_name)) +
  geom_line() +
  geom_point(col = "white", size = 2.75) +
  geom_point() +
  labs(x = "Date (2019)")

clay2019_eems2 %>% 
  filter(!grepl("Buoy", site_name)) %>% 
  ggplot(aes(date_ymd, A254, colour = site_name)) +
  geom_line() +
  geom_point(col = "white", size = 2.75) +
  geom_point() +
  labs(x = "Date (2019)")


# clay2017_eems_raw -------------------------------------------------------

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
  mutate(site_altname = str_replace(site_altname, "BPL ", ""),
         site_altname = str_replace(site_altname, "Buffalo Pound Lake ", ""),
         site_name = ifelse(site_altname == "Below Qu'Appelle East" | site_altname == "below Qu'Appelle East", "1.5 km below Qu'Appelle R. Inflow East",
                            ifelse(site_altname == "below Qu'Appelle" | site_altname == "Below Qu'Appelle", "1.5 km below Qu'Appelle R. Inflow Centre",
                                   ifelse(site_altname == "below Qu'Appelle West" | site_altname == "Below Qu'Appelle West", "1.5 km below Qu'Appelle R. Inflow West",
                                          ifelse(site_altname == "Us Causeway West" | site_altname == "US Causeway West", "Upstream of Causeway West",
                                                 ifelse(site_altname == "Upstream Causeway" | site_altname == "US Causeway", "Upstream of Causeway Centre",
                                                        ifelse(site_altname == "Us Causeway East" | site_altname == "US Causeway East", "Upstream of Causeway East",
                                                               ifelse(site_altname == "0.5 mi below Causeway", "0.5 mi Below Causeway",
                                                                      ifelse(site_altname == "Opp Parkview" | site_altname == "Opp. Parkview", "Opposite Parkview",
                                                                             ifelse(site_altname == "Opp Sun Valley" | site_altname == "Opp. Sun Valley", "Opposite Sun Valley",
                                                                                    ifelse(site_altname == "Opp South Lake" | site_altname == "Opp. South Lake", "Opposite South Lake",
                                                                                           ifelse(site_altname == "Opp WTP" | site_altname == "Opp. WTP", "Opposite WTP Intake",
                                                                                                  ifelse(site_altname == "0.5 mi Abore Oulet" | site_altname == "0.5 mi above Oulet" | site_altname == "0.5 mi above outlet", "0.5 mi Above Outlet",
                                                                                                         ifelse(site_altname == "Buffalo Pound Outlet", "0.5 mi Below Outlet",
                                                                                                                ifelse(site_altname == "Qu'Appelle at Marquis" | site_altname == "Qu'Appelle R at Marquis", "Qu'Appelle River at Marquis",
                                                                                                                       ifelse(site_altname == "Qu'Appelle R at Hwy 19" | site_altname == "Qu'Appelle at Highway 19", "Qu'Appelle River at Hwy 19",
                                                                                                                              ifelse(sample_id == "WSA 001", "Qu'Appelle River at Hwy 19",
                                                                                                                                     ifelse(site_altname == "Raw", "BPWTP Raw",
                                                                                                                                            ifelse(site_altname == "BPTWP Q5", "BPWTP Q5", site_altname)))))))))))))))))),
         grouping = ifelse(site_name %in% c("Qu'Appelle River at Hwy 19", "Qu'Appelle River at Marquis"), "Roadside",
                           ifelse(site_name %in% c("Ridge Creek", "Iskwao Creek"), "Tributaries",
                                  ifelse(site_name %in% c("0.5 mi Below Outlet", "0.5 mi Above Outlet"), "Outlet",
                                         ifelse(site_name %in% c("Upstream of Causeway Centre", "Upstream of Causeway East", "Upstream of Causeway West", "0.5 mi Below Causeway"), "Causeway",
                                               ifelse(site_name %in% c("1.5 km below Qu'Appelle R. Inflow East", "1.5 km below Qu'Appelle R. Inflow West", "1.5 km below Qu'Appelle R. Inflow"), "Inflow",
                                                      ifelse(site_name %in% c("BPWTP Raw", "BPWTP Q2 D1", "BPWTP Q3", "BPWTP Q5", "BPWTP Q6", "BPWTP Q2 D2"), "BPWTP",
                                                             ifelse(site_name %in% c("Opposite WTP Intake", "Opposite Sun Valley", "Opposite South Lake", "Opposite Parkview"), "Other", NA))))))))


clay2017_eems1 %>% 
  filter(!site_name %in% c("Method Blank", "Not Provided")) %>% 
  filter(!is.na(site_name)) %>% 
  ggplot(aes(date_ymd, DOC_mg.L, colour = factor(site_name))) +
  geom_line() +
  geom_point(col = "white", size = 2.75) +
  geom_point() +
  labs(x = "Date (2017)") +
  theme(legend.position = "bottom")

clay2017_eems1 %>% 
  filter(!site_name %in% c("Method Blank", "Not Provided")) %>% 
  filter(!is.na(site_name)) %>% 
  filter(!grouping == "BPWTP") %>% 
  ggplot(aes(date_ymd, DOC_mg.L, colour = factor(site_name))) +
  facet_wrap(~ grouping, scales = "free_y") +
  geom_line() +
  geom_point(col = "white", size = 2.75) +
  geom_point() +
  labs(x = "Date (2017)") +
  theme(legend.position = "bottom")



# eems_doc_2016_raw -------------------------------------------------------

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
  mutate(site_altname = str_replace(site_altname, "Buffalo Pound Lake ", ""),
         site_name = ifelse(site_altname == "0.5 mi above Outlet", "0.5 mi Above Outlet",
                            ifelse(site_altname %in% c("0.5 mi Below Causeway - WEST", "0.5 mi ds Causeway - WEST"),  "0.5 mi Below Causeway West",
                                   ifelse(site_altname == "1.5 km below Qu’Appelle R. Inflow", "1.5 km below Qu'Appelle R. Inflow Centre",
                                          ifelse(site_altname == "1.5 km below Qu’Appelle R. Inflow- EAST", "1.5 km below Qu'Appelle R. Inflow East",
                                                 ifelse(site_altname == "1.5 km below Qu’Appelle R. Inflow- WEST", "1.5 km below Qu'Appelle R. Inflow West",
                                                        ifelse(site_altname == 	"Buffalo Pound Outlet", "0.5 mi Below Outlet",
                                                               ifelse(site_altname == "Opp Parkview", "Opposite Parkview",
                                                                      ifelse(site_altname == "Opp South Lake", "Opposite South Lake",
                                                                             ifelse(site_altname == "Opp Sun Valley", "Opposite Sun Valley",
                                                                                    ifelse(site_altname == "Opp Sun Valley (1m)", "Opposite Sun Valley (1 m)",
                                                                                           ifelse(site_altname == "Opp Sun Valley (3m)", "Opposite Sun Valley (3 m)",
                                                                                                  ifelse(site_altname == 	"Opp Treatment Plant Intake", "Opposite WTP Intake",
                                                                                                         ifelse(site_altname == "Qu’Appelle R @ Hwy#19", "Qu'Appelle River at Hwy 19", 
                                                                                                                ifelse(site_altname == "Qu’Appelle R @ Marquis", "Qu'Appelle River at Marquis",
                                                                                                                       ifelse(site_altname == "Upstream of Causeway - EAST", "Upstream of Causeway East",
                                                                                                                              ifelse(site_altname == "Upstream of Causeway - WEST", "Upstream of Causeway West", site_altname)))))))))))))))),
         S350to400 = as.numeric(S350to400),
         SR = as.numeric(SR)) %>% 
  select(sample_num:sample_id_EEM, site_name, everything())
  

