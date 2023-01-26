library(readxl)
library(readr) 
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(janitor)
library(patchwork)

theme_set(theme_bw(base_size = 14))

wq2018_raw <- read_excel("./R_data-cleaning/EEMs/data/raw/doc-eems/BP 2018 data.xlsx")

names(wq2018_raw)
unique(wq2018_raw$Variable) 
unique(wq2018_raw$Station.Name)
# "BUFFALO POUND LK.-0.5MI. BELOW CAUSEWAY; OPP."        | # Below Causeway           | SK05JG0094
# "BUFFALO POUND LK.-3MI. BELOW CAUSEWAY;OPP.START OF"   | # Opposite South Lake      | SK05JG0095
# "BUFFALO POUND LK.-OPP. SUN VALLEY;OPP.ENTRANCE RD."   | # Opposite Sun Valley      | SK05JG0096
# "BUFFALO POUND LK.-10MI.BELOW CAUSEWAY OPP.PROV.PK."   | # Opposite Parkview        | SK05JG0098
# "BUFFALO POUND LK.-OPP. PLANT INTAKE @ MID-LK. (B8"    | # WTP Intake               | SK05JG0102
# "BUFFALO POUND LK.-0.5MI.ABOVE OUTLET FROM BPL @MID"   | # Above Outlet             | SK05JG0104
# "BUFFALO POUND LK.-UPSTRM OLD HWY 2 CAUSEWAY (SWA)"    | # Upstream Causeway Centre | SK05JG0219
# "BUFFALO POUND LK.-UPSTRM OF CSWY E. SIDE (SWA)"       | # Upstream Causeway East   | SK05JG0223
# "BUFFALO POUND LK.-UPSTRM OF CSWY W. SIDE (SWA)"       | # Upstream Causeway West   | SK05JG0222
# "BUFFALO POUND LK.-1.5KM DNSTRM QU'APPELLE W. (SWA)"   | # Inflow West              | SK05JG0224
# "BUFFALO POUND LK.-1.5KM DNSTRM QU'APPELLE E. (SWA)"   | # Inflow East              | SK05JG0225

ww <- wq2018_raw %>% 
  select(Sample.Number, Station.Number, Station.Name, Sample.Date, Variable,
         Value, Unit) %>% 
  filter(Variable %in% c("CHLOROPHYLL A",                             # MG/M3 (mg/m^3)
                         "TURBIDITY (FIELD)",                         # NTU
                         "TURBIDITY LIGHT PENETRN. SECCHI DSC.",      # M (m)
                         "Photosynthetically Active Radiation"),      # m-1 (m^-1)
         !Station.Number == "SK05JG0219") %>%  
  mutate(site_code_long = case_when(
    Station.Number == "SK05JG0094" ~ "Below Causeway",
    Station.Number == "SK05JG0095" ~ "Opposite South Lake",
    Station.Number == "SK05JG0096" ~ "Opposite Sun Valley",
    Station.Number == "SK05JG0098" ~ "Opposite Parkview",
    Station.Number == "SK05JG0102" ~ "WTP Intake",
    Station.Number == "SK05JG0104" ~ "Above Outlet",
    Station.Number == "SK05JG0223" ~ "Upstream Causeway East",
    Station.Number == "SK05JG0222" ~ "Upstream Causeway Centre",
    Station.Number == "SK05JG0224" ~ "Inflow West",
    Station.Number == "SK05JG0225" ~ "Inflow East"
  )) 

# Chlorophyll a
ww %>% 
  filter(Variable == "CHLOROPHYLL A") %>% 
  ggplot(aes(Sample.Date, Value)) +
  facet_wrap(~ site_code_long) +
  geom_point(size = 2, alpha = 3/4, col = 'blue')
  
# Turbidity
ww %>% 
  filter(Variable == "TURBIDITY (FIELD)") %>% 
  ggplot(aes(Sample.Date, Value)) +
  facet_wrap(~ site_code_long) +
  geom_point(size = 2, alpha = 3/4, col = 'blue')

# Secchi depth
ww %>% 
  filter(Variable == "TURBIDITY LIGHT PENETRN. SECCHI DSC.") %>% 
  ggplot(aes(Sample.Date, Value)) +
  facet_wrap(~ site_code_long) +
  geom_point(size = 2, alpha = 3/4, col = 'blue')

# PAR
ww %>% 
  filter(Variable == "Photosynthetically Active Radiation") %>% 
  ggplot(aes(Sample.Date, Value)) +
  facet_wrap(~ site_code_long) +
  geom_point(size = 2, alpha = 3/4, col = 'blue')

ww2 <- ww %>% 
  select(station_num  = Station.Number,
         date_ymd     = Sample.Date,
         parameter    = Variable,
         result       = Value,
         site_code_long) %>% 
  mutate(parameter = case_when(
    parameter == "CHLOROPHYLL A" ~ "chla_ug.L",
    parameter == "TURBIDITY (FIELD)" ~ "turb_field_NTU", 
    parameter == "TURBIDITY LIGHT PENETRN. SECCHI DSC." ~ "secchi_depth_m",
    parameter == "Photosynthetically Active Radiation" ~ "ext_coeff_m")) %>% 
  mutate(date_ymd = substr(date_ymd, start = 0, stop = 10),
         date_ymd = ymd(date_ymd))


# Upstream Causeway sites
uu <- subset(ww2, grepl("Upstream Causeway", site_code_long))
unique(uu$site_code_long) # 2

CU <- uu %>% group_by(parameter, date_ymd) %>% 
  summarise(result = mean(result, na.rm = TRUE)) %>% 
  mutate(site_code_long = "Upstream Causeway")
 
# Inflow sites
ff <- subset(ww2, grepl("Inflow", site_code_long))
unique(ff$site_code_long) # 2

LI <- ff %>% group_by(parameter, date_ymd) %>% 
  summarise(result = mean(result, na.rm = TRUE)) %>% 
  mutate(site_code_long = "Lake Inflow")

ww3 <- ww2 %>% filter(!grepl("Upstream Causeway", site_code_long),
                      !grepl("Inflow", site_code_long))
unique(ww3$site_code_long)

wq2018 <- ww3 %>% bind_rows(CU) %>% bind_rows(LI) %>% 
  mutate(sitet_abbr1 = case_when(
    site_code_long == "Lake Inflow" ~ "B1.7",
    site_code_long == "Upstream Causeway" ~ "B3.8",
    site_code_long == "Below Causeway" ~ "B5.2",
    site_code_long == "Opposite South Lake" ~ "B9.0",
    site_code_long == "Opposite Sun Valley" ~ "B13.0",
    site_code_long == "Opposite Parkview" ~ "B19.8",
    site_code_long == "WTP Intake" ~ "B25.2",
    site_code_long == "Above Outlet" ~ "B29.1",
  )) %>% 
  mutate(dist_km = case_when(
    site_code_long == "Lake Inflow" ~ 1.7,
    site_code_long == "Upstream Causeway" ~ 3.8,
    site_code_long == "Below Causeway" ~ 5.2,
    site_code_long == "Opposite South Lake" ~ 9.0,
    site_code_long == "Opposite Sun Valley" ~ 13.0,
    site_code_long == "Opposite Parkview" ~ 19.8,
    site_code_long == "WTP Intake" ~ 25.2,
    site_code_long == "Above Outlet" ~ 29.1,
  ))

# write.csv(wq2018, "./R_data-cleaning/EEMs/data/clean/BP_WQ_2018.csv")
