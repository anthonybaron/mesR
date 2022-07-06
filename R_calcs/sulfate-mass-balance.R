library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(patchwork)

### Colin Slack note Wed March 16 2022: I worked through the mass balance…and
### got to the grand conclusion of inconclusive. See my additions at the bottom
### of script Anthony — I approached it somewhat differently than you, in that I
### tried to isolate year over year changes.. What this points me to is that we
### need to include Dief flux (which will be much higher than I expected, with
### concentrations hovering around 60 mg L–1), and even then, the uncertainty
### around sulphate concentrations in the catchment (and flows) might prove
### problematic. I can chip away again at this another day and see if I can shed
### more light on things.

# Source data
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-sulphate.R")
source("./R_data-cleaning/flow-reconstruction/code/clean-flows.R")

sf_daily_raw <- station_flow_daily()
bp_sulphate_raw <- bp_sulphate_all()

# Calculate annual values
sf_daily <- sf_daily_raw %>% 
  select(Year, date_ymd, SK05JG004_combined_cms, SK05JG006_cms, SK05JG013_cms, RC_IC_cms) %>% 
  rename(BP = SK05JG004_combined_cms,
         Dief = SK05JG006_cms,
         Ridge = SK05JG013_cms,
         Ridge_Iskwao = RC_IC_cms)

sf_annual <- sf_daily %>% 
  group_by(Year) %>% 
  summarise_at(vars(BP:Ridge_Iskwao), mean, na.rm = TRUE)

sf_annual %>% 
  pivot_longer(!Year, names_to = "station", values_to = "flow") %>% 
  ggplot(aes(Year, flow, col = station)) + 
  geom_line(size = 1) +
  scale_color_colorblind() +
  theme(legend.position = "bottom") +
  labs(y = "Flow (cm/s)")

bp_sulphate_annual <- bp_sulphate_raw %>% 
  group_by(Year = year) %>% 
  summarise(bp_SO4_mg.L = mean(result, na.rm = TRUE))

sf_SO4 <- full_join(sf_annual, bp_sulphate_annual) %>% 
  mutate(BP_Ls = BP * 1000,                      # BP discharge (L/s)
         Dief_Ls = Dief * 1000,                  # Dief discharge (L/s)
         Ridge_Ls = Ridge * 1000,                # Ridge discharge (L/s)
         Ridge_Iskwao_Ls = Ridge_Iskwao * 1000,  # Ridge+Iskwao discharge (L/s)
         V_bp_m3 = 90000000,                     # BP volume (m3)  
         V_bp_L = V_bp_m3 * 1000,                # BP volume (L)  
         m_bp_SO4_mg = V_bp_L * bp_SO4_mg.L,     # BP SO4 mass (g)
         m_bp_SO4_kg = m_bp_SO4_mg / 1e6,        # BP SO4 mass (kg)
         wtlnd_SO4_min = 6.985,                  # wetland SO4 conc (mg/L)
         wtlnd_SO4_1qu = 26.244,                 # wetland SO4 conc (mg/L)
         wtlnd_SO4_median = 163.912,             # wetland SO4 conc (mg/L)  
         wtlnd_SO4_mean = 519.483,               # wetland SO4 conc (mg/L)
         wtlnd_SO4_3qu = 620.246,                # wetland SO4 conc (mg/L)
         wtlnd_SO4_max = 5501.210,               # wetland SO4 conc (mg/L)
         Mso4_D_BP_kg = BP_Ls * wtlnd_SO4_3qu * 3.154e7 * (1/1e6)
         ) 


v2009 <- full_join(sf_annual, bp_sulphate_annual) %>% filter(Year == 2009)
v2015 <- full_join(sf_annual, bp_sulphate_annual) %>% filter(Year == 2015)


#### Change in SO4 mass in BPL b/w 2009 and 2015 ####



# change in BP SO4 concentration between 2009 and 2015
delta_C_bp_mgL <- v2015$bp_SO4_mg.L - v2009$bp_SO4_mg.L # 279 - 94 = 185
delta_C_bp_kgL <- delta_C_bp_mgL / 1e6 # 0.0001852421 kg/L

# BP lake volume (assume constant)
V_bp_m3 <- 90000000
V_bp_L <- V_bp_m3 * 1000 # 9e10 L

# change in BP SO4 mass between 2009 and 2015
delta_M_bp_kg <- delta_C_bp_kgL * V_bp_L # 16,671,788 kg

#### 

# 3rd quantile wetland SO4 concentration (assumed to be the driver of BP SO4
# concentration change)
C_w_mgL <- 620.246
C_w_kgL <- C_w_mgL / 1e6 # 0.000620246 kg/L

# catchment (Ridge+Iskwao) flow; use to calculate mass of SO4 coming into system
# via catchment wetlands
D_ri09_cms <- v2009$Ridge_Iskwao * 60 * 60 * 24 * 365 # 5425414 m3/yr
D_ri15_cms <- v2015$Ridge_Iskwao * 60 * 60 * 24 * 365 # 14919925 m3/yr

D_ri09_Lyr <- D_ri09_cms * 1000 # 5,425,413,870 L/yr
D_ri15_Lyr <- D_ri15_cms * 1000 # 14,919,924,661 L/yr

# change in catchment flow between 2009 and 2015
delta_D_ri_Lyr <- D_ri15_Lyr - D_ri09_Lyr # 9,494,510,791 L/yr

# mass of SO4 from catchment wetlands using the change between 2009 and 2015
M_w_kg_1 <- C_w_kgL * delta_D_ri_Lyr # 5,888,932 kg 

# mass of SO4 from catchment wetlands using 2015 catchment flow
M_w_kg_2 <- C_w_kgL * D_ri15_Lyr # 9,254,024

# compare the change in BPL SO4 mass to change in catchment wetland sourced SO4
# mass
delta_M_bp_kg / M_w_kg_1 # BP change is 2.8 times higher 
delta_M_bp_kg / M_w_kg_2 # BP change is 1.8 times higher 



# v2009_bpl <- 
v2009 %>% 
  select(-c(Dief, Ridge, BP, Ridge_Iskwao)) %>% 
  rename(C_bp_mg.L = bp_SO4_mg.L) %>% 
  mutate(C_bp_kg.L = bp_SO4_mg.L)

v2009_catchment %>% 
  select(-c(Dief, Ridge)) %>% 
  rename(D_bp_cms = BP,
         D_ri_cms = Ridge_Iskwao,
         C_bp_mgL = bp_SO4_mg.L) %>% 
  mutate(D_bp)


sf_SO4 %>% 
  ggplot(aes(Year, m_bp_SO4_kg)) + 
  geom_line() +
  geom_point() + 
  geom_point(data = sf_SO4, aes(Year, Mso4_D_BP_kg), col = "steelblue") +
  geom_line(data = sf_SO4, aes(Year, Mso4_D_BP_kg), col = "steelblue")


sf_SO4$M_D_BP_kg


sf_SO4 %>% 
  ggplot(aes(Year, m_bp_SO4_kg)) + 
  geom_line(size = 1)



V_bp_m3 <- 90000000 # cubic metres
V_bp_L <- 90000000000 # litres

sf_annual %>% 
  mutate(V_bp_m3 = 90000000,
         V_bp_L = V_bp_m3 * 1000)
