library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggthemes)
library(lubridate)

### Colin Slack note Wed March 16 2022: 
### I worked through the mass balance…and got to the grand conclusion of
### inconclusive. See my additions at the bottom of script Anthony — I
### approached it somewhat differently than you, in that I tried to isolate Year
### over Year changes.. What this points me to is that we need to include Dief
### flux (which will be much higher than I expected, with concentrations
### hovering around 60 mg L–1), and even then, the uncertainty around sulfate
### concentrations in the catchment (and flows) might prove problematic. I can
### chip away again at this another day and see if I can shed more light on
### things.

theme_set(theme_bw(base_size = 12))
SO4_lab <- expression(paste("SO"[4]*" concentration (mg L"^-1*")")) 
options(scipen = 3)

bp_sulfate_raw <- read_csv("./R_mass-balance/data/sulphate.csv")
sf_daily_raw <- read_csv("./R_mass-balance/data/station-flow-daily.csv")
dief_eyebrow_bp_raw <- read_csv("./R_mass-balance/data/dief-eyebrow-bp.csv")

# Calculate annual values
sf_daily <- sf_daily_raw %>% 
  filter(Year %in% c(1990:2019)) %>% 
  select(Year, date_ymd, SK05JG004_combined_cms, SK05JG006_cms, SK05JG013_cms, RC_IC_cms) %>% 
  rename(BP = SK05JG004_combined_cms,
         Dief = SK05JG006_cms,
         Ridge = SK05JG013_cms,
         Ridge_Iskwao = RC_IC_cms)

# Annual station flows
sf_annual <- sf_daily %>% 
  group_by(Year) %>% 
  summarise_at(vars(BP:Ridge_Iskwao), mean, na.rm = TRUE)

# Annual BP sulfate concentrations
bp_sulfate_annual <- bp_sulfate_raw %>% 
  group_by(Year = year) %>% 
  summarise(bp_SO4_mg.L = mean(result, na.rm = TRUE))

# Annual Dief sulfate concentrations
dief_sulfate_annual <- dief_eyebrow_bp_raw %>% 
  mutate(Year = year(date_ymd)) %>% 
  filter(site == "Lake Diefenbaker", parameter == "Sulphate", Year >= 1990) %>% 
  pivot_wider(-site, names_from = parameter, values_from = result) %>% 
  group_by(Year) %>% 
  summarise(dief_SO4_mg.L = mean(Sulphate, na.rm = TRUE))

# Get summary stats for 
# "Sulphate", "Iron", "Total-P", "Ortho-P", "UV 254", "D.O.C.", 
# "Chlorophyll \"A\"", "Ammonia-N", "Organic-N", "Nitrate as N"
library(rstatix)
deb <- subset(dief_eyebrow_bp_raw, parameter %in% 
                c("Sulphate", "Iron", "Total-P", "Ortho-P", "UV 254", "D.O.C.",
                  "Chlorophyll \"A\"", "Ammonia-N", "Organic-N", "Nitrate as N") &
                !is.na(result) & date_ymd >= "1990-01-01")
site_ord <- c("Lake Diefenbaker", "Qu'Appelle Dam", "Eyebrow Lake", 
              "Buffalo Pound Lake West", "Buffalo Pound Lake")
deb <- deb %>% mutate(site = forcats::fct_relevel(site, site_ord)) %>% 
  rename(Site = site, Year = date_ymd)

debDOC <- subset(deb, parameter == "D.O.C.")
debDOC %>% group_by(Site) %>% get_summary_stats(result, type = 'five_number')
# Site                    variable       n   min   max    q1 median    q3
# <chr>                     <chr>    <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
# 1 Buffalo Pound Lake      result      43  1.2  11.2   4.6    5.6   7.02
# 2 Buffalo Pound Lake West result      43  3.5  10.6   4.46   5.3   6.05
# 3 Eyebrow Lake            result      41  3    29.2   3.8    4.43  4.9 
# 4 Lake Diefenbaker        result      43  2.54  6.08  3.6    4.3   4.65
# 5 Qu'Appelle Dam          result      42  2.85  8.1   3.44   4.05  4.6 
p_DOC <- ggplot(debDOC, aes(Year, result, col = Site)) +
  geom_point() + geom_line() + facet_wrap(~ Site, scales = "free_y", ncol = 1) +
  scale_color_viridis_d(end = 0.85) + 
  ylim(c(0, NA)) +
  labs(y = "DOC concentration (mg/L)")

debUV254 <- subset(deb, parameter == "UV 254")
debUV254 %>% group_by(Site) %>% get_summary_stats(result, type = 'five_number')
# Site                    variable       n   min   max    q1 median    q3
# <chr>                    <chr>    <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
# 1 Buffalo Pound Lake      result      12 0.767  1.50 0.841  0.94  1.02 
# 2 Buffalo Pound Lake West result      12 0.898  1.84 1.02   1.10  1.32 
# 3 Eyebrow Lake            result      11 0.624  3.12 0.912  1.08  1.21 
# 4 Lake Diefenbaker        result      12 0.586  1.61 0.739  1.10  1.21 
# 5 Qu'Appelle Dam          result      12 0.594  1.51 0.803  0.942 0.998
ggplot(debUV254, aes(Year, result, col = Site)) +
  geom_point() + geom_line() + facet_wrap(~ Site, scales = "free_y", ncol = 1) +
  scale_color_viridis_d(end = 0.85)

debSUVA <- deb %>% 
  filter(parameter %in% c("D.O.C.", "UV 254")) %>% 
  select(-c(unit)) %>% 
  pivot_wider(id_cols = c(Site, Year),
              names_from = parameter,
              values_from = result) %>% 
  filter(!is.na(`UV 254`)) %>% 
  mutate(SUVA = `UV 254` / `D.O.C.` * 10)
debSUVA %>% group_by(Site) %>% get_summary_stats(SUVA, type = 'five_number')
# Site                    variable       n   min   max    q1 median    q3
# <fct>                    <chr>    <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
# 1 Lake Diefenbaker        SUVA        12  2.06  2.93  2.36   2.58  2.67
# 2 Eyebrow Lake            SUVA        11  1.92  2.91  2.17   2.28  2.47
# 3 Qu'Appelle Dam          SUVA        12  1.99  2.83  2.26   2.41  2.52
# 4 Buffalo Pound Lake West SUVA        12  1.91  2.33  2.04   2.10  2.25
# 5 Buffalo Pound Lake      SUVA        12  1.54  2.32  1.71   1.79  1.88
p_SUVA <- ggplot(debSUVA, aes(Year, SUVA, col = Site)) +
  geom_point() + geom_line() + facet_wrap(~ Site, ncol = 1) +
  scale_color_viridis_d(end = 0.85) + 
  labs(y = "SUVA (L/mg m)")

debSO4 <- subset(deb, parameter == "Sulphate")
debSO4 %>% group_by(Site) %>% get_summary_stats(result, type = 'five_number')
# Site                    variable       n   min    max    q1 median    q3
# <chr>                      <chr>   <dbl> <dbl>  <dbl> <dbl>  <dbl> <dbl>
# 1 Lake Diefenbaker        result      43  34     88.6  48.5   58    66.4
# 2 Eyebrow Lake            result      41  43   1305    56     69    90.6
# 3 Qu'Appelle Dam          result      34  46    147    56.5   64.4  70.6
# 4 Buffalo Pound Lake West result      35  41.7  303.   79.5   88.2 106. 
# 5 Buffalo Pound Lake      result      43  41    155.   83.8   95   110. 
p_SO4 <- ggplot(debSO4, aes(Year, result, col = Site)) +
  geom_point() + geom_line() + facet_wrap(~ Site, scales = "free_y", ncol = 1) +
  scale_color_viridis_d(end = 0.85) + 
  labs(y = "Sulfate concentration (mg/L)")

ggplot(debSO4 %>% filter(Site == "Buffalo Pound Lake West"), aes(yday(Year), result)) +
  geom_point() + geom_line() + facet_wrap(~ year(Year)) +
  labs(y = "Sulfate concentration (mg/L)")

debIron <- subset(deb, parameter == "Iron")
debIron %>% group_by(Site) %>% get_summary_stats(result, type = 'five_number')
# Site                    variable       n   min   max    q1 median    q3
# <chr>                      <chr>   <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
# 1 Lake Diefenbaker        result      42     0  0.18 0.01   0.025 0.057
# 2 Eyebrow Lake            result      40     0  3.5  0.02   0.225 0.64 
# 3 Qu'Appelle Dam          result      40     0  0.68 0.007  0.055 0.092
# 4 Buffalo Pound Lake West result      41     0  0.61 0.034  0.16  0.37 
# 5 Buffalo Pound Lake      result      40     0  0.48 0.012  0.07  0.16 
ggplot(debIron, aes(Year, result, col = Site)) +
  geom_point() + geom_line() + facet_wrap(~ Site, scales = "free_y", ncol = 1) +
  scale_color_viridis_d(end = 0.85) + 
  labs(y = "Iron concentration (mg/L)")

debTP <- subset(deb, parameter == "Total-P")
debTP %>% group_by(Site) %>% get_summary_stats(result, type = 'five_number')
# Site                    variable       n   min   max    q1 median    q3
# <chr>                    <chr>    <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
# 1 Lake Diefenbaker        result      43   0     59   10     14.0  25.1
# 2 Eyebrow Lake            result      40   1.5  380   35     57    98.5
# 3 Qu'Appelle Dam          result      41   0    152   13     19    25  
# 4 Buffalo Pound Lake West result      43  15    172.  46.5   69    90  
# 5 Buffalo Pound Lake      result      43   4    208   39     62    79.9
p_TP <- ggplot(debTP, aes(Year, result, col = Site)) +
  geom_point() + geom_line() + facet_wrap(~ Site, scales = "free_y", ncol = 1) +
  scale_color_viridis_d(end = 0.85) + 
  labs(y = "TP concentration (µg/L)")

debSRP <- subset(deb, parameter == "Ortho-P")
debSRP %>% group_by(Site) %>% get_summary_stats(result, type = 'five_number')
# Site                    variable       n   min   max    q1 median    q3
# <chr>                     <chr>    <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
# 1 Lake Diefenbaker        result      43     0    28     0    1     3  
# 2 Eyebrow Lake            result      39     0    38     0    3     8  
# 3 Qu'Appelle Dam          result      41     0    63     0    0     3  
# 4 Buffalo Pound Lake West result      43     0    34     2    7    12.5
# 5 Buffalo Pound Lake      result      42     0    90     3    6.5  13 
p_SRP <- ggplot(debSRP, aes(Year, result, col = Site)) +
  geom_point() + geom_line() + facet_wrap(~ Site, scales = "free_y", ncol = 1) +
  scale_color_viridis_d(end = 0.85) +
  labs(y = "SRP concentration (µg/L)")

debChla <- subset(deb, parameter == "Chlorophyll \"A\"" &
                    !Site == "Buffalo Pound Lake West")
debChla %>% group_by(Site) %>% get_summary_stats(result, type = 'five_number')
# Site                    variable       n   min   max    q1 median    q3
# <chr>                     <chr>    <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
# 1 Lake Diefenbaker        result      43     0    32  3         4   6  
# 3 Buffalo Pound Lake      result      42     1    89  6.92     13  28.8
ggplot(debChla, aes(Year, result, col = Site)) +
  geom_point() + geom_line() + facet_wrap(~ Site, scales = "free_y", ncol = 1) +
  scale_color_viridis_d(end = 0.85)

debOrgN <- subset(deb, parameter == "Organic-N")
debOrgN %>% group_by(Site) %>% get_summary_stats(result, type = 'five_number')
# Site                    variable       n   min   max    q1 median    q3
# <chr>                     <chr>    <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
# 1 Lake Diefenbaker        result      43  0      0.77 0.265  0.32  0.365
# 2 Eyebrow Lake            result      40  0      2.89 0.35   0.41  0.55 
# 3 Qu'Appelle Dam          result      38  0    210    0.28   0.354 0.43 
# 4 Buffalo Pound Lake West result      38  0      2.09 0.39   0.535 0.737
# 5 Buffalo Pound Lake      result      43  0.28   1.61 0.46   0.61  0.85 
ggplot(debOrgN, aes(Year, result, col = Site)) +
  geom_point() + geom_line() + facet_wrap(~ Site, scales = "free_y", ncol = 1) +
  scale_color_viridis_d(end = 0.85) + 
  labs(y = "Organic N concentration (mg/L)")

debNH3 <- subset(deb, parameter == "Ammonia-N")
debNH3 %>% group_by(Site) %>% get_summary_stats(result, type = 'five_number')
# Site                    variable       n   min   max    q1 median    q3
# <chr>                     <chr>    <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
# 1 Lake Diefenbaker        result      43     0  0.14     0  0.014 0.06 
# 2 Eyebrow Lake            result      39     0  0.2      0  0.02  0.055
# 3 Qu'Appelle Dam          result      13     0  0.15     0  0.01  0.02 
# 4 Buffalo Pound Lake West result      14     0  0.15     0  0.014 0.03 
# 5 Buffalo Pound Lake      result      43     0  0.24     0  0.04  0.07 
p_NH3 <- ggplot(debNH3, aes(Year, result, col = Site)) +
  geom_point() + geom_line() + facet_wrap(~ Site, scales = "free_y", ncol = 1) +
  scale_color_viridis_d(end = 0.85) + 
  labs(y = "Ammonia–N concentration (mg/L)")
  
debNO3 <- subset(deb, parameter == "Nitrate as N")
debNO3 %>% group_by(Site) %>% get_summary_stats(result, type = 'five_number')
# Site                    variable       n   min   max    q1 median    q3
# <chr>                     <chr>    <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
# 1 Lake Diefenbaker        result      41     0  2.35  0.17  0.38  0.611
# 2 Eyebrow Lake            result      38     0  1.11  0     0.112 0.495
# 3 Qu'Appelle Dam          result      40     0  1.21  0.07  0.18  0.48 
# 4 Buffalo Pound Lake West result      40     0  0.8   0     0.005 0.075
# 5 Buffalo Pound Lake      result      40     0  0.35  0     0     0.046
ggplot(debNO3, aes(Year, result, col = Site)) +
  geom_point() + geom_line() + facet_wrap(~ Site, scales = "free_y", ncol = 1) +
  scale_color_viridis_d(end = 0.85) + 
  labs(y = "Nitrate as N concentration (mg/L)")

(p_DOC + p_SO4 + p_TP + p_NH3) + patchwork::plot_layout(guides = "collect", ncol = 4) & theme(legend.position = "bottom")

# Values for calculations -------------------------------------------------

### 2010
flows2010 <- sf_annual %>% filter(Year == 2010) %>% 
  rename(bp_flow_cms = BP,
         dief_flow_cms = Dief,
         ridge_flow_cms = Ridge,
         ridge_iskwao_flow_cms = Ridge_Iskwao)

sulfate2010 <- bp_sulfate_annual %>% 
  filter(Year == 2010) %>% 
  right_join(., (dief_sulfate_annual %>% filter(Year == 2010))) %>% 
  mutate(pond_SO4_mg.L = 164,     # median SO4 value from PW wetland survey
         pond_ri_SO4_mg.L = 1094) # median SO4 value from predicted Ridge Creek wetland conc. 

v2010 <- full_join(flows2010, sulfate2010) %>% mutate(bp_V_L = 9e+10)

### 2011
flows2011 <- sf_annual %>% filter(Year == 2011) %>% 
  rename(bp_flow_cms = BP,
         dief_flow_cms = Dief,
         ridge_flow_cms = Ridge,
         ridge_iskwao_flow_cms = Ridge_Iskwao)

sulfate2011 <- bp_sulfate_annual %>% 
  filter(Year == 2011) %>% 
  right_join(., (dief_sulfate_annual %>% filter(Year == 2011))) %>% 
  mutate(pond_SO4_mg.L = 164,     # median SO4 value from PW wetland survey
         pond_ri_SO4_mg.L = 1094) # median SO4 value from predicted Ridge Creek wetland conc. 

v2011 <- full_join(flows2011, sulfate2011) %>% mutate(bp_V_L = 9e+10)


# Plots -------------------------------------------------------------------

# Plot annual station flows
sf_annual %>% 
  pivot_longer(!Year, names_to = "station", values_to = "flow") %>% 
  ggplot(aes(Year, flow, col = station)) + 
  geom_line(size = 1) +
  scale_color_colorblind() +
  theme(legend.position = "bottom") +
  labs(y = "Flow (cm/s)")

# Plot annual BP sulfate concentrations
bp_sulfate_annual %>% 
  ggplot(aes(Year, bp_SO4_mg.L)) + 
  geom_line(col = "steelblue", size = 1) +
  geom_point(col = "white", size = 2.5) +
  geom_point(col = "steelblue", size = 1.75) +
  theme_bw(base_size = 16) +
  labs(y = SO4_lab,
       subtitle = "Buffalo Pound Lake sulfate concentration (1990–2019)")

# Plot annual Dief sulfate concentrations
dief_sulfate_annual %>% 
  ggplot(aes(Year, dief_SO4_mg.L)) + 
  geom_line(col = "black", size = 1) +
  geom_point(col = "white", size = 2.5) +
  geom_point(col = "black", size = 1.75) +
  theme_bw(base_size = 16) +
  labs(x = "Year", y = SO4_lab,
       subtitle = "Diefenbaker Lake sulfate concentration (1990–2019)")

# Plot annual BP and Dief sulfate concentrations
bp_sulfate_annual %>% 
  ggplot(aes(Year, bp_SO4_mg.L)) + 
  geom_line(col = "steelblue", size = 1) +
  geom_point(col = "white", size = 2.5) +
  geom_point(col = "steelblue", size = 1.75) +
  geom_line(data = dief_sulfate_annual, aes(Year, dief_SO4_mg.L), col = "black", size = 1) +
  geom_point(data = dief_sulfate_annual, aes(Year, dief_SO4_mg.L), col = "white", size = 2.5) +
  geom_point(data = dief_sulfate_annual, aes(Year, dief_SO4_mg.L), col = "black", size = 1.75) +
  theme_bw(base_size = 16) +
  ylim(c(0, 300)) +
  labs(y = SO4_lab,
       subtitle = "Buffalo Pound Lake and Lake Diefenbaker\nsulfate concentrations (1990–2019)")

# Calculations ------------------------------------------------------------

# PW Wetland sulfate ---------------------------------------------------------

##CJW additions

# Wetland sulfate concentrations from PW survey:
# Min.   :    6.985
# 1st Qu.:   26.244
# Median :  163.912
# Mean   :  519.483 
# 3rd Qu.:  620.246
# Max.   : 5501.210


##high change Year
# AB: Take flow and SO4 values from 2010. Scale the Ridge Creek + Iskwao Creek 
#     flow to the effective area of Buffalo Pound Lake, and assume the inflow to
#     Buffalo Pound Lake is equal to its outflow. Multiply to get annual values
#     in L/yr (i.e. V-bp.in = V-bp.out)
Inflow_ri2010_L <- v2010$ridge_iskwao_flow_cms * 60 * 60 * 24 * 365*1000 # L/yr (scale to effective area)
Outflow_BP2010_L <- v2010$bp_flow_cms * 60 * 60 * 24 * 365*1000*0.95 # L/yr (assume evaporative loss)
#
# 2010 is a high change change Year. 
# The inflow from Ridge Creek and Iskwao Creek scaled to the effective area of
# Buffalo Pound Lake is 15,627,962,724 L/yr (1.6e10 L/yr).
# The outflow from Buffalo Pound Lake (assuming 5% evaporative loss) is
# 114,854,943,729 L/yr (1.1e11 L/yr).

# Sulfate concentration change between end-of-Year 2009 and end-of-Year 2010 
# C-bp in mg/L
C_change2010.mgL <- 177-97 #2010 Year end minus 2009 Year end = 80 mg/L

# Sulfate mass change between 2009 and 2010
# M-bp = C-bp * V-bp --- units: kg = kg/L * L
M_change2010.kg <- C_change2010.mgL*v2010$bp_V_L/10^6 # = 7,200,000 kg (7.2e06 kg)

# Sulfate mass exported out of Buffalo Pound Lake in 2010 
# M-export = C-bp * V-bp.out 
M_export2010.kg <- v2010$bp_SO4_mg.L/10^6*Outflow_BP2010_L # = 13,480,142 kg (1.3e07 kg)

# Sulfate mass input from catchment (i.e., wetlands) introduced to system via
# Ridge and Iskwao Creeks in 2010
# M-catchment = C-catchment * V-ri
M_catchmentinput2010.kg <-  v2010$pond_SO4_mg.L*Inflow_ri2010_L/10^6 # = 2,562,986 kg (2.6e06 kg)

# M-fraction = (M-catchment — M-export) / M-bp
t_frac <- (M_catchmentinput2010.kg-M_export2010.kg)/M_change2010.kg ##predicted change from RI inflow as a fraction of observed change during 2010
t_frac_scaled <- (M_catchmentinput2010.kg*3.7-M_export2010.kg)/M_change2010.kg
# t_frac = -1.516272
# t_fract_scaled = -0.555152

###low change Year (2011)
Inflow_ri2011_L <- v2011$ridge_iskwao_flow_cms * 60 * 60 * 24 * 365*1000 # L/yr (scale to effective area)
Outflow_BP2011_L <- v2011$bp_flow_cms * 60 * 60 * 24 * 365*1000*0.95 # L/yr (assume evaporative loss)

C_change2011.mgL <- 213-181 #2011 Year end minus 2010 Year end
M_change2011.kg <- C_change2011.mgL*v2011$bp_V_L/10^6
M_export2011.kg <- v2011$bp_SO4_mg.L/10^6*Outflow_BP2011_L
M_catchmentinput2011.kg <-  v2011$pond_SO4_mg.L*Inflow_ri2011_L/10^6
t_frac_2011 <- (M_catchmentinput2011.kg-M_export2011.kg)/M_change2011.kg ##predicted change from RI inflow as a fraction of observed change during 2010
t_frac_2011_scaled <- (M_catchmentinput2011.kg*3.7-M_export2011.kg)/M_change2011.kg
# t_frac_2011 = -4.608044
# t_frac_2011_scaled = -0.8356989


# Deifenbaker sulfate -----------------------------------------------------

##high change Year (2010)
Inflow_dief2010_L <- v2010$dief_flow_cms * 60 * 60 * 24 * 365*1000 # L/yr (scale to effective area)
Outflow_BP2010_L <- v2010$bp_flow_cms * 60 * 60 * 24 * 365*1000*0.95 # L/yr (assume evaporative loss)
C_change2010.mgL <- 177-97 #2010 Year end minus 2009 Year end = 80 mg/L
M_change2010.kg <- C_change2010.mgL*v2010$bp_V_L/10^6 # = 7,200,000 kg (7.2e06 kg)
M_export2010.kg <- v2010$bp_SO4_mg.L/10^6*Outflow_BP2010_L # = 13,480,142 kg (1.3e07 kg)

# L Dief mean sulfate concentration between 2009 and 2010
C_dief_mgL <- as.numeric((dief_sulfate_annual %>% filter(Year %in% c(2009:2010)) %>% summarise(so4_mean = mean(dief_SO4_mg.L)))) # 70.5 mg/L
M_diefinput2010.kg <-  C_dief_mgL*Inflow_dief2010_L/10^6 # = 6,324,110 kg (6.3e06 kg)

t_frac_dief <- (M_diefinput2010.kg-M_export2010.kg)/M_change2010.kg ##predicted change from L Dief inflow as a fraction of observed change during 2010
t_frac_scaled_dief <- (M_diefinput2010.kg*3.7-M_export2010.kg)/M_change2010.kg
# t_frac = -0.9938934
# t_fract_scaled = 1.377648


# Ridge Creek predicted wetland sulfate -----------------------------------

Inflow_ri2010_L <- v2010$ridge_iskwao_flow_cms * 60 * 60 * 24 * 365*1000 # L/yr (scale to effective area)
Outflow_BP2010_L <- v2010$bp_flow_cms * 60 * 60 * 24 * 365*1000*0.95 # L/yr (assume evaporative loss)

C_change2010.mgL <- C_change2010.mgL <- 177-97 #2010 Year end minus 2009 Year end = 80 mg/L
M_change2010.kg <- C_change2010.mgL*v2010$bp_V_L/10^6
M_export2010.kg <- v2010$bp_SO4_mg.L/10^6*Outflow_BP2010_L
M_rc_pred_input2010.kg <-  v2010$pond_ri_SO4_mg.L*Inflow_ri2010_L/10^6
t_frac_rcpred_2010 <- (M_rc_pred_input2010.kg-M_export2010.kg)/M_change2010.kg ##predicted change from RI inflow as a fraction of observed change during 2010
t_frac_rcpred_2010_scaled <- (M_rc_pred_input2010.kg*3.7-M_export2010.kg)/M_change2010.kg
# t_frac_rcpred_2010 = 0.5023402
# t_frac_rcpred_2010_scaled = 6.913712

# Combined sulfate inputs  -----------------------------------------------

# Is our last step then to combine sulfate inputs? Or just Dief + Ridge predicted? Or other?

M_diefinput2010.kg # Dief SO4 input
M_catchmentinput2010.kg # PW survey SO4 input
M_rc_pred_input2010.kg # Ridge predicted SO4 input based on PW survey
M_export2010.kg # SO4 mass export from BP
M_change2010.kg # SO4 conc change in BP b/w 2009 and 2010

t_frac_combined_all <- (((M_diefinput2010.kg+M_catchmentinput2010.kg+M_rc_pred_input2010.kg)-M_export2010.kg)/M_change2010.kg)
t_frac_combined_all_scaled <- (((M_diefinput2010.kg+M_catchmentinput2010.kg+M_rc_pred_input2010.kg)*3.7-M_export2010.kg)/M_change2010.kg)
# t_frac_combined_all = 1.736659
# t_frac_combined_all_scaled = 11.48069

t_frac_dief_rcpred <- (((M_diefinput2010.kg+M_rc_pred_input2010.kg)-M_export2010.kg)/M_change2010.kg)
t_frac_dief_rcpred_scaled <- ((((M_diefinput2010.kg+M_rc_pred_input2010.kg)*3.7)-M_export2010.kg)/M_change2010.kg)
# t_frac_dief_rcpred = 1.380689
# t_frac_dief_rcpred_scaled = 10.1636