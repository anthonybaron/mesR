### Notes from Colin:
## (1) This script reads hydrometric station data.
## This represents a transfer (and extension) of the approach provided in .xlsx 
## form by Ali Nazemi while a Senior Hydrologist with SK WSA (in 2016).
## 
## (2) Reconstructs missing flow from observational data following Nazemi data 
## are infilled, and observational and reconstructed records are combined for 
## each station, and flows are broken down by source area (with some caveats).

## Details on the approach are here:
## Proportional flows from locations of the upstream of the BPL are determined 
## (estimated). 
## The ungauged portions are estimated using linear regression of the nearby
## gauges.
## The period of estimation originally covered from January 1, 2001 to October 
## 31, 2013 (and 1972-1992 was used as training for this approach).
## Please see the schematic of the system in the sheet called 
## “Upstream BPL_Train (1972-1992)” in the file BPL upstream_backwater_H&C_April2016. 


# For estimating Iskwao Creek (Y) based on Ridge Creek (X) ----------------

#  If cold season: Y = 0.3112*(X)^0.4537
#  If warm season: If X = 0 then Y = 0; Otherwise, Y = 0.4957*X + 0.1185


# # For estimating Ungauged part (U) based on Ridge Creek (X) -------------

#  If cold season: U = 0.01292*X^3 - 0.303*X^2 + 1.249*X - 0.0956
#  If warm season: U = -0.05958*X^2 + 2.77*X - 0.1463

# Cold months include December, January, February and March and the rest of 
# months are warm. 
# 
# U includes both positive and negative values. Negative values are related to 
# the episodes, in which the ungauged instream part (including Eyebrow Lake)
# retain part of the water coming from upstream (i.e. diverted flow from LD as 
# well as Iskwao and Ridge creeks). 
# 
# These equations were selected among a pool of 420 forms of linear and
# non-linear regression forms and they stand out based on Bayesian Information 
# Criterion. 
# 
# Coefficient of determination (R2) for estimating the inflow to BPL stays as 
# 0.87 during training period.

library(tidyverse)
library(lubridate)
theme_set(theme_bw(base_size = 12))


# 1.  Read station data ---------------------------------------------------

# 05JG004 = Qu'Appelle River Above Buffalo Pound Lake
s_05JG004 <- read_csv("./R_flow-reconstruction/Colin/data/BP_flow_raw/Daily__05JG004_withmissing.csv", skip = 1) %>% 
  subset(PARAM == 1) %>% 
  mutate(DD = as.numeric(DD),
         date = as.Date(DD - 1, origin = paste(YEAR, "01-01", sep = "-")))

# 05JG006 = Elbow Diversion Canal at Drop Structure
s_05JG006 <- read_csv("./R_flow-reconstruction/Colin/data/BP_flow_raw/Daily__05JG006_withmissing.csv", skip = 1) %>% 
  subset(PARAM == 1) %>% 
  mutate(DD = as.numeric(DD),
         date = as.Date(DD - 1, origin = paste(YEAR, "01-01", sep = "-")))

# 05JG013 = Ridge Creek Near Bridgeford
s_05JG013 <- read_csv("./R_flow-reconstruction/Colin/data/BP_flow_raw/Daily__05JG013_withmissing.csv", skip = 1) %>% 
  subset(PARAM == 1) %>% 
  mutate(DD = as.numeric(DD),
         date = as.Date(DD - 1, origin = paste(YEAR, "01-01", sep = "-")))

# 05JG014 = Iskwao Creek Near Craik
s_05JG014 <- read_csv("./R_flow-reconstruction/Colin/data/BP_flow_raw/Daily__05JG014_withmissing.csv", skip = 1) %>% 
  subset(PARAM == 1) %>% 
  mutate(DD = as.numeric(DD),
         date = as.Date(DD - 1, origin = paste(YEAR, "01-01", sep = "-")))

# Create station_flow data frame with data from all four stations...
# Nazemi's training set starts at 1972-10-01
# Make Season var for Warm and Cold seasons
# 
station_flow1 <- bind_rows(s_05JG004, s_05JG006, s_05JG013, s_05JG014) %>% 
  subset(date >= "1972-10-01") %>%
  select(-c(PARAM, SYM)) %>% 
  pivot_wider(names_from = ID, values_from = Value) %>% 
  rename(Year = YEAR,
         doy = DD,
         date_ymd = date,
         SK05JG004_cms = `05JG004`,
         SK05JG006_cms = `05JG006`,
         SK05JG013_cms = `05JG013`,
         SK05JG014_cms = `05JG014`) %>% 
  mutate(Month = month(date_ymd),
         Season = as.factor(ifelse(Month %in% c(12, 01, 02, 03), "Cold", "Warm")))



# 2.  Infill data,  reconstruct flows, and estimate proportions of flows ----- 

# Flow reconstructions
station_flow2 <- station_flow1 %>% 
  # Set NAs in 05JG013 to zero
  mutate(SK05JG013_cms = ifelse(is.na(SK05JG013_cms), 0, SK05JG013_cms)) %>% 
  # Reconstruct Iskwao Creek using Ridge Creek flows
  mutate(SK05JG014_predicted_cms = ifelse(Season == "Cold", 0.3112*SK05JG013_cms^0.4537, # cold season
                                          ifelse(SK05JG013_cms == 0, 0, 0.4957*SK05JG013_cms + 0.1185))) %>% # warm season
  # Reconstruct Ungauged catchment using Ridge Creek flows
  mutate(Ungauged_predicted_cms = ifelse(Season == "Cold", 
                                         0.01292*SK05JG013_cms^3 - 0.303*SK05JG013_cms^2 + 1.249*SK05JG013_cms - 0.0956, # cold season
                                         -0.05958*SK05JG013_cms^2 + 2.77*SK05JG013_cms - 0.1463))

# Plot Ungauged (predicted) vs. Ridge Creek near Bridgeford (05JG013)
station_flow2 %>% ggplot(aes(SK05JG013_cms, Ungauged_predicted_cms, colour = Season)) + geom_point()

# Additional flow reconstructions
station_flow3 <- station_flow2 %>%
  # Reconstruct BP inflow
  # BP inflow is the sum of gauged (observed) and ungauged (predicted) flows
  mutate(SK05JG004_predicted_cms = SK05JG013_cms + SK05JG014_predicted_cms + SK05JG006_cms + Ungauged_predicted_cms) %>% 
  # Combine observational and reconstructed records for Iskwao Creek and BP inflow
  mutate(SK05JG014_combined_cms = ifelse(is.na(SK05JG014_cms), SK05JG014_predicted_cms, SK05JG014_cms),
         SK05JG004_combined_cms = ifelse(is.na(SK05JG004_cms), SK05JG004_predicted_cms, SK05JG004_cms))

# Proportional flows
station_flow4 <- station_flow3 %>% 
  # Diefenbaker proportion = Elbow Diversion flow / BP inflow 
  mutate(SK05JG006_percent = SK05JG006_cms / SK05JG004_combined_cms * 100) %>% 
  # Ridge Creek proportion = Ridge Creek flow / BP inflow
  mutate(SK05JG013_percent = SK05JG013_cms / SK05JG004_combined_cms * 100) %>% 
  # Iskwao proportion = Iskwao flow / BP inflow
  mutate(SK05JG014_percent = SK05JG014_combined_cms / SK05JG004_combined_cms * 100) %>% 
  # Ungauged proportion = Ungauged (predicted) / BP inflow 
  mutate(Ungauged_percent = Ungauged_predicted_cms / SK05JG004_combined_cms * 100)

# Plot raw flows from Diefenbaker (05JG006) and to Buffalo Pound (05JG004)
station_flow4 %>% 
  ggplot(aes(SK05JG004_cms, SK05JG006_cms)) + 
  geom_point(alpha = 1/4) +
  geom_abline(colour = "red") +
  labs(title = "Raw flows from Diefenbaker and to Buffalo Pound")

# write_csv(station_flow4,"./R_flow-reconstruction/Anthony/outputs/data_clean/BP-flow-infilled-with-proportions_(1972-2019).csv")


# 3.  Clean up data set  --------------------------------------------------

# Make a column denoting Observed or Predicted flow
# When is BP inflow missing observed data? 1995-01-01 to 2015-05-31
# When is Iskwao Creek missing observed data

station_flow_long <- station_flow4 %>%
  select(!ends_with("percent")) %>% 
  filter(date_ymd >= "1980-01-01") %>%  
  pivot_longer(cols = starts_with(c("SK", "Ungauged")),
               names_to = "Station",
               values_to = "discharge_cms") %>% 
  mutate(flow_tag = ifelse(grepl("predicted", Station), "Predicted",
                           ifelse(grepl("combined", Station), "Combined", "Observed")),
         Station = ifelse(grepl("SK05JG004", Station), "SK05JG004_cms",
                          ifelse(grepl("SK05JG014", Station), "SK05JG014_cms", 
                                 ifelse(grepl("Ungauged", Station), "Ungauged", Station))),
         flow_tag = factor(flow_tag, levels = c("Observed", "Predicted", "Combined")),
         Station = factor(Station))


station_flow_long %>% 
  filter(Station == "SK05JG004_cms" & flow_tag == "Combined") %>% 
  ggplot(aes(date_ymd, discharge_cms)) +
  geom_line(colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("1995-01-01")), linetype = 2, size = 3/4, col = "red") +
  geom_vline(xintercept = as.numeric(as.Date("2015-06-01")), linetype = 2, size = 3/4, col = "red")


station_flow_long %>% 
  filter(Station == "SK05JG014_cms") %>% View()

station_flow_long %>% 
  filter(Station == "SK05JG014_cms") %>% 
  ggplot(aes(date_ymd, discharge_cms, colour = flow_tag)) +
  facet_wrap(~ flow_tag, nrow = 3) + 
  geom_line()

station_flow_long %>% 
  filter(Station == "SK05JG006_cms") %>% 
  ggplot(aes(date_ymd, discharge_cms, colour = flow_tag)) +
  facet_wrap(~ flow_tag, nrow = 3) + 
  geom_line()


station_flow4 %>% 
  select(date_ymd, SK05JG004_cms, SK05JG004_predicted_cms, SK05JG004_combined_cms) %>%  
  filter(date_ymd >= "1980-01-01" & is.na(SK05JG004_cms)) 









