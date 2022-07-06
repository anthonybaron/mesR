library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(janitor)
library(lubridate)
library(patchwork)
library(ggpubr)

source("./R_data-cleaning/flow-reconstruction/code/clean-flows.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-DOC.R")

theme_set(theme_bw(base_size = 12))


sf_daily <- station_flow_daily()
sf_weekly <- station_flow_weekly()
sf_monthly <- station_flow_monthly()
doc_monthly <- bp_DOC_monthly()

flow_lab <- expression(paste("Flow (m"^3*" s"^-1*")"))
SK05JG004_lab <- "Qu'Appelle River Above Buffalo Pound Lake (05JG004)"
SK05JG006_lab <- "Diefenbaker / Elbow Diversion Canal at Drop Structure (05JG006)"
SK05JG013_lab <- "Ridge Creek Near Bridgeford (05JG013)"
SK05JG014_lab <- "Iskwao Creek Near Craik (05JG014)"


# Full time series (1980-2019) --------------------------------------------

p_05JG006 <- sf_daily %>% 
  ggplot(aes(date_ymd, SK05JG006_cms)) +
  geom_line(colour = "#00BFC4", size = 1.25) +
  theme(axis.title.x = element_blank()) +
  labs(y = flow_lab, 
       subtitle = SK05JG006_lab)

p_05JG013 <- sf_daily %>% 
  ggplot(aes(date_ymd, SK05JG013_cms)) +
  geom_line(colour = "#00BFC4", size = 1.25) +
  theme(axis.title.x = element_blank()) +
  labs(y = flow_lab, 
       subtitle = SK05JG013_lab) 

p_05JG014 <- sf_daily %>%
  ggplot(aes(date_ymd, SK05JG014_cms)) +
  geom_line(colour = "#00BFC4", size = 1.25) +
  geom_line(data = sf_daily, aes(date_ymd, SK05JG014_predicted_cms), colour = "#F8766D", size = 1, alpha = 2/3) +
  # lims(y = c(0, 80)) +
  theme(axis.title.x = element_blank()) +
  labs(y = flow_lab, 
       subtitle = SK05JG014_lab)

sf_daily %>%
  ggplot(aes(yday(date_ymd), SK05JG014_cms)) +
  geom_line(colour = "#00BFC4", size = 1.25) +
  geom_line(data = sf_daily, aes(yday(date_ymd), SK05JG014_predicted_cms), colour = "#F8766D", size = 1, alpha = 2/3) +
  # lims(y = c(0, 80)) +
  theme(axis.title.x = element_blank()) +
  labs(y = flow_lab, 
       subtitle = SK05JG014_lab) +
  facet_wrap(~ Year)


p_05JG004 <- sf_daily %>% 
  ggplot(aes(date_ymd, SK05JG004_cms)) +
  geom_line(colour = "#00BFC4", size = 1.25) +
  geom_line(data = sf_daily, aes(date_ymd, SK05JG004_predicted_cms), colour = "#F8766D", size = 1, alpha = 2/3) +
  lims(y = c(0, 80)) +
  theme(axis.title.x = element_blank()) +
  labs(y = flow_lab, 
       subtitle = SK05JG004_lab)

p_ungauged <- sf_daily %>% 
  ggplot(aes(date_ymd, Ungauged_predicted_cms)) +
  geom_line(colour = "#F8766D", size = 1, alpha = 2/3) +
  labs(x = "Year", y = flow_lab, 
       subtitle = "Ungauged contribution based on Ridge Creek flow") 

p_raw_predicted_flows <- p_05JG006 / p_05JG013 / p_05JG014 / p_05JG004 / p_ungauged

# ggsave("./R_flow-reconstruction/Anthony/outputs/figures/p_raw_predicted_flows.png", p_raw_predicted_flows, w = 19.7, l = 11.9)


# Full time series (1980-2019) by year ------------------------------------

p_05JG006_year <- sf_daily %>% 
  ggplot(aes(doy, SK05JG006_cms)) + 
  facet_wrap(~ Year, ncol = 10) + 
  geom_line(colour = "#00BFC4", size = 1.25) +
  labs(x = "Day of year", y = flow_lab,
       subtitle = SK05JG006_lab)

p_05JG013_year <- sf_daily %>% 
  ggplot(aes(doy, SK05JG013_cms)) + 
  facet_wrap(~ Year, ncol = 10) + 
  geom_line(colour = "#00BFC4", size = 1.25) +
  labs(x = "Day of year", y = flow_lab,
       subtitle = SK05JG013_lab)

p_05JG014_year <- sf_daily %>% 
  ggplot(aes(doy, SK05JG014_cms)) + 
  facet_wrap(~ Year, ncol = 10) + 
  geom_line(colour = "#00BFC4", size = 1.25) +
  geom_line(data = station_flow, aes(doy, SK05JG014_predicted_cms), colour = "#F8766D", size = 1, alpha = 2/3) +
  labs(x = "Day of year", y = flow_lab,
       subtitle = SK05JG014_lab)

p_05JG004_year <- sf_daily %>% 
  ggplot(aes(doy, SK05JG004_cms)) + 
  facet_wrap(~ Year, ncol = 10) + 
  geom_line(colour = "#00BFC4", size = 1.25) +
  geom_line(data = station_flow, aes(doy, SK05JG004_predicted_cms), colour = "#F8766D", size = 1, alpha = 2/3) +
  labs(x = "Day of year", y = flow_lab,
       subtitle = SK05JG004_lab)

p_ungauged_year <- sf_daily %>% 
  ggplot(aes(doy, Ungauged_predicted_cms)) +
  facet_wrap(~ Year, ncol = 10) +
  geom_line(colour = "#F8766D", size = 1, alpha = 2/3) +
  labs(x = "Day of year", y = flow_lab, 
       subtitle = "Ungauged contribution based on Ridge Creek flow") 

# ggsave("./R_flow-reconstruction/Anthony/outputs/figures/p_05JG006_year.png", p_05JG006_year, w = 19.7, l = 11.9)
# ggsave("./R_flow-reconstruction/Anthony/outputs/figures/p_05JG013_year.png", p_05JG013_year, w = 19.7, l = 11.9)
# ggsave("./R_flow-reconstruction/Anthony/outputs/figures/p_05JG014_year.png", p_05JG014_year, w = 19.7, l = 11.9)
# ggsave("./R_flow-reconstruction/Anthony/outputs/figures/p_05JG004_year.png", p_05JG004_year, w = 19.7, l = 11.9)
# ggsave("./R_flow-reconstruction/Anthony/outputs/figures/p_ungauged_year.png", p_ungauged_year, w = 19.7, l = 11.9)



# Proportional flow plots -------------------------------------------------

prop_006 <- "Proportion of BPL inflow from Diefenbaker (05JG006)"
prop_013 <- "Proportion of BPL inflow from Ridge Creek (05JG013)"
prop_014 <- "Proportion of BPL inflow from Iskwao Creek (05JG014)"
prop_RC_IC <- "Proportion of BPL inflow from Ridge Creek and Iskwao Creek"
prop_RC_IC_U <- "Proportion of BPL inflow from Ridge Creek, Iskwao Creek, and Ungauged contributions"
prop_lab <- "Proportion of flow (%)"

# Diefenbaker / BP Inflow
p_05JG006_proportion <- sf_daily %>% 
  ggplot(aes(date_ymd, SK05JG006_percent)) +
  geom_point(colour = ifelse(station_flow$SK05JG006_percent > 100, "red", "green"), shape = 1) +
  geom_hline(yintercept = 100, colour = "blue") +
  lims(x = c(as.Date("1980-01-01"), as.Date("2019-12-31")),
       y = c(0, 1000)) +
  theme(axis.title.x = element_blank()) +
  labs(y = prop_lab, subtitle = prop_006)

# Ridge Creek / BP Inflow
p_05JG013_proportion <- sf_daily %>% 
  ggplot(aes(date_ymd, SK05JG013_percent)) +
  geom_point(colour = ifelse(station_flow$SK05JG013_percent > 100, "red", "green"), shape = 1) +
  geom_hline(yintercept = 100, colour = "blue") +
  lims(x = c(as.Date("1980-01-01"), as.Date("2019-12-31")),
       y = c(0, NA)) +
  theme(axis.title.x = element_blank()) +
  labs(y = prop_lab, subtitle = prop_013)

# Iskwao Creek / BP Inflow
p_05JG014_proportion <- sf_daily %>% 
  ggplot(aes(date_ymd, SK05JG014_percent)) +
  geom_point(colour = ifelse(station_flow$SK05JG014_percent > 100, "red", "green"), shape = 1) +
  geom_hline(yintercept = 100, colour = "blue") +
  lims(x = c(as.Date("1980-01-01"), as.Date("2019-12-31")),
       y = c(0, 700)) +
  labs(x = "Year", y = prop_lab, subtitle = prop_013)

# (Ridge Creek + Iskwao Creek) / BP Inflow
P_RC_IC_proportion <- sf_daily %>% 
  ggplot(aes(date_ymd, RC_IC_percent)) +
  geom_point(colour = ifelse(station_flow$RC_IC_percent > 100, "red", "green"), shape = 1) +
  geom_hline(yintercept = 100, colour = "blue") +
  lims(x = c(as.Date("1980-01-01"), as.Date("2019-12-31")),
       y = c(0, 800)) +
  labs(x = "Year", y = prop_lab, subtitle = prop_RC_IC)

# (Ridge Creek + Iskwao Creek + Ungauged) / BP  Inflow
P_RC_IC_U_proportion <- sf_daily %>% 
  ggplot(aes(date_ymd, RC_IC_U_percent)) +
  geom_point(colour = ifelse(station_flow$RC_IC_U_percent > 100 | station_flow$RC_IC_U_percent < 0, "red", "green"), shape = 1) +
  geom_hline(yintercept = 100, colour = "blue") +
  geom_hline(yintercept = 0, colour = "blue") +
  lims(x = c(as.Date("1980-01-01"), as.Date("2019-12-31"))) +
  labs(x = "Year", y = prop_lab, subtitle = prop_RC_IC_U)


p_proportional_flows <- p_05JG006_proportion / P_RC_IC_proportion / P_RC_IC_U_proportion

# ggsave("./R_flow-reconstruction/Anthony/outputs/figures/p_proportional_flows.png", p_proportional_flows, w = 14, l = 13)



# Weekly plots ------------------------------------------------------------

sf_weekly %>% 
  ggplot(aes(Week, SK05JG004_combined_cms)) + 
  facet_wrap(~ Year, ncol = 10) +
  geom_line() + 
  geom_point(colour = "white", size = 1.5) +
  geom_point(size = 0.75) +
  labs(y = "Discharge (cms)")


# Monthly plots -----------------------------------------------------------

p_month_SK05JG004 <- sf_monthly %>% 
  ggplot(aes(date_ymd, SK05JG004_combined_cms)) + 
  # facet_wrap(~ Year, ncol = 10) +
  geom_line(col = "grey70") + 
  # geom_point(colour = "white", size = 1.25) +
  # geom_point(size = 1) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Discharge (cms)", x = "",
       subtitle = "Monthly flows at SK05JG004")

p_month_SK05JG014 <- sf_monthly %>% 
  ggplot(aes(date_ymd, SK05JG014_combined_cms)) + 
  # facet_wrap(~ Year, ncol = 10) +
  geom_line(col = "grey70") + 
  # geom_point(colour = "white", size = 1.25) +
  # geom_point(size = 1) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Discharge (cms)", x = "",
       subtitle = "Monthly flows at SK05JG014")

p_month_SK05JG013 <- sf_monthly %>% 
  ggplot(aes(date_ymd, SK05JG013_cms)) + 
  # facet_wrap(~ Year, ncol = 10) +
  geom_line(col = "grey70") + 
  # geom_point(colour = "white", size = 1.25) +
  # geom_point(size = 1) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Discharge (cms)", x = "",
       subtitle = "Monthly flows at SK05JG013")

p_month_SK05JG006 <- sf_monthly %>% 
  ggplot(aes(date_ymd, SK05JG006_cms)) + 
  # facet_wrap(~ Year, ncol = 10) +
  geom_line(col = "grey70") + 
  # geom_point(colour = "white", size = 1.25) +
  # geom_point(size = 1) +
  theme(axis.title.x = element_blank()) +
  labs(y = "Discharge (cms)", x = "",
       subtitle = "Monthly flows at SK05JG006")  

p_month_Ungauged <- sf_monthly %>% 
  ggplot(aes(date_ymd, Ungauged_predicted_cms)) + 
  # facet_wrap(~ Year, ncol = 10) +
  geom_line(col = "grey70") + 
  # geom_point(colour = "white", size = 1.25) +
  # geom_point(size = 1) +
  labs(y = "Discharge (cms)", x = "",
       subtitle = "Ungauged monthly flows") 

p_monthly_flows <- p_month_SK05JG006 / p_month_SK05JG013 / p_month_SK05JG014 / p_month_SK05JG004 / p_month_Ungauged

# ggsave("./R_flow-reconstruction/Anthony/outputs/figures/p_monthly_flows.png", p_monthly_flows, w = 14, l = 14)

p_month_SK05JG006 <- sf_monthly %>% 
  ggplot(aes(date_ymd, SK05JG006_cms)) + 
  geom_line(col = "grey70", size = 1) + 
  theme(axis.title.x = element_blank()) +
  labs(y = flow_lab, x = "",
       subtitle = "Lake Diefenbaker outflow")

p_month_RC_IC <- sf_monthly %>% 
  ggplot(aes(date_ymd, SK05JG006_cms)) + 
  geom_line(col = "grey70", size = 1) + 
  theme(axis.title.x = element_blank()) +
  labs(y = flow_lab, x = "",
       subtitle = "Combined Ridge Creek + Iskwao Creek flows")

p_month_SK05JG004 <- sf_monthly %>% 
  ggplot(aes(date_ymd, SK05JG004_combined_cms)) + 
  geom_line(col = "grey70", size = 1) + 
  labs(y = flow_lab, x = "Year",
       subtitle = "Buffalo Pound Lake inflow")

station_order <- c("Lake Diefenbaker outflow",
                   "Combined Ridge Creek + Iskwao Creek flows",
                   "Buffalo Pound Lake inflow")

p_monthly <- sf_monthly %>%
  select(date_ymd, SK05JG006_cms, RC_IC_cms, SK05JG004_combined_cms) %>% 
  pivot_longer(cols = c(SK05JG006_cms, RC_IC_cms, SK05JG004_combined_cms), 
               names_to = 'station', values_to = 'flow_cms') %>% 
  mutate(station_name = ifelse(station == "SK05JG006_cms", "Lake Diefenbaker outflow",
                              ifelse(station == "RC_IC_cms", "Combined Ridge Creek + Iskwao Creek flows",
                                     ifelse(station == "SK05JG004_combined_cms", "Buffalo Pound Lake inflow", station))),
         station_name = as_factor(station_name),
         station_name = forcats::fct_relevel(station_name, station_order)) %>% 
  ggplot(aes(date_ymd, flow_cms)) + 
  facet_wrap(~station_name, ncol = 1, scales = 'free_x') + 
  geom_line(col = 'grey70', size = 1) + 
  theme_bw(base_size = 13) + 
  labs(x = "Year", y = flow_lab)

ggsave("./R_flow-reconstruction/Anthony/outputs/figures/20220604_p_monthly.png", p_monthly, dpi = 300, width = 7.5, height = 8.38)

p_monthly_2010_2019 <- sf_monthly %>%
  filter(Year %in% c(2010:2019)) %>% 
  select(date_ymd, SK05JG006_cms, RC_IC_cms, SK05JG004_combined_cms) %>% 
  pivot_longer(cols = c(SK05JG006_cms, RC_IC_cms, SK05JG004_combined_cms), 
               names_to = 'station', values_to = 'flow_cms') %>% 
  mutate(station_name = ifelse(station == "SK05JG006_cms", "Lake Diefenbaker outflow",
                               ifelse(station == "RC_IC_cms", "Combined Ridge Creek + Iskwao Creek flows",
                                      ifelse(station == "SK05JG004_combined_cms", "Buffalo Pound Lake inflow", station))),
         station_name = as_factor(station_name),
         station_name = forcats::fct_relevel(station_name, station_order)) %>% 
  ggplot(aes(date_ymd, flow_cms)) + 
  facet_wrap(~station_name, ncol = 1, scales = 'free_x') + 
  geom_line(col = 'grey70', size = 1) + 
  theme_bw(base_size = 13) + 
  labs(x = "Year", y = flow_lab)

p_monthly + p_monthly_2010_2019 + patchwork::plot_annotation(tag_levels = 'A')

p_month_doc <- doc_monthly %>% 
  ggplot(aes(date_ymd, DOC_mg.L)) + 
  geom_line(col = "forestgreen", size = 1) + 
  labs(y = DOC_lab, x = "Year",
       subtitle = "DOC concentration")

p_month_doc / p_month_SK05JG006 / p_month_RC_IC / p_month_SK05JG004



# Quantile plots ----------------------------------------------------------

p_quan_d <- sf_daily %>% 
  select(Year:date_ymd, SK05JG004_combined_cms, SK05JG006_cms, SK05JG013_cms, SK05JG014_predicted_cms) %>% 
  filter(!is.na(SK05JG006_cms)) %>% 
  group_by(doy) %>% 
  summarise(quants = scales::percent(c(0.05, 0.95)),
            dief_quants = quantile(SK05JG006_cms, c(0.05, 0.95))) %>%
  ggplot(aes(doy, dief_quants, col = quants)) + 
  geom_line(size = 1) + 
  geom_line(data = sf_daily %>% filter(Year == 2016), aes(doy, SK05JG006_cms))


# EEMs sampling dates -----------------------------------------------------

source("./R_data-cleaning/EEMs/code/clean-EEMs.R")
source("./R_EEMs/code/EEMs-plots.R")

eems_dates <- eems_dates() %>% mutate(valbp = 50, valrc = 20, valdief = 15)

eems_dates16 <- eems_dates %>% filter(Year == 2016) #, !doy %in% c(110, 266))
eems_dates17 <- eems_dates %>% filter(Year == 2017)
eems_dates18 <- eems_dates %>% filter(Year == 2018, !doy == 145)
eems_dates19 <- eems_dates %>% filter(Year == 2019) 


# BPL inflow quantiles ----------------------------------------------------

bpi2016 <- sf_daily %>% 
  select(Year, doy, SK05JG004_combined_cms) %>% 
  filter(Year == 2016 & !is.na(SK05JG004_combined_cms)) %>% 
  rename(bpi2016 = SK05JG004_combined_cms) %>% 
  select(-Year)

bpi2017 <- sf_daily %>% 
  select(Year, doy, SK05JG004_combined_cms) %>% 
  filter(Year == 2017 & !is.na(SK05JG004_combined_cms)) %>% 
  rename(bpi2017 = SK05JG004_combined_cms) %>% 
  select(-Year)

bpi2018 <- sf_daily %>% 
  select(Year, doy, SK05JG004_combined_cms) %>% 
  filter(Year == 2018 & !is.na(SK05JG004_combined_cms)) %>% 
  rename(bpi2018 = SK05JG004_combined_cms) %>% 
  select(-Year)

bpi2019 <- sf_daily %>% 
  select(Year, doy, SK05JG004_combined_cms) %>% 
  filter(Year == 2019 & !is.na(SK05JG004_combined_cms)) %>% 
  rename(bpi2019 = SK05JG004_combined_cms) %>% 
  select(-Year)

bpi5 <- sf_daily %>% 
  select(Year, doy, SK05JG004_combined_cms) %>% 
  filter(!is.na(SK05JG004_combined_cms)) %>% 
  group_by(doy) %>% 
  summarise(quant5 = scales::percent(c(0.05)),
            bpi5 = quantile(SK05JG004_combined_cms, c(0.05))) %>% 
  select(-quant5)

bpi95 <- sf_daily %>% 
  select(Year, doy, SK05JG004_combined_cms) %>% 
  filter(!is.na(SK05JG004_combined_cms)) %>% 
  group_by(doy) %>% 
  summarise(quant95 = scales::percent(c(0.95)),
            bpi95 = quantile(SK05JG004_combined_cms, c(0.95))) %>% 
  select(-quant95)

bpi_quants2016 <- bpi2016 %>% full_join(bpi5) %>% full_join(bpi95)
bpi_quants2017 <- bpi2017 %>% full_join(bpi5) %>% full_join(bpi95)
bpi_quants2018 <- bpi2018 %>% full_join(bpi5) %>% full_join(bpi95)
bpi_quants2019 <- bpi2019 %>% full_join(bpi5) %>% full_join(bpi95)

p_2016_b <- bpi_quants2016 %>% 
  ggplot(aes(doy, bpi2016)) +
  geom_point(data = eems_dates16, aes(doy, valbp), col = 'red', size = 1.5, shape = 4) + 
  # geom_vline(xintercept = eems_dates16$doy, col = 'red', alpha = 2/5, size = 0.5, lty = 2) +
  geom_line(size = 1, col = "white") +
  geom_line(data = bpi_quants2016, aes(doy, bpi5), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(data = bpi_quants2016, aes(doy, bpi95), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(size = 1, col = "steelblue") +
  theme_classic2(base_size = 14) +
  labs(x = NULL, y = NULL, subtitle = "2016")

p_2017_b <- bpi_quants2017 %>% 
  ggplot(aes(doy, bpi2017)) +
  geom_point(data = eems_dates17, aes(doy, valbp), col = 'red', size = 1.5, shape = 4) + 
  # geom_vline(xintercept = eems_dates17$doy, col = 'red', alpha = 2/5, size = 0.5, lty = 2) +
  geom_line(size = 1, col = "white") +
  geom_line(data = bpi_quants2017, aes(doy, bpi5), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(data = bpi_quants2017, aes(doy, bpi95), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(size = 1, col = "steelblue") +
  theme_classic2(base_size = 14) +
  labs(x = NULL, y = NULL, subtitle = "2017")

p_2018_b <- bpi_quants2018 %>% 
  ggplot(aes(doy, bpi2018)) +
  geom_point(data = eems_dates18, aes(doy, valbp), col = 'red', size = 1.5, shape = 4) + 
  # geom_vline(xintercept = eems_dates18$doy, col = 'red', alpha = 2/5, size = 0.5, lty = 2) +
  geom_line(size = 1, col = "white") +
  geom_line(data = bpi_quants2018, aes(doy, bpi5), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(data = bpi_quants2018, aes(doy, bpi95), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(size = 1, col = "steelblue") +
  theme_classic2(base_size = 14) +
  labs(x = NULL, y = NULL, subtitle = "2018")

p_2019_b <- bpi_quants2019 %>% 
  ggplot(aes(doy, bpi2019)) +
  geom_point(data = eems_dates19, aes(doy, valbp), col = 'red', size = 1.5, shape = 4) + 
  # geom_vline(xintercept = eems_dates19$doy, col = 'red', alpha = 2/5, size = 0.5, lty = 2) +
  geom_line(size = 1, col = "white") +
  geom_line(data = bpi_quants2019, aes(doy, bpi5), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(data = bpi_quants2019, aes(doy, bpi95), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(size = 1, col = "steelblue") +
  theme_classic2(base_size = 14) +
  labs(x = "Day of year", y = NULL, subtitle = "2019")

p_bpi_quants <- p_2016_b / p_2017_b / p_2018_b / p_2019_b



# Catchment quantiles -----------------------------------------------------

rcic2016 <- sf_daily %>% 
  select(Year, doy, RC_IC_cms) %>% 
  filter(Year == 2016 & !is.na(RC_IC_cms)) %>% 
  rename(rcic2016 = RC_IC_cms) %>% 
  select(-Year)

rcic2017 <- sf_daily %>% 
  select(Year, doy, RC_IC_cms) %>% 
  filter(Year == 2017 & !is.na(RC_IC_cms)) %>% 
  rename(rcic2017 = RC_IC_cms) %>% 
  select(-Year)

rcic2018 <- sf_daily %>% 
  select(Year, doy, RC_IC_cms) %>% 
  filter(Year == 2018 & !is.na(RC_IC_cms)) %>% 
  rename(rcic2018 = RC_IC_cms) %>% 
  select(-Year)

rcic2019 <- sf_daily %>% 
  select(Year, doy, RC_IC_cms) %>% 
  filter(Year == 2019 & !is.na(RC_IC_cms)) %>% 
  rename(rcic2019 = RC_IC_cms) %>% 
  select(-Year)

rcic5 <- sf_daily %>% 
  select(Year, doy, RC_IC_cms) %>% 
  filter(!is.na(RC_IC_cms)) %>% 
  group_by(doy) %>% 
  summarise(quant5 = scales::percent(c(0.05)),
            rcic5 = quantile(RC_IC_cms, c(0.05))) %>% 
  select(-quant5)

rcic95 <- sf_daily %>% 
  select(Year, doy, RC_IC_cms) %>% 
  filter(!is.na(RC_IC_cms)) %>% 
  group_by(doy) %>% 
  summarise(quant95 = scales::percent(c(0.95)),
            rcic95 = quantile(RC_IC_cms, c(0.95))) %>% 
  select(-quant95)

rcic_quants2016 <- rcic2016 %>% full_join(rcic5) %>% full_join(rcic95)
rcic_quants2017 <- rcic2017 %>% full_join(rcic5) %>% full_join(rcic95)
rcic_quants2018 <- rcic2018 %>% full_join(rcic5) %>% full_join(rcic95)
rcic_quants2019 <- rcic2019 %>% full_join(rcic5) %>% full_join(rcic95)

p_2016_c <- rcic_quants2016 %>% 
  ggplot(aes(doy, rcic2016)) +
  geom_point(data = eems_dates16, aes(doy, valrc), col = 'red', size = 1.5, shape = 4) + 
  geom_line(size = 1, col = "white") +
  geom_line(data = rcic_quants2016, aes(doy, rcic5), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(data = rcic_quants2016, aes(doy, rcic95), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(size = 1, col = "forestgreen") +
  theme_classic2(base_size = 14) +
  labs(x = NULL, y = NULL, subtitle = "2016")

p_2017_c <- rcic_quants2017 %>% 
  ggplot(aes(doy, rcic2017)) +
  geom_point(data = eems_dates17, aes(doy, valrc), col = 'red', size = 1.5, shape = 4) + 
  geom_line(size = 1, col = "white") +
  geom_line(data = rcic_quants2017, aes(doy, rcic5), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(data = rcic_quants2017, aes(doy, rcic95), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(size = 1, col = "forestgreen") +
  theme_classic2(base_size = 14) +
  labs(x = NULL, y = NULL, subtitle = "2017")

p_2018_c <- rcic_quants2018 %>% 
  ggplot(aes(doy, rcic2018)) +
  geom_point(data = eems_dates18, aes(doy, valrc), col = 'red', size = 1.5, shape = 4) + 
  geom_line(size = 1, col = "white") +
  geom_line(data = rcic_quants2018, aes(doy, rcic5), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(data = rcic_quants2018, aes(doy, rcic95), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(size = 1, col = "forestgreen") +
  theme_classic2(base_size = 14) +
  labs(x = NULL, y = NULL, subtitle = "2018")

p_2019_c <- rcic_quants2019 %>% 
  ggplot(aes(doy, rcic2019)) +
  geom_point(data = eems_dates19, aes(doy, valrc), col = 'red', size = 1.5, shape = 4) + 
  geom_line(size = 1, col = "white") +
  geom_line(data = rcic_quants2019, aes(doy, rcic5), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(data = rcic_quants2019, aes(doy, rcic95), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(size = 1, col = "forestgreen") +
  theme_classic2(base_size = 14) +
  labs(x = "Day of year", y = NULL, subtitle = "2019")

p_rcic_quants <- p_2016_c / p_2017_c / p_2018_c / p_2019_c


# Diefenbaker quantiles  --------------------------------------------------

dief2016 <- sf_daily %>% 
  select(Year, doy, SK05JG006_cms) %>% 
  filter(Year == 2016 & !is.na(SK05JG006_cms)) %>% 
  rename(dief2016 = SK05JG006_cms) %>% 
  select(-Year)

dief2017 <- sf_daily %>% 
  select(Year, doy, SK05JG006_cms) %>% 
  filter(Year == 2017 & !is.na(SK05JG006_cms)) %>% 
  rename(dief2017 = SK05JG006_cms) %>% 
  select(-Year)

dief2018 <- sf_daily %>% 
  select(Year, doy, SK05JG006_cms) %>% 
  filter(Year == 2018 & !is.na(SK05JG006_cms)) %>% 
  rename(dief2018 = SK05JG006_cms) %>% 
  select(-Year)

dief2019 <- sf_daily %>% 
  select(Year, doy, SK05JG006_cms) %>% 
  filter(Year == 2019 & !is.na(SK05JG006_cms)) %>% 
  rename(dief2019 = SK05JG006_cms) %>% 
  select(-Year)

dief5 <- sf_daily %>% 
  select(Year, doy, SK05JG006_cms) %>% 
  filter(!is.na(SK05JG006_cms)) %>% 
  group_by(doy) %>% 
  summarise(quant5 = scales::percent(c(0.05)),
            dief5 = quantile(SK05JG006_cms, c(0.05))) %>% 
  select(-quant5)

dief95 <- sf_daily %>% 
  select(Year, doy, SK05JG006_cms) %>% 
  filter(!is.na(SK05JG006_cms)) %>% 
  group_by(doy) %>% 
  summarise(quant95 = scales::percent(c(0.95)),
            dief95 = quantile(SK05JG006_cms, c(0.95))) %>% 
  select(-quant95)

dief_quants2016 <- dief2016 %>% full_join(dief5) %>% full_join(dief95)
dief_quants2017 <- dief2017 %>% full_join(dief5) %>% full_join(dief95)
dief_quants2018 <- dief2018 %>% full_join(dief5) %>% full_join(dief95)
dief_quants2019 <- dief2019 %>% full_join(dief5) %>% full_join(dief95)

p_2016_d <- dief_quants2016 %>% 
  ggplot(aes(doy, dief2016)) +
  geom_point(data = eems_dates16, aes(doy, valdief), col = 'red', size = 1.5, shape = 4) + 
  geom_line(size = 1, col = "white") +
  geom_line(data = dief_quants2016, aes(doy, dief5), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(data = dief_quants2016, aes(doy, dief95), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(size = 1, col = "black") +
  theme_classic2(base_size = 14) +
  labs(x = NULL, y = flow_lab, subtitle = "2016")

p_2017_d <- dief_quants2017 %>% 
  ggplot(aes(doy, dief2017)) +
  geom_point(data = eems_dates17, aes(doy, valdief), col = 'red', size = 1.5, shape = 4) + 
  geom_line(size = 1, col = "white") +
  geom_line(data = dief_quants2017, aes(doy, dief5), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(data = dief_quants2017, aes(doy, dief95), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(size = 1, col = "black") +
  theme_classic2(base_size = 14) +
  labs(x = NULL, y = flow_lab, subtitle = "2017")

p_2018_d <- dief_quants2018 %>% 
  ggplot(aes(doy, dief2018)) +
  geom_point(data = eems_dates18, aes(doy, valdief), col = 'red', size = 1.5, shape = 4) + 
  geom_line(size = 1, col = "white") +
  geom_line(data = dief_quants2018, aes(doy, dief5), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(data = dief_quants2018, aes(doy, dief95), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(size = 1, col = "black") +
  theme_classic2(base_size = 14) +
  labs(x = NULL, y = flow_lab, subtitle = "2018")

p_2019_d <- dief_quants2019 %>% 
  ggplot(aes(doy, dief2019)) +
  geom_point(data = eems_dates19, aes(doy, valdief), col = 'red', size = 1.5, shape = 4) + 
  geom_line(size = 1, col = "white") +
  geom_line(data = dief_quants2019, aes(doy, dief5), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(data = dief_quants2019, aes(doy, dief95), col = "grey70", size = 1, alpha = 3/4) +
  geom_line(size = 1, col = "black") +
  theme_classic2(base_size = 14) +
  labs(x = "Day of year", y = flow_lab, subtitle = "2019")

p_dief_quants <- p_2016_d / p_2017_d / p_2018_d / p_2019_d


p_quants <- (p_2016_d + p_2016_c + p_2016_b) / (p_2017_d + p_2017_c + p_2017_b) / (p_2018_d + p_2018_c + p_2018_b) / (p_2019_d + p_2019_c + p_2019_b) 

# 1440 x 796

ggsave("./R_EEMs/outputs/figures/20220627_quantile_flow_plots.png", p_quants, width = 9.6, height = 7.5)

# annual daily flows
adf_bp <- bpi_quants2016 %>% 
  full_join(bpi_quants2017) %>% 
  full_join(bpi_quants2018) %>% 
  full_join(bpi_quants2019) %>% 
  summarise_at(vars(bpi2016:bpi2019), mean, na.rm = TRUE) %>% 
  select(bpi2016, bpi2017:bpi2019, bpi5:bpi95) %>% 
  pivot_longer(cols = everything(), names_to = "siteyear", values_to = "flow_annual")
# 

adf_rcic <- rcic_quants2016 %>% 
  full_join(rcic_quants2017) %>% 
  full_join(rcic_quants2018) %>% 
  full_join(rcic_quants2019) %>% 
  summarise_at(vars(rcic2016:rcic2019), mean, na.rm = TRUE) %>% 
  select(rcic2016, rcic2017:rcic2019, rcic5:rcic95) %>% 
  pivot_longer(cols = everything(), names_to = "siteyear", values_to = "flow_annual")

adf_dief <- dief_quants2016 %>% 
  full_join(dief_quants2017) %>% 
  full_join(dief_quants2018) %>% 
  full_join(dief_quants2019) %>% 
  summarise_at(vars(dief2016:dief2019), mean, na.rm = TRUE) %>% 
  select(dief2016, dief2017:dief2019, dief5:dief95) %>% 
  pivot_longer(cols = everything(), names_to = "siteyear", values_to = "flow_annual")

