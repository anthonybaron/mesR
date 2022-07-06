library(readxl)
library(readr) 
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(janitor)

source("./R_EEMs/code/read-and-prep-raw-files.R")

DOC_lab <- expression(paste("DOC (mg L"^-1*")")) 

eems_all <- bp_all()

eems <- eems_all %>% 
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
                           "0.5 mi Below Causeway West", "Buoy 0.8 m", "Buoy 2.8 m")) 


eems_doc <- eems %>% select(file_name:site_name, source, date_ymd, Year, Month, DOY, DOC_mg.L, A254)

eems_doc %>% 
  filter(!is.na(Year)) %>% 
  ggplot(aes(DOY, A254, colour = source)) +
  facet_wrap(~ Year) +
  geom_point()
  
eems_doc %>% 
  filter(Year == "2019") %>% 
  ggplot(aes(DOY, A254, colour = source)) +
  facet_wrap(~ site_name) +
  geom_point() +
  theme(legend.position = "bottom")

eems_doc %>% 
  filter(Year == "2015") %>% 
  ggplot(aes(DOY, A254, colour = source)) +
  facet_wrap(~ site_name) +
  geom_point() +
  theme(legend.position = "bottom")



eems_doc %>% 
  ggplot(aes(DOY, DOC_mg.L)) + 
  geom_point() 

# Missing DOC data:
eems_doc %>% 
  filter(!is.na(Year)) %>% 
  group_by(Year) %>% 
  summarise(DOC_NA_count = sum(is.na(DOC_mg.L)))
# A tibble: 5 × 2
#   Year  DOC_NA_count
#   <fct>        <int>
# 1 2015            68
# 2 2016             0
# 3 2017             0
# 4 2018             8
# 5 2019            75

eems_doc %>% 
  filter(!is.na(site_name) & !is.na(Year) & Year == 2019) %>% 
  mutate(DOC_mg.L = ifelse(is.na(DOC_mg.L), 2, DOC_mg.L),
         Data = ifelse(DOC_mg.L == 2, "Missing", "Not missing")) %>% 
  ggplot(aes(DOY, DOC_mg.L, colour = Data, shape = Year)) + 
  facet_wrap(~ site_name) +
  geom_point(size = 2, alpha = 3/4) +
  theme(legend.position = "bottom") +
  labs(x = "Day of Year", y = DOC_lab)

eems_doc %>% 
  filter(A254 > 1) %>% 
  ggplot(aes(DOY, A254)) + 
  geom_point() 

eems_doc %>% 
  filter(!is.na(site_name) & !is.na(Year) & A254 > 1) %>% 
  mutate(A254 = ifelse(is.na(A254), 5, A254),
         missing = ifelse(A254 == 5, "Missing", "Not missing")) %>% 
  ggplot(aes(DOY, A254, colour = missing, shape = Year)) + 
  facet_wrap(~ site_name) +
  geom_point() +
  theme(legend.position = "bottom")
