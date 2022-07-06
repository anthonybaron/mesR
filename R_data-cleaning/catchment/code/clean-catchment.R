library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(janitor)
library(patchwork)

source("./R_data-cleaning/bpwtp/code/clean-bpwtp-DOC.R")

theme_set(theme_bw(base_size = 14))

DOC_lab <- expression(paste("DOC concentration (mg L"^-1*")")) 

bp_doc <- DOC_complete_1990_2019() 

bp_doc_yr <- bp_doc %>% 
  group_by(year) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE))


# Lake Diefenbaker --------------------------------------------------------
ca_dief <- read_excel("./R_data-cleaning/catchment/data/raw/QuAppelle-System-Database_1979-v2.xlsx",
           sheet = "1 L. Dief", col_names = TRUE, col_types = "text", range = "B1:BY43") %>% 
  rename(Parameters = "...1", Units = `Dates:`) %>% 
  select(-c("...3")) %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  filter(!Parameters %in% c("Physical Parameters", "Major Ions", "Trace Constiuents")) %>% 
  pivot_longer(cols = -c(Parameters, Units), names_to = "date_ymd", values_to = "result") %>%
  mutate(date_ymd = as.numeric(date_ymd),
         date_ymd = excel_numeric_to_date(date_ymd, date_system = "modern"),
         year = year(date_ymd), 
         month = month(date_ymd), 
         week = week(date_ymd),
         result = as.numeric(result),
         Parameters = ifelse(is.na(Parameters), "Flow", Parameters),
         Units = ifelse(Units == "Flow:", "cms", Units),
         site = "Lake Diefenbaker") %>% 
  select(site, parameter = Parameters, unit = Units, date_ymd, year, month, week, result) 


# Qu'Appelle Dam ----------------------------------------------------------
ca_qdam <- read_excel("./R_data-cleaning/catchment/data/raw/QuAppelle-System-Database_1979-v2.xlsx",
                      sheet = "2 Q.Dam", col_names = TRUE, col_types = "text", range = "B1:BY43") %>% 
  rename(Parameters = "...1", Units = `Dates:`) %>% 
  select(-c("...3")) %>% 
  select(-c(`27-Feb-79`)) %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  filter(!Parameters %in% c("Physical Parameters", "Major Ions", "Trace Constiuents")) %>% 
  pivot_longer(cols = -c(Parameters, Units), names_to = "date_ymd", values_to = "result") %>%
  mutate(date_ymd = as.numeric(date_ymd),
         date_ymd = excel_numeric_to_date(date_ymd, date_system = "modern"),
         date_ymd = as.character(date_ymd),
         date_ymd = ifelse(is.na(date_ymd), "1996-10-21", date_ymd),
         date_ymd = ymd(date_ymd),
         year = year(date_ymd), 
         month = month(date_ymd), 
         week = week(date_ymd),
         result = as.numeric(result),
         Parameters = ifelse(is.na(Parameters), "Flow", Parameters),
         Units = ifelse(Units == "Flow:", "cms", Units),
         site = "Qu'Appelle Dam") %>% 
  select(site, parameter = Parameters, unit = Units, date_ymd, year, month, week, result) 


# Eyebrow Lake ------------------------------------------------------------
ca_eyebrow <- read_excel("./R_data-cleaning/catchment/data/raw/QuAppelle-System-Database_1979-v2.xlsx",
                      sheet = "3 Eyebrow", col_names = TRUE, col_types = "text", range = "B1:BY43") %>% 
  rename(Parameters = "...1", Units = `Dates:`) %>% 
  select(-c("...3")) %>% 
  select(-c(`27-Feb-79`)) %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  filter(!Parameters %in% c("Physical Parameters", "Major Ions", "Trace Constiuents")) %>% 
  pivot_longer(cols = -c(Parameters, Units), names_to = "date_ymd", values_to = "result") %>%
  mutate(date_ymd = as.numeric(date_ymd),
         date_ymd = excel_numeric_to_date(date_ymd, date_system = "modern"),
         date_ymd = as.character(date_ymd),
         date_ymd = ifelse(is.na(date_ymd), "1996-10-21", date_ymd),
         date_ymd = ymd(date_ymd),
         year = year(date_ymd), 
         month = month(date_ymd), 
         week = week(date_ymd),
         result = as.numeric(result),
         Parameters = ifelse(is.na(Parameters), "Flow", Parameters),
         Units = ifelse(Units == "Flow:", "cms", Units),
         site = "Eyebrow Lake") %>% 
  select(site, parameter = Parameters, unit = Units, date_ymd, year, month, week, result) 


# BPL West ----------------------------------------------------------------
ca_bp_west <- read_excel("./R_data-cleaning/catchment/data/raw/QuAppelle-System-Database_1979-v2.xlsx",
                         sheet = "6 BPL West", col_names = TRUE, col_types = "text", range = "B1:BY43") %>% 
  rename(Parameters = "...1", Units = `Dates:`) %>% 
  select(-c("...3")) %>% 
  select(-c(`27-Feb-79`)) %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  filter(!Parameters %in% c("Physical Parameters", "Major Ions", "Trace Constiuents")) %>% 
  pivot_longer(cols = -c(Parameters, Units), names_to = "date_ymd", values_to = "result") %>%
  mutate(date_ymd = as.numeric(date_ymd),
         date_ymd = excel_numeric_to_date(date_ymd, date_system = "modern"),
         date_ymd = as.character(date_ymd),
         date_ymd = ifelse(is.na(date_ymd), "1996-10-21", date_ymd),
         date_ymd = ymd(date_ymd),
         year = year(date_ymd), 
         month = month(date_ymd), 
         week = week(date_ymd),
         result = as.numeric(result),
         Parameters = ifelse(is.na(Parameters), "Flow", Parameters),
         Units = ifelse(Units == "Flow:", "cms", Units),
         site = "Buffalo Pound Lake West") %>% 
  select(site, parameter = Parameters, unit = Units, date_ymd, year, month, week, result) 



# BPL Lake ----------------------------------------------------------------
ca_bp_lake <- read_excel("./R_data-cleaning/catchment/data/raw/QuAppelle-System-Database_1979-v2.xlsx",
                         sheet = "7 BPL Lake", col_names = TRUE, col_types = "text", range = "B1:BY43") %>% 
  rename(Parameters = "...1", Units = `Dates:`) %>% 
  select(-c("...3")) %>% 
  select(-c(`27-Feb-79`)) %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  filter(!Parameters %in% c("Physical Parameters", "Major Ions", "Trace Constiuents")) %>% 
  pivot_longer(cols = -c(Parameters, Units), names_to = "date_ymd", values_to = "result") %>%
  mutate(date_ymd = as.numeric(date_ymd),
         date_ymd = excel_numeric_to_date(date_ymd, date_system = "modern"),
         date_ymd = as.character(date_ymd),
         date_ymd = ifelse(is.na(date_ymd), "1996-10-21", date_ymd),
         date_ymd = ymd(date_ymd),
         year = year(date_ymd), 
         month = month(date_ymd), 
         week = week(date_ymd),
         result = as.numeric(result),
         Parameters = ifelse(is.na(Parameters), "Flow", Parameters),
         Units = ifelse(Units == "Flow:", "cms", Units),
         site = "Buffalo Pound Lake") %>% 
  select(site, parameter = Parameters, unit = Units, date_ymd, year, month, week, result) 


# Join sites (DOC) --------------------------------------------------------------
ca_dief_doc <- ca_dief %>% 
  filter(parameter == "D.O.C.", year %in% c(1990:2019)) %>% # N = 43
  rename(DOC_dief = result) 
  # group_by(year) %>% 
  # summarise(DOC_dief = mean(DOC_dief, na.rm = TRUE)) # n = 24

ca_qdam_doc <- ca_qdam %>% 
  filter(parameter == "D.O.C.", year %in% c(1990:2019)) %>% # N = 43
  rename(DOC_qdam = result) 
  # group_by(year) %>% 
  # summarise(DOC_qdam = mean(DOC_qdam, na.rm = TRUE)) # n = 23

ca_eyebrow_doc <- ca_eyebrow %>% 
  filter(parameter == "D.O.C.", year %in% c(1990:2019)) %>% # N = 43
  rename(DOC_eyebrow = result) 
  # group_by(year) %>% 
  # summarise(DOC_eyebrow = mean(DOC_eyebrow, na.rm = TRUE)) # n = 22

ca_bp_west_doc <- ca_bp_west %>% 
  filter(parameter == "D.O.C.", year %in% c(1990:2019)) %>% # N = 43
  rename(DOC_bp_west = result) 
  # group_by(year) %>% 
  # summarise(DOC_bp_west = mean(DOC_bp_west, na.rm = TRUE)) # n = 22

ca_bp_lake_doc <- ca_bp_west %>% 
  filter(parameter == "D.O.C.", year %in% c(1990:2019)) %>% # N = 43
  rename(DOC_bp_lake = result) 
  # group_by(year) %>% 
  # summarise(DOC_bp_lake = mean(DOC_bp_lake, na.rm = TRUE)) # n = 22

ca_doc <- bp_doc %>% 
  select(date_ymd, year, DOC_mg.L) %>% 
  rename(DOC_bp = DOC_mg.L) %>% 
  left_join(ca_dief_doc) %>% 
  left_join(ca_qdam_doc) %>% 
  left_join(ca_eyebrow_doc) %>% 
  left_join(ca_bp_west_doc) %>% 
  left_join(ca_bp_lake_doc) %>% 
  rename("Buffalo Pound long-term" = DOC_bp,
         "Diefenbaker" = DOC_dief,
         "Qu'Appelle Dam" = DOC_qdam,
         "Eyebrow" = DOC_eyebrow,
         "Buffalo Pound West" = DOC_bp_west,
         "Buffalo Pound Lake" = DOC_bp_lake) %>% 
  select(-c(year, parameter, unit, month, week)) %>% 
  pivot_longer(cols = -c(date_ymd), names_to = "site_name", values_to = "result") 


# Plots -------------------------------------------------------------------

fct_order <- c("Diefenbaker",
               "Qu'Appelle Dam", 
               "Eyebrow",
               "Buffalo Pound Lake")

ca_doc %>% 
  filter(!site_name == "Buffalo Pound long-term") %>%
  filter(!site_name == "Buffalo Pound West") %>%
  rename(Site = site_name) %>% 
  ggplot(aes(yday(date_ymd), result, col = Site)) +
  facet_wrap(~ year(date_ymd), ncol = 10) + 
  geom_line(size = 1) + 
  geom_point(col = "white", size = 3) + 
  geom_point(size = 2) +
  theme(legend.position = "bottom") + 
  scale_colour_colorblind() + 
  labs(x = NULL, y = DOC_lab)

ca_doc %>% 
  filter(!is.na(result),
         !site_name %in% c("Buffalo Pound long-term", "Buffalo Pound West")) %>%
  rename(Site = site_name) %>%
  mutate(Site = fct_relevel(Site, fct_order)) %>% 
  ggplot(aes(date_ymd, result, col = Site)) +
  geom_line(data = ca_doc %>% filter(site_name == "Buffalo Pound long-term",
                                     date_ymd <= "2012-06-18"), 
            aes(date_ymd, result), col = "grey75", size = 1) +
  geom_line(size = 1) + 
  geom_point(size = 3) +
  theme(legend.position = "bottom") + 
  scale_colour_colorblind() + 
  labs(x = NULL, y = DOC_lab)

# ca_doc %>% 
#   ggplot(aes(date_ymd, result, col = site_name)

ca_doc %>%   
  filter(!site_name == "Buffalo Pound West") %>% 
  rename(Site = site_name) %>% 
  # mutate(Site = fct_relevel(Site, fct_order)) %>% 
  ggplot(aes(year, result, col = Site)) + 
  geom_line(size = 1) + 
  geom_point(col = "white", size = 3) + 
  geom_point(size = 2) +
  theme(legend.position = "bottom") + 
  scale_colour_colorblind() + 
  labs(x = NULL, y = DOC_lab)

ca_doc %>%   
  filter(!site_name == "Buffalo Pound West") %>% 
  rename(Site = site_name) %>% 
  mutate(Site = fct_relevel(Site, fct_order)) %>% 
  ggplot(aes(year, result, col = Site)) + 
  facet_wrap(~ Site, ncol = 1) +
  geom_line(size = 1) + 
  geom_point(col = "white", size = 3) + 
  geom_point(size = 2) +
  theme(legend.position = "bottom") + 
  scale_colour_colorblind() + 
  labs(x = NULL, y = DOC_lab) + 
  theme(legend.position = "none")

ca_doc %>% 
  filter(site_name %in% c("Buffalo Pound", "Buffalo Pound Lake")) %>%  
  ggplot(aes(year, result)) + 
  facet_wrap(~ site_name, ncol = 1) +
  geom_line() + 
  geom_point()




# Workspace ---------------------------------------------------------------

ca_qdam %>% 
  filter(parameter == "D.O.C.", year %in% c(1990:2014)) %>% 
  group_by(year) %>% 
  summarise(result = mean(result, na.rm = TRUE)) %>% 
  ggplot(aes(year, result)) + 
  geom_line(size = 1) + 
  geom_point(col = "white", size = 3) +
  geom_point(size = 2)

ca_dief %>% 
  filter(parameter == "D.O.C.", year %in% c(1990:2014)) %>% 
  group_by(year) %>% 
  summarise(result = mean(result, na.rm = TRUE)) %>% 
  ggplot(aes(year, result)) + 
  geom_line(size = 1) + 
  geom_point(col = "white", size = 3) +
  geom_point(size = 2) +
  geom_line(data = bp_doc_yr, aes(year, DOC_mg.L), col = "steelblue", size = 1) + 
  geom_point(data = bp_doc_yr, aes(year, DOC_mg.L), col = "white", size = 3) +
  geom_point(data = bp_doc_yr, aes(year, DOC_mg.L), col = "steelblue", size = 2) +
  labs(x = NULL, y = DOC_lab,
       subtitle = "Annual Buffalo Pound (blue) and Diefenbaker (black) DOC concentrations")


# Data export -------------------------------------------------------------

q_system <- bind_rows(
  ca_bp_lake,
  ca_bp_west,
  ca_eyebrow,
  ca_qdam,
  ca_dief
) %>% 
  select(-c(year, month, week))

write_csv(q_system, "~/Documents/masters_gschool/sulfate-mass-balance/dief-eyebrow-bp.csv")
  