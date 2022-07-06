library(tidyverse)
library(lubridate)

theme_set(theme_bw(base_size = 16))

DOC_label <- expression(paste("DOC concentration (mg L"^-1*")")) 

BP_longterm <- read_csv("data/BPWTP_labdat_current.csv") %>% 
  filter(datasheet == "RawWater") %>% 
  select(-c(datasheet))


# DOC stats ---------------------------------------------------------------

doc_longterm <- BP_longterm %>% 
  filter(parm_unit == "DOC.GFdiss_mg.L.C") %>% 
  mutate(parm_unit = ifelse(parm_unit == "DOC.GFdiss_mg.L.C", "DOC_mg.L", parm_unit),
         month = factor(month, levels = month.abb)) %>% 
  mutate_if(is.character, as.factor)

# DOC range
doc_longterm %>% 
  summarise(doc_mean = mean(result, na.rm = TRUE),     # 6.66
            doc_sd = sd(result, na.rm = TRUE),         # 1.90
            doc_median = median(result, na.rm = TRUE), # 6.3
            doc_min = min(result, na.rm = TRUE),       # 1.96
            doc_max = max(result, na.rm = TRUE))       # 15

# high DOC years
doc_longterm %>% 
  filter(year %in% c("1998", "1999", "2000", "2014", "2015", "2016")) %>% 
  summarise(doc_mean = mean(result, na.rm = TRUE),     # 9.10
            doc_sd = sd(result, na.rm = TRUE),         # 1.65
            doc_median = median(result, na.rm = TRUE), # 8.9
            doc_min = min(result, na.rm = TRUE),       # 5.9
            doc_max = max(result, na.rm = TRUE))       # 15

doc_longterm %>% 
  filter(year %in% c("1998", "1999", "2000")) %>% 
  summarise(doc_mean = mean(result, na.rm = TRUE),     # 9.37
            doc_sd = sd(result, na.rm = TRUE),         # 1.79
            doc_median = median(result, na.rm = TRUE), # 8.8
            doc_min = min(result, na.rm = TRUE),       # 6.6
            doc_max = max(result, na.rm = TRUE))       # 15

doc_longterm %>% 
  filter(year %in% c("2014", "2015", "2016")) %>% 
  summarise(doc_mean = mean(result, na.rm = TRUE),     # 8.82
            doc_sd = sd(result, na.rm = TRUE),         # 1.45
            doc_median = median(result, na.rm = TRUE), # 9.0
            doc_min = min(result, na.rm = TRUE),       # 5.9
            doc_max = max(result, na.rm = TRUE))       # 11.4

# low DOC years 
doc_longterm %>% 
  filter(year %in% c("1986", "1987", "1988", "1989", "1990", "1993")) %>% 
  summarise(doc_mean = mean(result, na.rm = TRUE),     # 4.62
            doc_sd = sd(result, na.rm = TRUE),         # 0.915
            doc_median = median(result, na.rm = TRUE), # 4.5
            doc_min = min(result, na.rm = TRUE),       # 1.96
            doc_max = max(result, na.rm = TRUE))       # 8.3

# Sulphate stats ----------------------------------------------------------

SO4_longterm <- BP_longterm %>% 
  filter(parm_unit == "sulphate_mg.L") %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  mutate_if(is.character, as.factor)

SO4_longterm %>% 
  ggplot(aes(datetime_ymd.hms, result)) + 
  geom_point()

# SO4 range
SO4_longterm %>% 
  summarise(SO4_mean = mean(result, na.rm = TRUE),     # 111
            SO4_sd = sd(result, na.rm = TRUE),         # 41.2
            SO4_median = median(result, na.rm = TRUE), # 99
            SO4_min = min(result, na.rm = TRUE),       # 41
            SO4_max = max(result, na.rm = TRUE))       # 340


# Chlorophyll stats -------------------------------------------------------


