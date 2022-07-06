library(dplyr)
library(lubridate)
library(readr)
library(readxl)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(patchwork)

theme_set(theme_bw(base_size = 11))

source("./R_data-cleaning/bpwtp/code/clean-bpwtp.R")

SRP_lab <- expression(paste("SRP (µg L"^-1*")")) 


# Data --------------------------------------------------------------------

bp_longterm_SRP <- clean_bp_longterm() %>% filter(parameter == "Phosphate (ortho)") # n = 1559, NAs = 764
bp_historical_SRP <- clean_bp_historical() %>% filter(parameter == "Phosphate (ortho)") # n = 1574, NAs = 804
bp_masterfile_SRP <- clean_bp_masterfile() %>% filter(parameter == "Phosphate (ortho)") # n = 1461, NAs = 794

bp_longterm_SRP %>% 
  ggplot(aes(yday(date_ymd), result)) + 
  facet_wrap(~ year) +
  geom_point()

bp_historical_SRP %>% 
  ggplot(aes(yday(date_ymd), result)) + 
  facet_wrap(~ year) +
  geom_point()

bp_masterfile_SRP %>% 
  ggplot(aes(yday(date_ymd), result)) + 
  facet_wrap(~ year) +
  geom_point()


SRP_2001 <- read_excel("./R_data-cleaning/bpwtp/data/raw/ROUTINE LAB DATA 2001(v2).xlsx",
                      sheet = "Weekly Data", col_names = TRUE, range = "A8:BD65") %>% 
  filter(Parameters == "Phosphate(ortho)") %>% 
  select(-c("...3":"...4")) %>% 
  pivot_longer(cols = -c(Parameters, Units), names_to = "date_ymd", values_to = "result") %>% 
  mutate_at(c("date_ymd", "result"), as.numeric) %>% 
  mutate(date_ymd = excel_numeric_to_date(date_ymd),
         year = year(date_ymd), 
         month = month(date_ymd), 
         week = week(date_ymd)) %>% 
  select(parameter = Parameters, unit = Units, date_ymd, year, month, week, result) 

bp_longterm_SRP_sans2001 <- bp_longterm_SRP %>% filter(!year == 2001) 

bp_longterm_SRP_infill <- bind_rows(bp_longterm_SRP_sans2001, SRP_2001) %>% 
  arrange(date_ymd)

bp_longterm_SRP_infill %>% 
  ggplot(aes(yday(date_ymd), result)) +
  facet_wrap(~ year) +
  geom_point()

bp_SRP_month <- bp_longterm_SRP_infill %>% 
  group_by(year, month) %>% 
  summarise(SRP_ug.L = mean(result, na.rm = TRUE)) %>% 
  ungroup()

bp_SRP_month %>% filter(is.na(SRP_ug.L))
# A tibble: 10 × 3
#     year month SRP_ug.L
#    <dbl> <dbl>    <dbl>
#  1  2003    12      NaN
#  2  2004     1      NaN
#  3  2004     2      NaN
#  4  2004     3      NaN
#  5  2004     4      NaN
#  6  2004     5      NaN
#  7  2004     6      NaN
#  8  2004     7      NaN
#  9  2015     5      NaN
# 10  2017    12      NaN

median_monthly_SRP <- bp_SRP_month %>% 
  group_by(month) %>%
  summarise(median_SRP = median(SRP_ug.L, na.rm = TRUE))

bp_SRP_monthly <- function(df = bp_SRP_month) {
  
  bp_SRP_cc <- df %>% 
    mutate(SRP_ug.L = ifelse(is.na(SRP_ug.L), median_monthly_SRP$median_SRP, SRP_ug.L)) %>% 
    unite("year_month", c(year, month), sep = "-", remove = FALSE) %>% 
    mutate(date_ymd = paste0(year_month, "-01"),
           date_ymd = ymd(date_ymd)) %>% 
    select(date_ymd, year, month, SRP_ug.L)
  
  return(bp_SRP_cc)
  
}


bpsrp <- bp_SRP_monthly()

bpsrp %>%
  ggplot(aes(yday(date_ymd), SRP_ug.L)) +
  facet_wrap(~ year) +
  geom_line(col = "steelblue") +
  geom_point(col = "white", size = 2) +
  geom_point(col = "steelblue")

bpsrp %>%
  ggplot(aes(date_ymd, SRP_ug.L)) +
  geom_line(col = "steelblue") +
  geom_point(col = "white", size = 2) +
  geom_point(col = "steelblue")
