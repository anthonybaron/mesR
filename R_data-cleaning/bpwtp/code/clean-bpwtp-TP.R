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

TP_lab <- expression(paste("TP (µg L"^-1*")")) 


# Data --------------------------------------------------------------------

bp_longterm_TP <- clean_bp_longterm() %>% filter(parameter == "Phosphate (total)") # n = 1559, NAs = 770
bp_historical_TP <- clean_bp_historical() %>% filter(parameter == "Phosphate (total)") # n = 1574, NAs = 785
bp_masterfile_TP <- clean_bp_masterfile() %>% filter(parameter == "Phosphate (total)") # n = 1461, NAs = 696

bp_longterm_TP %>% 
  ggplot(aes(yday(date_ymd), result)) + 
  facet_wrap(~ year) +
  geom_point()

bp_historical_TP %>% 
  ggplot(aes(yday(date_ymd), result)) + 
  facet_wrap(~ year) +
  geom_point()

bp_masterfile_TP %>% 
  ggplot(aes(yday(date_ymd), result)) + 
  facet_wrap(~ year) +
  geom_point()


TP_2001 <- read_excel("./R_data-cleaning/bpwtp/data/raw/ROUTINE LAB DATA 2001(v2).xlsx",
                           sheet = "Weekly Data", col_names = TRUE, range = "A8:BD65") %>% 
  filter(Parameters == "Phosphate(total)") %>% 
  select(-c("...3":"...4")) %>% 
  pivot_longer(cols = -c(Parameters, Units), names_to = "date_ymd", values_to = "result") %>% 
  mutate_at(c("date_ymd", "result"), as.numeric) %>% 
  mutate(date_ymd = excel_numeric_to_date(date_ymd),
         year = year(date_ymd), 
         month = month(date_ymd), 
         week = week(date_ymd)) %>% 
  select(parameter = Parameters, unit = Units, date_ymd, year, month, week, result) 

bp_longterm_TP_sans2001 <- bp_longterm_TP %>% filter(!year == 2001) 

bp_longterm_TP_infill <- bind_rows(bp_longterm_TP_sans2001, TP_2001) %>% 
  arrange(date_ymd)

bp_longterm_TP_infill %>% 
  ggplot(aes(yday(date_ymd), result)) +
  facet_wrap(~ year) +
  geom_point()

bp_TP_month <- bp_longterm_TP_infill %>% 
  group_by(year, month) %>% 
  summarise(TP_ug.L = mean(result, na.rm = TRUE)) %>% 
  ungroup()

bp_TP_month %>% filter(is.na(TP_ug.L))
# A tibble: 9 × 3
#    year month TP_ug.L
#   <dbl> <dbl>   <dbl>
# 1  2003    12     NaN
# 2  2004     1     NaN
# 3  2004     2     NaN
# 4  2004     3     NaN
# 5  2004     4     NaN
# 6  2004     5     NaN
# 7  2004     6     NaN
# 8  2004     7     NaN
# 9  2017    12     NaN

median_monthly_TP <- bp_TP_month %>% 
  group_by(month) %>%
  summarise(median_TP = median(TP_ug.L, na.rm = TRUE))

bp_TP_monthly <- function(df = bp_TP_month) {
  
  bp_TP_cc <- df %>% 
    mutate(TP_ug.L = ifelse(is.na(TP_ug.L), median_monthly_TP$median_TP, TP_ug.L)) %>% 
    unite("year_month", c(year, month), sep = "-", remove = FALSE) %>% 
    mutate(date_ymd = paste0(year_month, "-01"),
           date_ymd = ymd(date_ymd)) %>% 
    select(date_ymd, year, month, TP_ug.L)
  
  return(bp_TP_cc)
  
}

# dftp <- bp_TP_monthly()
# 
# dftp %>%
#   ggplot(aes(yday(date_ymd), TP_ug.L)) +
#   facet_wrap(~ year) +
#   geom_line(col = "steelblue") +
#   geom_point(col = "white", size = 2) +
#   geom_point(col = "steelblue")
# 
# dftp %>%
#   ggplot(aes(date_ymd, TP_ug.L)) +
#   geom_line(col = "steelblue") +
#   geom_point(col = "white", size = 2) +
#   geom_point(col = "steelblue")
