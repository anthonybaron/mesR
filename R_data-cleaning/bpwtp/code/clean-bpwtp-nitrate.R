library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(patchwork)

theme_set(theme_bw(base_size = 11))

source("./R_data-cleaning/bpwtp/code/clean-bpwtp.R")

NO3_lab <- expression(paste("NO"[3]*" (mg L"^-1*")")) 


# Data --------------------------------------------------------------------

bp_longterm_nitrate <- clean_bp_longterm() %>% filter(parameter == "Nitrate") # n = 1559, NAs = 699
bp_historical_nitrate <- clean_bp_historical() %>% filter(parameter == "Nitrate") # n = 1574, NAs = 812
# bp_masterfile_nitrate <- clean_bp_masterfile() %>% filter(parameter == "Nitrate") # n = 1461, NAs = 894

bp_longterm_nitrate %>% 
  ggplot(aes(yday(date_ymd), result)) + 
  facet_wrap(~ year) +
  geom_point()

bp_historical_nitrate %>% 
  ggplot(aes(yday(date_ymd), result)) + 
  facet_wrap(~ year) +
  geom_point()

# bp_masterfile_nitrate %>% 
#   ggplot(aes(yday(date_ymd), result)) + 
#   facet_wrap(~ year) +
#   geom_point()


nitrate_2001 <- read_excel("./R_data-cleaning/bpwtp/data/raw/ROUTINE LAB DATA 2001(v2).xlsx",
                           sheet = "Weekly Data", col_names = TRUE, range = "A8:BD58") %>% 
  filter(Parameters == "Nitrate") %>% 
  select(-c("...3":"...4")) %>% 
  pivot_longer(cols = -c(Parameters, Units), names_to = "date_ymd", values_to = "result") %>% 
  mutate_at(c("date_ymd", "result"), as.numeric) %>% 
  mutate(date_ymd = excel_numeric_to_date(date_ymd),
         year = year(date_ymd), 
         month = month(date_ymd), 
         week = week(date_ymd)) %>% 
  select(parameter = Parameters, unit = Units, date_ymd, year, month, week, result) 

bp_longterm_nitrate_sans2001 <- bp_longterm_nitrate %>% filter(!year == 2001) 

bp_longterm_nitrate_infill <- bind_rows(bp_longterm_nitrate_sans2001, nitrate_2001) %>% 
  arrange(date_ymd)

bp_longterm_nitrate_infill %>% 
  ggplot(aes(yday(date_ymd), result)) +
  facet_wrap(~ year) +
  geom_point()

bp_longterm_nitrate_infill %>% 
  filter(!is.na(result)) %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  mutate(rate_d = n / 365,
         rate_w = n / 52,
         rate_m = n / 12) 

bp_nitrate_month <- bp_longterm_nitrate_infill %>% 
  group_by(year, month) %>% 
  summarise(NO3_mg.L = mean(result, na.rm = TRUE)) %>% 
  ungroup()

bp_nitrate_month %>% filter(is.na(NO3_mg.L)) # 2 NAs

bp_nitrate_monthly <- function(df = bp_nitrate_month) {
  
  bp_nitrate <- df %>% 
    mutate(rownum = row_number(),
           NO3_mg.L = ifelse(rownum == 60, mean(c(0, 0.15)), 
                             ifelse(rownum == 310, mean(c(0, 0.04)), NO3_mg.L))) %>% 
    unite("year_month", c(year, month), sep = "-", remove = FALSE) %>% 
    mutate(date_ymd = paste0(year_month, "-01"),
           date_ymd = ymd(date_ymd)) %>% 
    select(date_ymd, year, month, NO3_mg.L)
  
  return(bp_nitrate)
  
}


# bp_nitrate_monthly <- bp_nitrate_monthly()
# 
# bp_nitrate_monthly %>% filter(is.na(NO3_mg.L))
# 
# bp_nitrate_monthly %>% 
#   ggplot(aes(month, NO3_mg.L)) + 
#   facet_wrap(~ year) + 
#   geom_hline(yintercept = 0.0970, col = "steelblue") + 
#   geom_hline(yintercept = 0.04, col = "pink") +
#   geom_line() + 
#   geom_point() 
# 
# bp_nitrate_monthly %>% 
#   summarise(NO3_mean = mean(NO3_mg.L, na.rm = TRUE),
#             NO3_median = median(NO3_mg.L, na.rm = TRUE))
