library(dplyr)
library(lubridate)
library(readxl)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(patchwork)

theme_set(theme_bw(base_size = 11))

source("./R_data-cleaning/bpwtp/code/clean-bpwtp.R")

OrganicN_lab <- expression(paste("Organic N (mg L"^-1*")")) 


# Data --------------------------------------------------------------------

bp_longterm_organicN <- clean_bp_longterm() %>% filter(parameter == "Organic N") %>% # n = 1559, NAs = 736
  select(date_ymd, year, month, week, parameter, result_longterm = result)

bp_historical_organicN <- clean_bp_historical() %>% filter(parameter == "Organic N") %>% # n = 1574, NAs = 749
  mutate(month = month(date_ymd)) %>% 
  select(date_ymd, year, month, week, parameter, result_historical = result)
  
# bp_masterfile_organicN <- clean_bp_masterfile() %>% filter(parameter == "Organic N") %>% # n = 1461, NAs = 660
#   rename(result_masterfile = result)  
  
bp_longterm_organicN %>% 
  ggplot(aes(yday(date_ymd), result_longterm)) + 
  facet_wrap(~ year) +
  geom_line(col = "steelblue") +
  geom_point(col = "white", size = 2) +
  geom_point(col = "steelblue")

bp_historical_organicN %>% 
  ggplot(aes(yday(date_ymd), result_historical)) + 
  facet_wrap(~ year) +
  geom_line(col = "steelblue") +
  geom_point(col = "white", size = 2) +
  geom_point(col = "steelblue")

# bp_masterfile_organicN %>% 
#   ggplot(aes(yday(date_ymd), result_masterfile)) + 
#   facet_wrap(~ year) +
#   geom_line(col = "steelblue") +
#   geom_point(col = "white", size = 2) +
#   geom_point(col = "steelblue")

bp_hist_long_orgN <- bp_historical_organicN %>% full_join(bp_longterm_organicN, by = "date_ymd")

bp_hist_long_orgN %>% 
  ggplot(aes(result_longterm, result_historical, col = year(date_ymd))) + 
  geom_point()

tmp <- bp_hist_long_orgN %>% 
  mutate(result = ifelse(is.na(result_longterm) & !is.na(result_historical), result_historical, result_longterm))

tmp %>% 
  ggplot(aes(yday(date_ymd), result)) +
  facet_wrap(~ year.x) + 
  geom_point()


# not done/...
organicN_2001 <- read_excel("./R_data-cleaning/bpwtp/data/raw/ROUTINE LAB DATA 2001(v2).xlsx",
                           sheet = "Weekly Data", col_names = TRUE, range = "A8:BD59") %>% 
  filter(Parameters == "Organic N") %>% 
  select(-c("...3":"...4")) %>% 
  pivot_longer(cols = -c(Parameters, Units), names_to = "date_ymd", values_to = "result") %>% 
  mutate_at(c("date_ymd", "result"), as.numeric) %>% 
  mutate(date_ymd = excel_numeric_to_date(date_ymd),
         year = year(date_ymd), 
         month = month(date_ymd), 
         week = week(date_ymd)) %>% 
  select(parameter = Parameters, unit = Units, date_ymd, year, month, week, result_longterm = result) 

bp_longterm_organicN_sans2001 <- bp_longterm_organicN %>% filter(!year == 2001) 

bp_longterm_organicN_infill <- bind_rows(bp_longterm_organicN_sans2001, organicN_2001) %>% 
  arrange(date_ymd)

bp_longterm_organicN_infill %>% 
  ggplot(aes(yday(date_ymd), result_longterm)) +
  facet_wrap(~ year) +
  geom_point()

bp_organicN_month <- bp_longterm_organicN_infill %>% 
  group_by(year, month) %>% 
  summarise(orgN_mg.L = mean(result_longterm, na.rm = TRUE)) %>% 
  ungroup()

bp_organicN_month %>% filter(is.na(orgN_mg.L))
# A tibble: 11 × 3
#     year month orgN_mg.L
#    <dbl> <dbl>    <dbl>
#  1  2002     3      NaN
#  2  2003     1      NaN
#  3  2003     2      NaN
#  4  2003     3      NaN
#  5  2003    11      NaN
#  6  2003    12      NaN
#  7  2004     1      NaN
#  8  2004     2      NaN
#  9  2004     3      NaN
# 10  2012    11      NaN
# 11  2017    12      NaN
# 
# January = 2
# February = 2
# March = 3
# November = 2
# December = 2

bp_organicN_month %>% 
  filter(!is.na(orgN_mg.L)) %>% 
  ggplot(aes(orgN_mg.L)) + 
  facet_wrap(~ month) +
  geom_histogram()

median_monthly_orgN <- bp_organicN_month %>% 
  group_by(month) %>% 
  summarise(median_orgN = median(orgN_mg.L, na.rm = TRUE))
# A tibble: 12 × 2
#    month median_orgN
#    <dbl>       <dbl>
#  1     1       0.592
#  2     2       0.565
#  3     3       0.64 
#  4     4       0.665
#  5     5       0.579
#  6     6       0.625
#  7     7       0.908
#  8     8       1.01 
#  9     9       0.902
# 10    10       0.748
# 11    11       0.66 
# 12    12       0.625

bp_organicN_monthly <- function(df = bp_organicN_month) {
  
  bp_organicN_cc <- df %>% 
    mutate(orgN_mg.L = ifelse(is.na(orgN_mg.L), median_monthly_orgN$median_orgN, orgN_mg.L)) %>% 
    unite("year_month", c(year, month), sep = "-", remove = FALSE) %>% 
    mutate(date_ymd = paste0(year_month, "-01"),
           date_ymd = ymd(date_ymd)) %>% 
    select(date_ymd, year, month, orgN_mg.L)
  
  return(bp_organicN_cc)
  
}
# 
# df1 <- bp_organicN_monthly()
# 
# 
# df1 %>%
#   ggplot(aes(yday(date_ymd), orgN_mg.L)) +
#   facet_wrap(~ year) +
#   geom_line(col = "steelblue") +
#   geom_point(col = "white", size = 2) +
#   geom_point(col = "steelblue")
# 
# df1 %>%
#   ggplot(aes(date_ymd, orgN_mg.L)) +
#   geom_line(col = "steelblue") +
#   geom_point(col = "white", size = 2) +
#   geom_point(col = "steelblue")
