library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(patchwork)

theme_set(theme_bw(base_size = 11))

source("./R_data-cleaning/bpwtp/code/clean-bpwtp.R")

NH3_lab <- expression(paste("NH"[3]*" (mg N L"^-1*")")) 

mypink <- "#F8766D"
myblue <- "#00BFC4"
mygreen <- "#7CAE00"
mypurple <- "#C77CFF"


# Data --------------------------------------------------------------------

bp_longterm_ammonia <- clean_bp_longterm() %>% filter(parameter == "Ammonia N") # n = 1559, NAs = 758
bp_historical_ammonia <- clean_bp_historical() %>% filter(parameter == "Ammonia N") # n = 1574, NAs = 866
# bp_masterfile_ammonia <- clean_bp_masterfile() %>% filter(parameter == "Ammonia N") # n = 1461, NAs = 757

bp_longterm_ammonia %>% 
  ggplot(aes(yday(date_ymd), result)) + 
  facet_wrap(~ year) +
  geom_point(col = myblue)

bp_historical_ammonia %>% 
  ggplot(aes(yday(date_ymd), result)) + 
  facet_wrap(~ year) +
  geom_point(col = mypink)

# bp_masterfile_ammonia %>% 
#   ggplot(aes(yday(date_ymd), result)) + 
#   facet_wrap(~ year) +
#   geom_point(col = mygreen)


ammonia_2001 <- read_excel("./R_data-cleaning/bpwtp/data/raw/ROUTINE LAB DATA 2001(v2).xlsx",
                           sheet = "Weekly Data", col_names = TRUE, range = "A8:BD49") %>% 
  filter(Parameters == "Ammonia N") %>% 
  select(-c("...3":"...4")) %>% 
  pivot_longer(cols = -c(Parameters, Units), names_to = "date_ymd", values_to = "result") %>% 
  mutate_at(c("date_ymd", "result"), as.numeric) %>% 
  mutate(date_ymd = excel_numeric_to_date(date_ymd),
         year = year(date_ymd), 
         month = month(date_ymd), 
         week = week(date_ymd)) %>% 
  select(parameter = Parameters, unit = Units, date_ymd, year, month, week, result) 

bp_longterm_ammonia_sans2001 <- bp_longterm_ammonia %>% filter(!year == 2001) 

bp_longterm_ammonia_infill <- bind_rows(bp_longterm_ammonia_sans2001, ammonia_2001) %>% 
  arrange(date_ymd)

bp_longterm_ammonia_infill %>% 
  ggplot(aes(yday(date_ymd), result)) +
  facet_wrap(~ year) +
  geom_point(col = "steelblue")

bp_ammonia_month <- bp_longterm_ammonia_infill %>% 
  group_by(year, month) %>% 
  summarise(NH3_mg.L = mean(result, na.rm = TRUE)) %>% 
  ungroup()

bp_ammonia_month %>% filter(is.na(NH3_mg.L))
# A tibble: 13 × 3
#     year month NH3_mg.L
#    <dbl> <dbl>    <dbl>
# 1   2002     3      NaN
# 2   2003     1      NaN
# 3   2003     2      NaN
# 4   2003    11      NaN
# 5   2003    12      NaN
# 6   2004     1      NaN
# 7   2004     2      NaN
# 8   2004     3      NaN
# 9   2012    10      NaN
# 10  2012    11      NaN
# 11  2012    12      NaN
# 12  2013    12      NaN
# 13  2017    12      NaN
# 
# January = 2
# February = 2
# March = 2
# October = 1
# November = 2
# December = 4

bp_ammonia_month %>% 
  filter(!is.na(NH3_mg.L)) %>% 
  ggplot(aes(NH3_mg.L)) + 
  facet_wrap(~ month) +
  geom_histogram()

median_monthly_NH3 <- bp_ammonia_month %>% 
  group_by(month) %>% 
  summarise(median_NH3 = median(NH3_mg.L, na.rm = TRUE))
# A tibble: 12 × 2
#   month median_NH3
#   <dbl>      <dbl>
# 1      1     0.128 
# 2      2     0.147 
# 3      3     0.105 
# 4      4     0.0671
# 5      5     0.0425
# 6      6     0.043 
# 7      7     0.069 
# 8      8     0.0862
# 9      9     0.085 
# 10    10     0.07  
# 11    11     0.0658
# 12    12     0.07 

# bp_longterm_ammonia_infill %>% 
#   rename(NH3_mg.L = result) %>% 
#   group_by(month) %>% 
#   summarise(median_NH3_all = median(NH3_mg.L, na.rm = TRUE)) %>% 
#   right_join(median_monthly_NH3) %>% 
#   ggplot(aes(median_NH3_all, median_NH3)) + 
#   geom_point()

bp_ammonia_monthly <- function(df = bp_ammonia_month) {
  
  bp_ammonia_cc <- df %>% 
    mutate(NH3_mg.L = ifelse(is.na(NH3_mg.L), median_monthly_NH3$median_NH3, NH3_mg.L)) %>% 
    unite("year_month", c(year, month), sep = "-", remove = FALSE) %>% 
    mutate(date_ymd = paste0(year_month, "-01"),
           date_ymd = ymd(date_ymd)) %>% 
    select(date_ymd, year, month, NH3_mg.L)
  
  return(bp_ammonia_cc)
  
}

# df1 <- bp_ammonia_monthly()
# 
# df <- bp_ammonia_month %>% 
#   mutate(NH3_mg.L = ifelse(is.na(NH3_mg.L), median_monthly_NH3$median_NH3, NH3_mg.L)) %>% 
#   unite("year_month", c(year, month), sep = "-", remove = FALSE) %>% 
#   mutate(date_ymd = paste0(year_month, "-01"),
#          date_ymd = ymd(date_ymd)) %>% 
#   select(date_ymd, year, month, NH3_mg.L)
#   
# df1 %>% 
#   ggplot(aes(yday(date_ymd), NH3_mg.L)) +
#   facet_wrap(~ year) +
#   geom_line(col = "steelblue") +
#   geom_point(col = "white", size = 2) + 
#   geom_point(col = "steelblue")
# 
# df1 %>% 
#   ggplot(aes(date_ymd, NH3_mg.L)) + 
#   geom_line(col = "steelblue") +
#   geom_point(col = "white", size = 2) + 
#   geom_point(col = "steelblue")

