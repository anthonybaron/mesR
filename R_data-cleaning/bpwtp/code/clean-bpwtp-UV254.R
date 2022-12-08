library(dplyr)
library(lubridate)
library(readr)
library(readxl)
library(tidyr)
library(ggplot2)
library(patchwork)
library(janitor)

theme_set(theme_bw(base_size = 12))
UV254_lab <- expression(paste("abs 10"))

source("./R_data-cleaning/bpwtp/code/clean-bpwtp.R")

uv254_longterm_raw <- clean_bp_longterm()
hh_raw <- clean_bp_historical()
mm_raw <- clean_bp_masterfile()

uv254_longterm <- subset(uv254_longterm_raw, parameter == "UV 254") # n = 1559, NAs = 422
hh <- subset(hh_raw, parameter == "UV 254") # n = 784, NAs = 414
mm <- subset(mm_raw, parameter == "UV 254") # n = 1461, NAs = 485

bp_2001 <- read_excel("./R_data-cleaning/bpwtp/data/raw/ROUTINE LAB DATA 2001(v2).xlsx",
                      sheet = "Weekly Data", col_names = TRUE, range = "A8:BD62") %>% 
  filter(Parameters == "Org. Carbon (diss @ 254nm)") %>% 
  select(-c("...3":"...4")) %>% 
  pivot_longer(cols = -c(Parameters, Units), names_to = "date_ymd", values_to = "result") %>% 
  mutate_at(c("date_ymd", "result"), as.numeric) %>% 
  mutate(date_ymd = excel_numeric_to_date(date_ymd)) %>% 
  rename(parameter = Parameters,
         unit = Units) %>% 
  mutate(parameter = ifelse(parameter == "Org. Carbon (diss @ 254nm)", "UV 254", NA),
         year = 2001,
         month = month(date_ymd),
         week = week(date_ymd))

uv254_longterm %>% ggplot(aes(date_ymd, result)) + geom_point()
uv254_longterm %>% filter(year == 2001) %>% ggplot(aes(date_ymd, result)) + geom_point()
hh %>% ggplot(aes(date_ymd, result)) + geom_point()
mm %>% ggplot(aes(date_ymd, result)) + geom_point()
bp_2001 %>% ggplot(aes(date_ymd, result)) + geom_point()

uv254_longterm_sans2001 <- subset(uv254_longterm, !year == 2001)
uv254_longterm <- bind_rows(uv254_longterm_sans2001, bp_2001) %>% arrange(date_ymd)

UV254_month <- uv254_longterm %>% 
  group_by(year, month) %>% 
  summarise(UV254 = mean(result, na.rm = TRUE),
            count = n(),
            count_NA = sum(is.na(result))) %>% 
  ungroup() %>% 
  mutate(date_ymd = ymd(paste(year, month, "01", sep = '-')))

UV254_month %>% ggplot(aes(date_ymd, UV254)) + geom_point()
UV254_month %>% ggplot(aes(yday(date_ymd), UV254)) + geom_point() + facet_wrap(~ year, ncol = 10)

bp_UV254_monthly <- function(df = UV254_month) {
  
  UV254 <- df %>% select(date_ymd, year, month, UV254)
  
  return(UV254)
  
}

