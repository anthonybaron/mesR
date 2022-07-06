library(dplyr)
library(lubridate)
library(readr)
library(readxl)
library(tidyr)
library(ggplot2)
library(patchwork)
library(janitor)

theme_set(theme_bw(base_size = 11))
SO4_lab <- expression(paste("SO"[4]*" (mg L"^-1*")")) 


# Data --------------------------------------------------------------------

source("./R_data-cleaning/bpwtp/code/clean-bpwtp.R")

bp_longterm_sulphate <- clean_bp_longterm() %>% filter(parameter == "Sulphate") # n = 1559, NAs = 680
bp_historical_sulphate <- clean_bp_historical() %>% filter(parameter == "Sulphate") # n = 1574, NAs = 692
bp_masterfile_sulphate <- clean_bp_masterfile() %>% filter(parameter == "Sulphate") # n = 1461, NAs = 604

bp_2001 <- read_excel("./R_data-cleaning/bpwtp/data/raw/ROUTINE LAB DATA 2001(v2).xlsx",
           sheet = "Weekly Data", col_names = TRUE, range = "A8:BD41") %>% 
  filter(Parameters == "Sulphate") %>% 
  select(-c("...3":"...4")) %>% 
  pivot_longer(cols = -c(Parameters, Units), names_to = "date_ymd", values_to = "result") %>% 
  mutate_at(c("date_ymd", "result"), as.numeric) %>% 
  mutate(date_ymd = excel_numeric_to_date(date_ymd))



# Sulphate infilling and cleaning -----------------------------------------

plot_sulphate <- function(years = "") {
  
  p1 <- bp_longterm_sulphate %>% 
    ggplot(aes(yday(date_ymd), result)) + 
    facet_wrap(~ year, nrow = 1) +
    geom_point(col = "steelblue") +
    labs(subtitle = "bp_longterm", x = NULL)
  
  p2 <- bp_historical_sulphate %>% 
    ggplot(aes(yday(date_ymd), result)) + 
    facet_wrap(~ year, nrow = 1) +
    geom_point(col = "#F8766D") +
    labs(subtitle = "bp_historical", x = NULL)
  
  p3 <- bp_masterfile_sulphate %>%  
    ggplot(aes(yday(date_ymd), result)) + 
    facet_wrap(~ year, nrow = 1) +
    geom_point(col = "forestgreen") +
    labs(subtitle = "bp_masterfile", x = NULL)
  
  return(p1 / p2 / p3)
  
}

p_90_94 <- plot_sulphate(years = c(1990:1994))
p_95_99 <- plot_sulphate(years = c(1995:1999))
p_00_04 <- plot_sulphate(years = c(2000:2004))
p_05_09 <- plot_sulphate(years = c(2005:2009))
p_10_14 <- plot_sulphate(years = c(2010:2014))
p_15_19 <- plot_sulphate(years = c(2015:2019))

bp_longterm_sulphate %>% 
  ggplot(aes(yday(date_ymd), result)) + 
  facet_wrap(~ year) +
  geom_point()

bp_historical_sulphate %>% 
  ggplot(aes(yday(date_ymd), result)) + 
  facet_wrap(~ year) +
  geom_point()

bp_masterfile_sulphate %>% 
  ggplot(aes(yday(date_ymd), result)) + 
  facet_wrap(~ year) +
  geom_point()


bp_longterm_sulphate_sans2001 <- bp_longterm_sulphate %>% filter(!year == 2001)
bp_masterfile_sulphate_2001 <- bp_masterfile_sulphate %>% filter(year == 2001)

bp_longterm_sulphate_infill <- bind_rows(bp_longterm_sulphate_sans2001, bp_masterfile_sulphate_2001) %>% 
  arrange(date_ymd)

bp_longterm_sulphate_infill %>% 
  ggplot(aes(yday(date_ymd), result)) + 
  facet_wrap(~ year) +
  geom_point()

bp_sulphate_all <- function(df = bp_longterm_sulphate_infill) {
  
  df <- df
  
  return(df)
  
}



bp_sulphate_month <- bp_longterm_sulphate_infill %>% 
  group_by(year, month) %>% 
  summarise(SO4_mg.L = mean(result, na.rm = TRUE),
            count = n(),
            count_NA = sum(is.na(result))) %>% 
  ungroup()
# N = 360 (30 years * 12 months = 360 observations)
# NAs = 1 (March 2009)

bp_sulphate_month %>% 
  ggplot(aes(month, SO4_mg.L)) + 
  facet_wrap(~ year) +
  geom_line(alpha = 9/10) +
  geom_point(size = 2)


bp_sulphate_monthly <- function(df = bp_sulphate_month) {
  
  bp_sulph <- df %>% 
    ungroup() %>% 
    mutate(row_num = row_number(),
           SO4_mg.L = ifelse(row_num == 231, mean(c(118, 114)), SO4_mg.L)) %>% 
    unite("year_month", c(year, month), sep = "-", remove = FALSE) %>% 
    mutate(date_ymd = paste0(year_month, "-01"),
           date_ymd = ymd(date_ymd)) %>% 
    select(date_ymd, year, month, SO4_mg.L)
  
  return(bp_sulph)
  
}


