library(dplyr)
library(lubridate)
library(readr)
library(readxl)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(patchwork)
library(janitor)

theme_set(theme_bw(base_size = 11))

source("./R_data-cleaning/bpwtp/code/clean-bpwtp.R")

Chla_lab <- expression(paste("Chlorophyll ", italic("a"), " (µg L"^-1*")")) 


# Data --------------------------------------------------------------------
bp_longterm <- clean_bp_longterm() %>% filter(parameter == "Chlorophyll a") # n = 1559, NAs = 661
bp_historical <- clean_bp_historical() %>% filter(parameter == "Chlorophyll a") # n = 1574, NAs = 691
bp_masterfile <- clean_bp_masterfile() %>% filter(parameter == "Chlorophyll a") # n = 1461, NAs = 611


# Cleaning ----------------------------------------------------------------

ll <- bp_longterm %>% 
  filter(parm_unit == "chla_ug.L") %>% 
  group_by(year, month) %>% 
  summarise(result = mean(result, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(nn = 0,
         mm = ifelse(month < 10, paste0(nn, month, sep = ""), month),
         dd = "01",
         date_ymd = paste(year, mm, dd, sep = "-"),
         date_ymd = ymd(date_ymd))

filter(ll, is.na(result))
# A tibble: 9 × 7
#    year month result    nn mm    dd    date_ymd  
#   <dbl> <dbl>  <dbl> <dbl> <chr> <chr> <date>    
# 1  2001     9    NaN     0 09    01    2001-09-01
# 2  2001    10    NaN     0 10    01    2001-10-01
# 3  2001    11    NaN     0 11    01    2001-11-01
# 4  2001    12    NaN     0 12    01    2001-12-01
# 5  2002     3    NaN     0 03    01    2002-03-01
# 6  2003     1    NaN     0 01    01    2003-01-01
# 7  2003     2    NaN     0 02    01    2003-02-01
# 8  2007    12    NaN     0 12    01    2007-12-01
# 9  2017    12    NaN     0 12    01    2017-12-01

mm <- bp_masterfile %>% 
  filter(parameter == "Chlorophyll a") %>% 
  mutate(month = month(date_ymd)) %>% 
  group_by(year, month) %>% 
  summarise(result = mean(result, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(nn = 0,
         mm = ifelse(month < 10, paste0(nn, month, sep = ""), month),
         dd = "01",
         date_ymd = paste(year, mm, dd, sep = "-"),
         date_ymd = ymd(date_ymd))

# Fill in missing Chl a in longterm df via:
# 2001-09-01 from masterfile == 46.5
# 2001-10-01 from masterfile == 28.4
# 2001-11-01 from masterfile ==  9.25
# 2001-12-01 from masterfule == 12.3
# 2002-03-01 impute w nearest neighbour == 7.79
# 2003-01-01 impute w nearest neighbour == 20.0
# 2003-02-01 impute w nearest neighbour == 20.0
# 2007-12-01 impute w nearest neighbour == 24.9
# 2017-12-01 impute w nearest neighbour == 24.5

filter(mm, date_ymd == "2001-09-01") # 46.5
filter(mm, date_ymd == "2001-10-01") # 28.4
filter(mm, date_ymd == "2001-11-01") #  9.25
filter(mm, date_ymd == "2001-12-01") # 12.3

ll$result <- ifelse(is.na(ll$result) & ll$date_ymd == "2001-09-01", 46.5, ll$result)
ll$result <- ifelse(is.na(ll$result) & ll$date_ymd == "2001-10-01", 28.4, ll$result)
ll$result <- ifelse(is.na(ll$result) & ll$date_ymd == "2001-11-01", 9.25, ll$result)
ll$result <- ifelse(is.na(ll$result) & ll$date_ymd == "2001-12-01", 12.3, ll$result)

ll %>% # "2002-03-01" == 7.79
  mutate(date_ymd = as.character(date_ymd)) %>% 
  filter(date_ymd %in% c("2002-02-01", "2002-03-01", "2002-04-01")) %>% 
  summarise(result = mean(result, na.rm = TRUE)) 

ll %>% # "2003-01-01" & "2003-02-01" == 20.0
  mutate(date_ymd = as.character(date_ymd)) %>% 
  filter(date_ymd %in% c("2002-12-01", "2003-01-01", "2003-02-01", "2003-03-01")) %>% 
  summarise(result = mean(result, na.rm = TRUE)) 

ll %>% # "2007-12-01" == 24.9
  mutate(date_ymd = as.character(date_ymd)) %>% 
  filter(date_ymd %in% c("2007-11-01", "2007-12-01", "2008-01-01")) %>% 
  summarise(result = mean(result, na.rm = TRUE)) 

ll %>% # "2017-12-01" == 24.5
  mutate(date_ymd = as.character(date_ymd)) %>% 
  filter(date_ymd %in% c("2017-11-01", "2017-12-01", "2018-01-01")) %>% 
  summarise(result = mean(result, na.rm = TRUE)) 

ll$result <- ifelse(is.na(ll$result) & ll$date_ymd == "2002-03-01", 7.79, ll$result)
ll$result <- ifelse(is.na(ll$result) & ll$date_ymd == "2003-01-01", 20.0, ll$result)
ll$result <- ifelse(is.na(ll$result) & ll$date_ymd == "2003-02-01", 20.0, ll$result)
ll$result <- ifelse(is.na(ll$result) & ll$date_ymd == "2007-12-01", 24.9, ll$result)
ll$result <- ifelse(is.na(ll$result) & ll$date_ymd == "2017-12-01", 24.5, ll$result)

bp_Chla_monthly <- function(df = ll) {
  
  Chla <- df %>% 
    select(date_ymd, year, month, result) %>% 
    rename(chla_ug.L = result)
  
  return(Chla)
  
}


