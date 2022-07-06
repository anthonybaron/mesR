library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(lubridate)

# write functions for daily, weekly, and monthly station flow then source this 
# script to time-series-plots.R and wavelet.R 

# Station flows for Diefenbaker, Ridge Creek, Iskwao Creek, Qu'Appelle River
# above Buffalo Pound, and the Ungauged portion of the catchment infilled with 
# modeled data (Nazemi's work) 
station_flow_raw <- read_csv("./R_data-cleaning/flow-reconstruction/data/processed/BP-flow-infilled-with-proportions_(1972-2019).csv")

stations <- c("SK05JG004_cms", "SK05JG004_combined_cms", "SK05JG004_predicted_cms",
              "SK05JG006_cms", "SK05JG013_cms", "SK05JG014_cms", "SK05JG014_combined_cms",
              "SK05JG014_predicted_cms", "Ungauged_predicted_cms", "RC_IC_cms")

station_flow_daily <- function(df = station_flow_raw, write = FALSE, 
                               outdir = "./R_data-cleaning/flow-reconstruction/data/processed/") {
  
  daily <- df %>% 
    filter(Year %in% c(1990:2019)) %>%
    mutate(RC_IC_cms = SK05JG013_cms + SK05JG014_combined_cms) %>% 
    mutate(Season = as.factor(Season)) %>% 
    mutate_if(is.character, as.numeric) %>%  
    select(Year, Month, doy, date_ymd, Season, everything()) %>% 
    # Add additional proportional flows...
    mutate(RC_IC_percent = (SK05JG014_combined_cms + SK05JG013_cms) / SK05JG004_combined_cms * 100,
           RC_IC_U_percent = (SK05JG014_combined_cms + SK05JG013_cms + Ungauged_predicted_cms) / SK05JG004_combined_cms * 100)
  
  outname <- paste0(Sys.Date(), "_", "station_flow_daily.csv")
  outpath <- file.path(outdir, outname)
  
  if (isTRUE(write)) {
    write.csv(daily, file = outpath)
  }
  
  return(daily)
  
}

station_flow_weekly <- function(df = station_flow_raw, write = FALSE, 
                                outdir = "./R_data-cleaning/flow-reconstruction/data/processed/") {
  
  weekly <- df %>%
    filter(Year %in% c(1990:2019)) %>%
    mutate(RC_IC_cms = SK05JG013_cms + SK05JG014_combined_cms) %>% 
    mutate(Week = week(date_ymd)) %>% 
    select(Year, Month, Week, everything()) %>% 
    group_by(Year, Week) %>% 
    summarise_at(stations, mean, na.rm = TRUE) %>% 
    ungroup() 
  
  outname <- paste0(Sys.Date(), "_", "station_flow_weekly.csv")
  outpath <- file.path(outdir, outname)
  
  if (isTRUE(write)) {
    write.csv(weekly, file = outpath)
  }
  
  return(weekly)
  
}


station_flow_monthly <- function(df = station_flow_raw, write = FALSE, 
                                 outdir = "./R_data-cleaning/flow-reconstruction/data/processed/") {
  
  monthly <- df %>%
    filter(Year %in% c(1990:2019)) %>%
    mutate(RC_IC_cms = SK05JG013_cms + SK05JG014_combined_cms) %>% 
    group_by(Year, Month) %>% 
    summarise_at(stations, mean, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate(date_ymd = ifelse(Month %in% c(1:9), "-0", "-"),
           date_ymd = paste0(Year, date_ymd, Month, "-01"),
           date_ymd = ymd(date_ymd)) %>% 
    select(date_ymd, everything()) %>% 
    ungroup()
  
  outname <- paste0(Sys.Date(), "_", "station_flow_monthly.csv")
  outpath <- file.path(outdir, outname)
  
  if (isTRUE(write)) {
    write.csv(monthly, file = outpath)
  }
  
  return(monthly)
  
}


station_lat_long <- function() {
  
  site_name <- c("Lake Diefenbaker outflow", "Ridge Creek", "Iskwao Creek",
                 "Buffalo Pound inflow")
  
  site_longname <- c("Elbow Diversion Canal at Drop Structure",
                     "Ridge Creek Near Bridgeford", 
                     "Iskwao Creek Near Craik",
                     "Qu'Appelle River Above Buffalo Pound Lake")
  
  station_num <- c("SK05JG006", "SK05JG013", "SK05JG014", "SK05JG004")
  
  latitude <- c(50.97, 50.95, 50.97, 50.78)
  
  longitude <- c(-106.39, -106.32, -105.95, -105.82)
  
  df <- tibble(
     site_name = site_name,
     site_longname = site_longname,
     station_num = station_num,
     latitude = latitude,
     longitude = longitude
  )
  
  return(df)
  
}

# write.csv(sll, "./R_data-cleaning/flow-reconstruction/data/clean/stations-lat-long.csv")
