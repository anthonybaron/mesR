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

# Data prep ---------------------------------------------------------------

# Parameters (15):
# conductivity, pH, sulphate, ammonia, nitrate, organic nitrogen, chlorophyll a,
# TOC, DOC, A254, TP, SRP, blue-green algae, green algae, total blue-green + 
# green algae

clean_bp_longterm <- function() {
  
  bp_longterm_raw <- read_csv("./R_data-cleaning/bpwtp/data/raw/BPWTP_labdat_current.csv") # nrow = 368,269
  
  bp_longterm <- bp_longterm_raw %>% # nrow = 29,753
    filter(datetime_ymd.hms >= "1983-01-01", 
           datasheet %in% c("RawWater", "doc_profile"),
           station == "Raw",
           parameter %in% c("Sulphate", "Conductivity", "pH", "Chlorophyll a", "Nitrate",
                            "TOC", "DOC", "UV 254", "Phosphate (ortho)", "Phosphate (total)",
                            "Blue Green Algae", "Green Algae", "Ammonia N", "Total BG + G", "Organic N")) %>% 
    select(datasheet, station, datetime_ymd.hms, year, week, parameter, unit, parm_unit, result) %>% 
    mutate(date_ymd = ymd(str_split(datetime_ymd.hms, " ", simplify = TRUE)[, 1]),
           month = month(date_ymd)) %>% 
    select(datasheet, station, date_ymd, year, month, week:result) %>% 
    filter(year %in% c(1990:2019))
  
  return(bp_longterm)
  
}

clean_bp_masterfile <- function() { 
  
  bp_masterfile_raw <- read_xlsx("./R_data-cleaning/bpwtp/data/raw/MASTERFILEWTPStandardMeasurementsWORKINGApr26_2018.xlsx",
                                 sheet = "Sheet1")
  
  bp_masterfile <- bp_masterfile_raw %>% 
    select(SampleDateTime, year, month, week, cond, ph, sulfate, chla, ammn,
           no3, orgn, rawtoc, rawdoc, uv254doc, orthop, tp, blgrlg, grlg, totgrnbg) %>% 
    filter(SampleDateTime >= "1983-01-01 00:00:00") %>%
    mutate(date_ymd = ymd(str_split(SampleDateTime, " ", simplify = TRUE)[, 1])) %>% 
    rename(Sulphate = sulfate,
           Conductivity = cond,
           pH = ph, 
           `Chlorophyll a` = chla,
           `Ammonia N` = ammn,
           Nitrate = no3,
           TOC = rawtoc,
           DOC = rawdoc,
           `UV 254` = uv254doc,
           `Phosphate (ortho)` = orthop,
           `Phosphate (total)` = tp,
           `Blue Green Algae` = blgrlg,
           `Green Algae` = grlg,
           `Total BG + G` = totgrnbg,
           `Organic N` = orgn) %>% 
    select(date_ymd, year:`Total BG + G`) %>%
    pivot_longer(cols = c(Conductivity:`Total BG + G`), 
                 names_to = "parameter", values_to = "result") %>% 
    arrange(parameter) %>% 
    filter(year %in% c(1990:2019))
  
  return(bp_masterfile)
  
}

clean_bp_historical <- function() {
  
  bp_historical_raw <- read_csv("./R_data-cleaning/bpwtp/data/raw/20200721_BPWTP_routinelabdata_historicalbase_scraped.csv") # nrow = 307,981
  
  bp_historical <- bp_historical_raw %>% 
    filter(datetime_ymd.hms >= "1983-01-01", 
           # datetime_ymd.hms <= "2019-12-31",
           station == "Raw",
           parameter %in% c("Sulphate", "Conductivity", "pH", "Chlorophyll a", "Ammonia N",
                            "Nitrate", "Raw TOC", "Raw DOC (GF diss)", "UV 254", "Phosphate(ortho)",
                            "Phosphate(total)", "Blue Green Algae (x10^-3)", "Green Algae (x10^-3)",
                            "Total BG + G", "Organic N", "Org. Carbon (diss @ 254nm)")) %>% 
    mutate(date_ymd = datetime_ymd.hms,
           month = month(date_ymd),
           year = year(datetime_ymd.hms),
           week = week(date_ymd),
           result = as.numeric(result),
           parameter = ifelse(parameter == "Raw TOC", "TOC",
                              ifelse(parameter == "Raw DOC (GF diss)", "DOC",
                                     ifelse(parameter == "Org. Carbon (diss @ 254nm)", "UV 254",
                                            ifelse(parameter == "Phosphate(ortho)", "Phosphate (ortho)",
                                                   ifelse(parameter == "Phosphate(total)", "Phosphate (total)",
                                                          ifelse(parameter == "Blue Green Algae (x10^-3)", "Blue Green Algae",
                                                                 ifelse(parameter == "Green Algae (x10^-3)", "Green Algae", parameter)))))))) %>% 
    select(datasheet, station, date_ymd, year, week, parameter, unit, result) %>% 
    filter(year %in% c(1990:2019))
  
  return(bp_historical)
  
}

# Plot labels -------------------------------------------------------------

# DOC_lab <- expression(paste("DOC (mg L"^-1*")")) 
# OrganicN_lab <- expression(paste("Organic N (mg L"^-1*")")) 
# NO3_lab <- expression(paste("NO"[3]*" (mg L"^-1*")")) 
# TP_lab <- expression(paste("TP (µg L"^-1*")")) 
# SO4_lab <- expression(paste("SO"[4]*" (mg L"^-1*")"))


# Plot differences between datasets ---------------------------------------

# fn to plot both data sets faceted by year
# plot_facet_year <- function(df1 = "", df2 = "", var = "") {
#   
#   df1 <- df1 %>% 
#     filter(parameter == var,
#            date_ymd >= "1990-01-01") %>%
#     mutate(doy = yday(date_ymd))
#   
#   df2 <- df2 %>% 
#     filter(parameter == var,
#            date_ymd >= "1990-01-01") %>%
#     mutate(doy = yday(date_ymd))
#   
#   pf <- df1 %>% 
#     ggplot(aes(doy, result)) + 
#     facet_wrap(~ year) +
#     geom_point(size = 4, col = "#00BFC4") +
#     geom_point(data = df2, aes(doy, result), size = 2, col = "#F8766D") +
#     labs(y = var, 
#          title = var,
#          subtitle = "blue = bp_longterm, pink = bp_masterfile") +
#     theme(axis.title.x = element_blank())
#   
#   return(pf)
#   
# }

# plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "TOC")
# plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "DOC")
# plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Sulphate")
# plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Conductivity")
# plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "pH")
# plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Chlorophyll a")
# plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Nitrate")
# plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Organic N")
# plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "UV 254")
# plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Phosphate (ortho)")
# plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Phosphate (total)")
# plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Blue Green Algae")
# plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Green Algae")
# plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Total BG + G")
# plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Calculated TDS")


# fn to plot full time series individually and overlain on same plot
# plot_parms <- function(df1 = "", df2 = "", var = "") {
#   
#   df1 <- df1 %>% filter(parameter == var, date_ymd >= "1990-01-01")
#   df2 <- df2 %>% filter(parameter == var, date_ymd >= "1990-01-01") 
#   
#   # df3 <- df1 %>% mutate(result_df2 = df2$result,
#   #                       source = ifelse((is.na(result) & !is.na(result_df2)), "bp_masterfile",
#   #                                       ifelse((!is.na(result) & is.na(result_df2)), "bp_longterm",
#   #                                              ifelse((is.na(result) & is.na(result_df2)), "missing", "bp_longterm"))),
#   #                       result = ifelse(is.na(result), result_df2, result))
#   
#   p1 <- df1 %>%
#     ggplot(aes(date_ymd, result)) +
#     geom_point(alpha = 1/2, col = "#00BFC4", size = 2) +
#     labs(y = var, title = var, subtitle = "bp_longterm") +
#     theme(axis.title.x = element_blank())
#   
#   p2 <- df2 %>%
#     ggplot(aes(date_ymd, result)) +
#     geom_point(alpha = 1/2, col = "#F8766D", size = 2) +
#     labs(y = var, subtitle = "bp_masterfile") +
#     theme(axis.title.x = element_blank())
#   
#   # p3 <- df3 %>% 
#   #   ggplot(aes(date_ymd, result)) + 
#   #   geom_point(alpha = 1/2, size = 2) +
#   #   labs(y = var, subtitle = "combined") +
#   #   theme(axis.title.x = element_blank())
#   
#   p3 <- df1 %>%
#     ggplot(aes(date_ymd, result)) +
#     geom_point(size = 2, col = "#00BFC4") +
#     geom_point(data = df2, aes(date_ymd, result), alpha = 1/4, size = 2, col = "#F8766D") +
#     labs(y = var, subtitle = "combined") +
#     theme(axis.title.x = element_blank())
#   
#   p123 <- p1 / p2 / p3
#   
#   return(p123)
#   
# }  

# plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "TOC")
# plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "DOC")
# plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Sulphate")
# plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Conductivity")
# plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "pH")
# plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Chlorophyll a")
# plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Nitrate")
# plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Organic N")
# plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "UV 254")
# plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Phosphate (ortho)")
# plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Phosphate (total)")
# plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Blue Green Algae")
# plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Green Algae")
# plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Calculated TDS")
# plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Total BG + G")

# Join: DOC, SO4, cond, pH, Chl a, NO3, organic N, SRP, TP, TDScalc
# Don't join: uv254, algae counts



# Combine datasets --------------------------------------------------------

# fn to combine both data sets in a "raw" form
combine_datasets <- function(df1 = "", df2 = "", var = "") {

  # if (var == "DOC") {
  #   df1 <- df1 %>% filter(parameter == var, datasheet == "RawWater")
  # } else {
  #   df1 <- df1 %>% filter(parameter == var)
  # }

  df1 <- df1 %>% filter(parameter == var) %>% mutate(dataset = "bp_longterm")
  df2 <- df2 %>% filter(parameter == var) %>% mutate(dataset = "bp_masterfile")

  df_combined <- df1 %>%
    mutate(result_combined = ifelse(is.na(result) & !is.na(result), df2$result, result))

  return(df_combined)

}

# combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "DOC")
# combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "Sulphate")
# combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "Conductivity")
# combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "pH")
# combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "Chlorophyll a")
# combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "Nitrate")
# combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "Organic N")
# combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "UV 254")
# combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "Phosphate (ortho)")
# combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "Phosphate (total)")
# combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "Calculated TDS")

# fn to plot combined data set in "raw" form, faceted by year
# missing data are plotted in red to identify gaps
# plot_combined_datasets <- function(df = "", var = "") {
#
#   df <- df %>% filter(parameter == var)
#
#   p_df <- df %>%
#     filter(date_ymd >= "1990-01-01" & date_ymd <= "2019-12-31") %>%
#     mutate(missing = ifelse(is.na(result_combined), "yes", "no"),
#            missing = fct_rev(missing),
#            result_combined = ifelse(is.na(result_combined), 2, result_combined)) %>%
#     ggplot(aes(yday(date_ymd), result_combined, col = missing, shape = dataset)) +
#     facet_wrap(~ year) +
#     geom_point(size = 2) +
#     labs(subtitle = var, y = var) +
#     theme(legend.position = "bottom")
#
#   return(p_df)
#
# }

# plot_combined_datasets(combine_datasets(bp_longterm, bp_masterfile, "TOC"), "TOC")
# plot_combined_datasets(combine_datasets(bp_longterm, bp_masterfile,  "DOC"), "DOC")
# plot_combined_datasets(combine_datasets(bp_longterm, bp_masterfile,  "Sulphate"), "Sulphate")
# plot_combined_datasets(combine_datasets(bp_longterm, bp_masterfile,  "Conductivity"), "Conductivity")
# plot_combined_datasets(combine_datasets(bp_longterm, bp_masterfile,  "pH"), "pH")
# plot_combined_datasets(combine_datasets(bp_longterm, bp_masterfile,  "Chlorophyll a"), "Chlorophyll a")
# plot_combined_datasets(combine_datasets(bp_longterm, bp_masterfile,  "Nitrate"), "Nitrate")
# plot_combined_datasets(combine_datasets(bp_longterm, bp_masterfile,  "Organic N"), "Organic N")
# plot_combined_datasets(combine_datasets(bp_longterm, bp_masterfile,  "Phosphate (ortho)"), "Phosphate (ortho)")
# plot_combined_datasets(combine_datasets(bp_longterm, bp_masterfile,  "Phosphate (total)"), "Phosphate (total)")

bind_all_parms <- function() {

  df <- bp_longterm %>% filter(parameter %in% c("Blue Green Algae", "Green Algae", "Total BG + G"))

  bp_bind <- bind_rows(
    combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "TOC"),
    combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "DOC"),
    combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "Sulphate"),
    combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "Conductivity"),
    combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "pH"),
    combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "Chlorophyll a"),
    combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "Nitrate"),
    combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "Organic N"),
    combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "UV 254"),
    combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "Phosphate (ortho)"),
    combine_datasets(df1 = bp_longterm, df2 = bp_masterfile, var = "Phosphate (total)"),
  ) %>%
    mutate(result = result_combined)

  bp_bind_all <- bind_rows(df, bp_bind)

  bp_bind_clean <- bp_bind_all %>%
    mutate(parameter = ifelse(parameter == "Phosphate (ortho)", "SRP",
                              ifelse(parameter == "Phosphate (total)", "TP",
                                     ifelse(parameter == "UV 254", "UV254", parameter))),
           unit = ifelse(unit == "\xb5S/cm", "µS/cm",
                         ifelse(unit == "pH units", NA,
                                ifelse(unit == "\xb5g/L P", "µg/L P", unit)))) %>%
    filter(!datasheet == "doc_profile") %>%
    select(-result_combined)

  return(bp_bind_clean)

}


