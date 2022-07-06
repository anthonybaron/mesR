library(dplyr)
library(lubridate)
library(readr)
library(readxl)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)
library(patchwork)

theme_set(theme_bw(base_size = 12))


# Data prep ---------------------------------------------------------------

bp_longterm_raw <- read_csv("./R_data-cleaning/bpwtp/data/raw/BPWTP_labdat_current.csv")

bp_masterfile_raw <- read_xlsx("./R_data-cleaning/bpwtp/data/raw/MASTERFILEWTPStandardMeasurementsWORKINGApr26_2018.xlsx",
                               sheet = "Sheet1")


bp_longterm <- bp_longterm_raw %>% 
  filter(datetime_ymd.hms >= "1983-01-01", 
         # datetime_ymd.hms <= "2019-12-31",
         station == "Raw",
         parameter %in% c("Sulphate", "Conductivity", "pH", "Chlorophyll a",
                          "Nitrate", "DOC", "UV 254", "Phosphate (ortho)",
                          "Phosphate (total)", "Blue Green Algae", "Green Algae",
                          "Calculated TDS", "SUVA", "Total BG + G", "Organic N")) %>% 
  select(datasheet, datetime_ymd.hms, year, week, parameter, unit, parm_unit, result) %>% 
  mutate(date_ymd = ymd(str_split(datetime_ymd.hms, " ", simplify = TRUE)[, 1]),
         month = month(date_ymd)) %>% 
  select(datasheet, date_ymd, year, month, week:result)


bp_masterfile <- bp_masterfile_raw %>% 
  select(SampleDateTime, year, month, week, cond, ph, tdscalc, sulfate, chla, 
         no3, orgn, rawdoc, uv254doc, orthop, tp, blgrlg, grlg, totgrnbg) %>% 
  filter(SampleDateTime >= "1983-01-01 00:00:00") %>%
  mutate(date_ymd = ymd(str_split(SampleDateTime, " ", simplify = TRUE)[, 1])) %>% 
  rename(Sulphate = sulfate,
         Conductivity = cond,
         pH = ph, 
         `Chlorophyll a` = chla,
         Nitrate = no3,
         DOC = rawdoc,
         `UV 254` = uv254doc,
         `Phosphate (ortho)` = orthop,
         `Phosphate (total)` = tp,
         `Blue Green Algae` = blgrlg,
         `Green Algae` = grlg,
         `Total BG + G` = totgrnbg,
         `Calculated TDS` = tdscalc,
         `Organic N` = orgn) %>% 
  select(date_ymd, year:`Total BG + G`) %>%
  pivot_longer(cols = c(Conductivity:`Total BG + G`), 
               names_to = "parameter", values_to = "result") %>% 
  arrange(parameter)



# Plot differences between datasets ---------------------------------------

plot_facet_year <- function(df1 = "", df2 = "", var = "") {
  
  df1 <- df1 %>% 
    filter(parameter == var) %>% 
    # date_ymd >= "1993-01-01") %>%
    mutate(doy = yday(date_ymd))
  
  df2 <- df2 %>% 
    filter(parameter == var) %>% 
    # date_ymd >= "1993-01-01") %>%
    mutate(doy = yday(date_ymd))
  
  pf <- df1 %>% 
    ggplot(aes(doy, result)) + 
    facet_wrap(~ year) +
    geom_point(size = 4, col = "#00BFC4") +
    geom_point(data = df2, aes(doy, result), size = 2, col = "#F8766D") +
    labs(y = var, 
         title = var,
         subtitle = "blue = bp_longterm, pink = bp_masterfile") +
    theme(axis.title.x = element_blank())
  
  return(pf)
  
}

plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "DOC")
plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Sulphate")
plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Conductivity")
plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "pH")
plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Chlorophyll a")
plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Nitrate")
plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Organic N")
plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "UV 254")
plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Phosphate (ortho)")
plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Phosphate (total)")
plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Blue Green Algae")
plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Green Algae")
plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Total BG + G")
plot_facet_year(df1 = bp_longterm, df2 = bp_masterfile, var = "Calculated TDS")

plot_parms <- function(df1 = "", df2 = "", var = "") {
  
  df1 <- df1 %>% filter(parameter == var, date_ymd >= "1993-01-01")
  df2 <- df2 %>% filter(parameter == var, date_ymd >= "1993-01-01") 
  
  # df3 <- df1 %>% mutate(result_df2 = df2$result,
  #                       source = ifelse((is.na(result) & !is.na(result_df2)), "bp_masterfile",
  #                                       ifelse((!is.na(result) & is.na(result_df2)), "bp_longterm",
  #                                              ifelse((is.na(result) & is.na(result_df2)), "missing", "bp_longterm"))),
  #                       result = ifelse(is.na(result), result_df2, result))
  
  p1 <- df1 %>%
    ggplot(aes(date_ymd, result)) +
    geom_point(alpha = 1/2, col = "#00BFC4", size = 2) +
    labs(y = var, title = var, subtitle = "bp_longterm") +
    theme(axis.title.x = element_blank())
  
  p2 <- df2 %>%
    ggplot(aes(date_ymd, result)) +
    geom_point(alpha = 1/2, col = "#F8766D", size = 2) +
    labs(y = var, subtitle = "bp_masterfile") +
    theme(axis.title.x = element_blank())
  
  # p3 <- df3 %>% 
  #   ggplot(aes(date_ymd, result)) + 
  #   geom_point(alpha = 1/2, size = 2) +
  #   labs(y = var, subtitle = "combined") +
  #   theme(axis.title.x = element_blank())
  
  p3 <- df1 %>%
    ggplot(aes(date_ymd, result)) +
    geom_point(size = 2, col = "#00BFC4") +
    geom_point(data = df2, aes(date_ymd, result), alpha = 1/4, size = 2, col = "#F8766D") +
    labs(y = var, subtitle = "combined") +
    theme(axis.title.x = element_blank())
  
  p123 <- p1 / p2 / p3
  
  return(p123)
  
}

plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "DOC")
plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Sulphate")
plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Conductivity")
plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "pH")
plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Chlorophyll a")
plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Nitrate")
plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Organic N")
plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "UV 254")
plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Phosphate (ortho)")
plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Phosphate (total)")
plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Blue Green Algae")
plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Green Algae")
plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Calculated TDS")
plot_parms(df1 = bp_longterm, df2 = bp_masterfile, var = "Total BG + G")

# Join: DOC, SO4, cond, pH, Chl a, NO3, organic N, SRP, TP, TDScalc
# Don't join: uv254, algae counts

# DOC missing values diagnostics ------------------------------------------

DOC_raw <- bp_longterm_raw %>% 
  filter(grepl("DOC", parameter),
         !parameter %in% c("alum to DOC ratio"),
         !unit %in% c("percent"))

DOC_NA_datasheet <- bp_longterm_raw %>% 
  filter(grepl("DOC", parameter)) %>% 
  filter(is.na(datasheet) & is.na(unit))

View(DOC_NA_datasheet)

unique(DOC_NA_datasheet$unit)
unique(DOC_NA_datasheet$parameter)

unique(DOC_raw$datasheet)
# [1] "RawWater"    "ClearWell"     "doc_profile"

DOC_raw %>% 
  filter(!datasheet == "ClearWell" & !station == "Clearwell" & datetime_ymd.hms >= "1998-05-27" & datetime_ymd.hms <= "2019-12-31") %>% 
  mutate(datasheet = factor(datasheet)) %>% 
  ggplot(aes(datetime_ymd.hms, result, col = datasheet, shape = datasheet)) +
  facet_wrap(~ datasheet + station) +
  geom_point() +
  theme(legend.position = "bottom")

unique(DOC_raw$station)
# [1] "Raw"       "Clearwell" "PreGAC"  "PreFM"   "FM"   "Channel"

DOC_raw %>% 
  filter(datasheet == "doc_profile") %>% 
  filter(station == "Raw" & datetime_ymd.hms >= "1998-05-27" & datetime_ymd.hms <= "2019-12-31") %>%
  ggplot(aes(yday(datetime_ymd.hms), result)) +
  facet_wrap(~ year) +
  # facet_wrap(~ datasheet, ncol = 1) +
  geom_point(col = "black") +
  geom_point(data = DOC_raw %>% filter(datasheet == "RawWater" & datetime_ymd.hms >= "1998-05-27" & datetime_ymd.hms <= "2019-12-31"), 
             aes(yday(datetime_ymd.hms), result), col = "#F8766D", alpha = 1/2) +
  labs(y = "DOC") +
  theme(legend.position = "bottom",
        axis.title.x = element_blank())

#F8766D = pink
#00BFC4 = blue

DOC_raw %>% 
  filter(datasheet == "doc_profile",
         station == "Raw",
         datetime_ymd.hms >= "1998-05-27",
         datetime_ymd.hms <= "2019-12-31", 
         is.na(result)) 
# 2003-01-14 and 2003-01-15

DOC_raw %>% 
  filter(datasheet == "RawWater",
         station == "Raw",
         datetime_ymd.hms >= "2003-01-01",
         datetime_ymd.hms <= "2003-01-31")


unique(DOC_raw$parameter)
# [1] "DOC"                            

unique(DOC_raw$unit)
# [1] "mg/L C"    "mg/L"

DOC_raw %>% 
  mutate(unit = factor(unit)) %>% 
  ggplot(aes(datetime_ymd.hms, result, col = station, shape = station)) +
  facet_wrap(~ unit, nrow = 2) +
  geom_point() +
  theme(legend.position = "bottom")

unique(DOC_raw$parm_unit)
# [1] "DOC.GFdiss_mg.L.C"                 "doc_profile"

DOC_raw %>% 
  mutate(parm_unit = factor(parm_unit)) %>% 
  ggplot(aes(datetime_ymd.hms, result, col = station, shape = station)) +
  facet_wrap(~ parm_unit, nrow = 2) +
  geom_point() +
  theme(legend.position = "bottom")

unique(DOC_raw$parm_eval)
# [1] "measured"   

unique(DOC_raw$parm_tag)
# [1] "traceConstituents"   "docprofile"

DOC_raw %>% 
  mutate(parm_tag = factor(parm_tag)) %>% 
  ggplot(aes(datetime_ymd.hms, result, col = station, shape = station)) +
  facet_wrap(~ parm_tag, nrow = 2) +
  geom_point() +
  theme(legend.position = "bottom")

DOC_raw %>% 
  mutate(parm_tag = factor(parm_tag)) %>% 
  ggplot(aes(datetime_ymd.hms, result, col = station, shape = station)) +
  facet_wrap(~ parm_tag, nrow = 2) +
  geom_point() +
  theme(legend.position = "bottom")


### 2021-11-17
### From what I can tell DOC is often missing in the long-term dataset Megan and
### I put together beginning in 1998 when the treatment plant started doing DOC
### profiles throughout the treatment process (raw water, preFM, FM, clearwell,
### etc.) Missing DOC values in the treatment plant's weekly reporting are often
### reported in the same xlsx file but under a "DOC profiles" sheet. The DOC 
### record in the masterfile Helen sent me appears to be filled in with the 
### raw water DOC values from the DOC profiles sheet, resulting in just two 
### missing DOC values between mid-1998 and 2018. I will infill the missing DOC
### in my file with the DOC in the masterfile.


# Infill DOC data ---------------------------------------------------------

bp_longterm_DOC <- bp_longterm %>% 
  filter(parameter == "DOC", datasheet == "RawWater")
# min date = 1983-01-01
# max date = 2019-12-30
# length = 1922

bp_masterfile_DOC <- bp_masterfile %>% 
  filter(parameter == "DOC")
# min date = 1983-01-01
# max date = 2017-12-25
# length = 1826

p1 <- bp_longterm_DOC %>% 
  ggplot(aes(date_ymd, result)) + 
  geom_point() +
  labs(subtitle = "bp_longterm")

p2 <- bp_masterfile_DOC %>% 
  ggplot(aes(date_ymd, result)) + 
  geom_point() +
  labs(subtitle = "bp_masterfile")

p1 / p2

# Missing values for the full record (starting 1983-01-03):
bp_longterm_DOC %>% filter(is.na(result)) # 363
bp_masterfile_DOC %>% filter(is.na(result)) # 356

# Identify the ~2-year gaps in the 1990s:
bp_longterm_DOC %>% filter(year >= 1990, year <= 1994) # 1991-01-07 to 1993-05-31
bp_masterfile_DOC %>% filter(year >= 1990, year <= 1994) # 1990-12-31 to 1993-05-31

# Missing values for the period prior two the ~2-year gap in 1991-1993.
bp_longterm_DOC_83_91 <- bp_longterm_DOC %>% filter(date_ymd <= "1991-01-07") %>% arrange(date_ymd)
bp_masterfile_DOC_83_91 <- bp_masterfile_DOC %>% filter(date_ymd <= "1991-01-07") %>% arrange(date_ymd)

bp_longterm_DOC_83_91 %>% filter(is.na(result)) # 162 NAs
bp_masterfile_DOC_83_91 %>% filter(is.na(result)) # 163 NAs

p1 <- bp_longterm_DOC_83_91 %>% 
  ggplot(aes(date_ymd, result)) + 
  geom_point() +
  labs(subtitle = "bp_longterm")

p2 <- bp_masterfile_DOC_83_91 %>% 
  ggplot(aes(date_ymd, result)) + 
  geom_point() +
  labs(subtitle = "bp_masterfile")

p1 / p2


combined_DOC_83_91 <- bp_longterm_DOC_83_91 %>% 
  mutate(result_combined = ifelse(is.na(result), bp_masterfile_DOC_83_91$result, result))

combined_DOC_83_91 %>% 
  ggplot(aes(date_ymd, result)) + 
  geom_point() +
  labs(subtitle = "combined")

combined_DOC_83_91 %>% filter(is.na(result_combined)) # down from 162 NAs to 142


# Missing values for the period after the gap in early 1990s. 
bp_longterm_DOC_93_17 <- bp_longterm_DOC %>% filter(date_ymd >= "1993-05-31") %>% arrange(date_ymd)
bp_masterfile_DOC_93_17 <- bp_masterfile_DOC %>% filter(date_ymd >= "1993-05-31") %>% arrange(date_ymd)

bp_longterm_DOC_93_17 %>% filter(is.na(result), year <= 2017) # 77 NAs
bp_masterfile_DOC_93_17 %>% filter(is.na(result)) # 69 NAs

p1 <- bp_longterm_DOC_93_17 %>% 
  ggplot(aes(date_ymd, result)) + 
  geom_point() +
  labs(subtitle = "bp_longterm")

p2 <- bp_masterfile_DOC_93_17 %>% 
  ggplot(aes(date_ymd, result)) + 
  geom_point() +
  labs(subtitle = "bp_masterfile")

p1 / p2

combined_DOC_93_17 <- bp_longterm_DOC_93_17 %>% 
  mutate(result_combined = ifelse(is.na(result), bp_masterfile_DOC_93_17$result, result))

combined_DOC_93_17 %>% 
  ggplot(aes(date_ymd, result)) + 
  geom_point() +
  labs(subtitle = "combined")

combined_DOC_93_17 %>% filter(is.na(result_combined)) # down from 77 NAs to 21

combined_DOC_93_17 %>% 
  mutate(missing = ifelse(is.na(result_combined), "yes", "no"),
         missing = fct_rev(missing),
         result_combined = ifelse(is.na(result_combined), 3, result_combined)) %>% 
  ggplot(aes(yday(date_ymd), result_combined, col = missing)) + 
  facet_wrap(~ year) +
  geom_point() +
  labs(subtitle = "combined") +
  theme(legend.position = "none")



combined_DOC <- bp_longterm_DOC %>% 
  mutate(result_combined = ifelse(is.na(result), bp_masterfile_DOC$result, result))

combined_DOC %>% filter(is.na(result_combined)) # 280 NAs

combined_DOC %>% 
  mutate(missing = ifelse(is.na(result_combined), "yes", "no"),
         missing = fct_rev(missing),
         result_combined = ifelse(is.na(result_combined), 2, result_combined)) %>% 
  ggplot(aes(yday(date_ymd), result_combined, col = missing)) + 
  facet_wrap(~ year) +
  geom_point() +
  labs(subtitle = "combined DOC", y = "DOC conc (mg/L)", x = "Day") +
  theme(legend.position = "none")

combined_DOC %>% filter(year >= 1994, is.na(result_combined))



combine_datasets <- function(df1 = "", df2 = "", var = "") {
  
  # if (var == "DOC") {
  #   df1 <- df1 %>% filter(parameter == var, datasheet == "RawWater")
  # } else {
  #   df1 <- df1 %>% filter(parameter == var)
  # }
  
  df1 <- df1 %>% filter(parameter == var)
  df2 <- df2 %>% filter(parameter == var)
  
  df_combined <- df1 %>% 
    mutate(result_combined = ifelse(is.na(result), df2$result, result))
  
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

plot_combined_datasets <- function(df = "") {
  
  df <- df %>% filter(parameter == var)
  
  p_df <- df %>% 
    mutate(missing = ifelse(is.na(result_combined), "yes", "no"),
           missing = fct_rev(missing),
           result_combined = ifelse(is.na(result_combined), 2, result_combined)) %>% 
    ggplot(aes(yday(date_ymd), result_combined, col = missing)) + 
    facet_wrap(~ year) +
    geom_point() +
    labs(subtitle = var, y = var) +
    theme(legend.position = "none")
  
  return(p_df)
  
}

# plot_combined_datasets(combine_datasets(bp_longterm, bp_masterfile,  "DOC"), "DOC")
# plot_combined_datasets(combine_datasets(bp_longterm, bp_masterfile,  "Sulphate"), "Sulphate")
# plot_combined_datasets(combine_datasets(bp_longterm, bp_masterfile,  "Conductivity"), "Conductivity")
# plot_combined_datasets(combine_datasets(bp_longterm, bp_masterfile,  "pH"), "pH")
# plot_combined_datasets(combine_datasets(bp_longterm, bp_masterfile,  "Chlorophyll a"), "Chlorophyll a")
# plot_combined_datasets(combine_datasets(bp_longterm, bp_masterfile,  "Nitrate"), "Nitrate")
# plot_combined_datasets(combine_datasets(bp_longterm, bp_masterfile,  "Organic N"), "Organic N")
# plot_combined_datasets(combine_datasets(bp_longterm, bp_masterfile,  "Phosphate (ortho)"), "Phosphate (ortho)")
# plot_combined_datasets(combine_datasets(bp_longterm, bp_masterfile,  "Phosphate (total)"), "Phosphate (total)")

bind_all_parms <- function(df = "") {
  
  df <- df %>% filter(parameter %in% c("Blue Green Algae", "Green Algae", "Tot BG + G"))
  
  bp_bind <- bind_rows(
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
  )
  
  bp_bind_all <- bind_rows(df, bp_bind)
  
  bp_bind_clean <- bp_bind_all %>%
    mutate(parameter = ifelse(parameter == "Phosphate (ortho)", "SRP",
                              ifelse(parameter == "Phosphate (total)", "TP",
                                     ifelse(parameter == "UV 254", "UV254", parameter))),
           unit = ifelse(unit == "\xb5S/cm", "µS/cm",
                         ifelse(unit == "pH units", NA, 
                                ifelse(unit == "\xb5g/L P", "µg/L P", unit))),
           result_orig = result,
           result = result_combined)
  
  return(bp_bind_clean)
  
}

bplc <- bind_all_parms(bp_longterm)

bplc %>% 
  filter(parameter == "Blue Green Algae") %>% 
  ggplot(aes(yday(date_ymd), result)) +
  facet_wrap(~ year) + 
  geom_point()
