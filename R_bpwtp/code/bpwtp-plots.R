library(dplyr)
library(lubridate)
library(readr)
library(tidyr)
library(ggplot2)
library(ggbeeswarm)

source("./R_data-cleaning/bpwtp/code/clean-bpwtp.R")

theme_set(theme_bw(base_size = 12) + theme(panel.grid = element_blank()))

bp_longterm_raw <- bind_all_parms()

bp_longterm <- bp_longterm_raw %>% filter(year %in% c(1990:2019)) 


parm_fig <- function(df = "", var = "") {
  
  df <- df %>% filter(parameter == var, !is.na(result)) 
  
  p_facet_year <- df %>% 
    ggplot(aes(x = yday(date_ymd), y = result)) +
    facet_wrap(~ year) +
    geom_point(col = "steelblue", alpha = 1/2, size = 2) + 
    scale_y_continuous(labels = scales::comma) + 
    labs(x = "Day", y = paste0(df$parameter, " (", df$unit, ")"),
         subtitle = paste(var)) +
    theme_bw(base_size = 12)
    
  return(p_facet_year)
  
}

# parm_fig(bp_longterm, "DOC")
# parm_fig(bp_longterm_clean, "DOC") # looks good
# 
# parm_fig(bp_longterm, "UV 254") 
# parm_fig(bp_longterm_clean, "UV254") # nope
# 
# parm_fig(bp_longterm, "SUVA")
# 
# parm_fig(bp_longterm, "Sulphate")
# parm_fig(bp_longterm_clean, "Sulphate") # nope
# 
# parm_fig(bp_longterm, "Conductivity")
# parm_fig(bp_longterm_clean, "Conductivity") # nope
# 
# parm_fig(bp_longterm, "pH")
# parm_fig(bp_longterm_clean, "pH") # looks ok
# 
# parm_fig(bp_longterm, "Nitrate")
# parm_fig(bp_longterm_clean, "Nitrate") # looks ok, but look into further
# 
# parm_fig(bp_longterm, "Phosphate (ortho)")
# parm_fig(bp_longterm_clean, "SRP") # maybe
# 
# parm_fig(bp_longterm, "Phosphate (total)")
# parm_fig(bp_longterm_clean, "TP") # maybe
# 
# parm_fig(bp_longterm, "Blue Green Algae")
# parm_fig(bp_longterm_clean, "Blue Green Algae") # no change
# 
# parm_fig(bp_longterm, "Green Algae")
# parm_fig(bp_longterm_clean, "Green Algae") # no change
# 
# parm_fig(bp_longterm, "Total BG + G") 
# parm_fig(bp_longterm_clean, "Total BG + G")


ts_fig <- function(df = "", var = "", save_plot = FALSE, outdir = "./R_bpwtp/thesis/outputs/figures") {
  
  df <- df %>% filter(parameter == var) 
  
  p_full_ts <- df %>% 
    filter(!is.na(result)) %>% 
    ggplot(aes(x = date_ymd, y = result)) +
    geom_point(col = "steelblue", alpha = 1/2, size = 2) + 
    # geom_smooth(method = 'gam', col = "black", se = FALSE) +
    scale_y_continuous(labels = scales::comma) + 
    labs(y = paste0(df$parameter, " (", df$unit, ")"),
         subtitle = paste0(var, " full time series (", min(df$year), "-", max(df$year), ")")) +
    theme_bw(base_size = 12) +
    theme(axis.title.x = element_blank()) 
  
  outname <- paste0(Sys.Date(), "_", var, "_full-ts", ".png")
  outpath <- file.path(outdir, outname)
  
  if (isTRUE(save_plot)) {
    ggsave(p_full_ts, filename = outpath, width = 11, height = 6, scale = 1.5, device = "png")
  }
  
  return(p_full_ts)
  
} 

# ts_fig(bp_longterm, "DOC", save_plot = TRUE)
# ts_fig(bp_longterm, "UV 254", save_plot = TRUE) 
# ts_fig(bp_longterm, "SUVA", save_plot = TRUE)
# ts_fig(bp_longterm, "Sulphate", save_plot = TRUE)
# ts_fig(bp_longterm, "Conductivity", save_plot = TRUE)
# ts_fig(bp_longterm, "pH", save_plot = TRUE)
# ts_fig(bp_longterm, "Nitrate", save_plot = TRUE)
# ts_fig(bp_longterm, "Phosphate (ortho)", save_plot = TRUE)
# ts_fig(bp_longterm, "Phosphate (total)", save_plot = TRUE)
# # ts_fig(bp_longterm, "Calculated TDS", save_plot = TRUE)
# ts_fig(bp_longterm, "Blue Green Algae", save_plot = TRUE)
# ts_fig(bp_longterm, "Green Algae", save_plot = TRUE)
# ts_fig(bp_longterm, "Total BG + G", save_plot = TRUE)


count_NAs <- function(df = "", var = "") {
  
  df <- df %>% filter(parameter == var) %>% 
    summarise(df_length = length(result),
              NA_count = sum(is.na(result)),
              NA_percent = NA_count/df_length*100)
  
  return(df)

}

# count_NAs(bp_longterm, "DOC")
# count_NAs(bp_longterm, "UV 254") 
# count_NAs(bp_longterm, "SUVA")
# count_NAs(bp_longterm, "Sulphate")
# count_NAs(bp_longterm, "Conductivity")
# count_NAs(bp_longterm, "pH")
# count_NAs(bp_longterm, "Nitrate")
# count_NAs(bp_longterm, "Phosphate (ortho)")
# count_NAs(bp_longterm, "Phosphate (total)")
# count_NAs(bp_longterm, "TDS") 
# count_NAs(bp_longterm, "Calculated TDS") 
# count_NAs(bp_longterm, "Blue Green Algae")
# count_NAs(bp_longterm, "Green Algae")
# count_NAs(bp_longterm, "Total BG + G")



week_fig <- function(df = "", var = "", save_plot = FALSE, outdir = "./R_bpwtp/thesis/outputs/figures") {
  
  df <- df %>% filter(parameter == var) 
  
  p_weekly <- df %>% 
    ggplot(aes(x = week, y = result)) +
    facet_wrap(~ year, ncol = 5) +
    geom_point(col = "steelblue", alpha = 1/2, size = 3) + 
    scale_x_continuous(breaks = seq(0, 52, by = 4)) +
    scale_y_continuous(labels = scales::comma) + 
    labs(x = "Week", y = paste0(df$parameter, " (", df$unit, ")")) +
    theme_bw(base_size = 12) 
  
  outname <- paste0(Sys.Date(), "_", var, "_weekly-ts", ".png")
  outpath <- file.path(outdir, outname)
  
  if (isTRUE(save_plot)) {
    ggsave(p_weekly, filename = outpath, width = 13, height = 8, scale = 1.5, device = "png")
  }
  
  return(p_weekly)
  
}

# week_fig(bp_longterm, "DOC", save_plot = TRUE)
# week_fig(bp_longterm, "UV 254", save_plot = TRUE) 
# week_fig(bp_longterm, "SUVA", save_plot = TRUE)
# week_fig(bp_longterm, "Sulphate", save_plot = TRUE)
# week_fig(bp_longterm, "Conductivity", save_plot = TRUE)
# week_fig(bp_longterm, "pH", save_plot = TRUE)
# week_fig(bp_longterm, "Nitrate", save_plot = TRUE)
# week_fig(bp_longterm, "Phosphate (ortho)", save_plot = TRUE)
# week_fig(bp_longterm, "Phosphate (total)", save_plot = TRUE)
# week_fig(bp_longterm, "Calculated TDS", save_plot = TRUE) 
# week_fig(bp_longterm, "Blue Green Algae", save_plot = TRUE)
# week_fig(bp_longterm, "Green Algae", save_plot = TRUE)
# week_fig(bp_longterm, "Total BG + G", save_plot = TRUE)



weeks_between_sampling <- function(df = "", var = "", save_plot = FALSE, outdir = "./R_bpwtp/thesis/outputs/figures") { 
  
  df <- df %>% filter(parameter == var, !is.na(result))
    
  df_lag <- df %>%
    slice(1:n() - 1) %>%
    rename(lag_date_ymd = date_ymd) %>%
    add_row(lag_date_ymd = NA, .before = 1) %>%
    select(lag_date_ymd)
  
  df_weeks_bw_sampling <- df %>% 
    mutate(lag_date_ymd = df_lag$lag_date_ymd) %>% 
    select(date_ymd, lag_date_ymd, everything()) %>% 
    mutate(weeks_bw_sampling = as.numeric(difftime(date_ymd, lag_date_ymd, unit = "weeks")))
  
  p_weeks_bw_sampling <- df_weeks_bw_sampling %>% 
    filter(weeks_bw_sampling < 20) %>% 
    ggplot(aes(date_ymd, weeks_bw_sampling)) + 
    geom_quasirandom(groupOnX = FALSE) +
    theme(axis.title.x = element_blank()) +
    labs(y = "Number of weeks", 
         subtitle = paste0("Weeks between sampling ", var, " (1993-2019)"))
  
  outname <- paste0(Sys.Date(), "_", var, "_weeks-bw-sampling", ".png")
  outpath <- file.path(outdir, outname)
  
  if (isTRUE(save_plot)) {
    ggsave(p_weeks_bw_sampling, filename = outpath, width = 9, height = 7, scale = 1.5, device = "png")
  }
    
  return(p_weeks_bw_sampling)
  
} 
  
# weeks_between_sampling(bp_longterm, "DOC", save_plot = TRUE)
# weeks_between_sampling(bp_longterm, "UV 254", save_plot = TRUE) 
# weeks_between_sampling(bp_longterm, "SUVA", save_plot = TRUE)
# weeks_between_sampling(bp_longterm, "Sulphate", save_plot = TRUE)
# weeks_between_sampling(bp_longterm, "Conductivity", save_plot = TRUE)
# weeks_between_sampling(bp_longterm, "pH", save_plot = TRUE)
# weeks_between_sampling(bp_longterm, "Nitrate", save_plot = TRUE)
# weeks_between_sampling(bp_longterm, "Phosphate (ortho)", save_plot = TRUE)
# weeks_between_sampling(bp_longterm, "Phosphate (total)", save_plot = TRUE)
# weeks_between_sampling(bp_longterm, "Blue Green Algae", save_plot = TRUE)
# weeks_between_sampling(bp_longterm, "Green Algae", save_plot = TRUE)
# weeks_between_sampling(bp_longterm, "Total BG + G", save_plot = TRUE)
# 
# weeks_between_sampling(bp_longterm_clean, "DOC")
# weeks_between_sampling(bp_longterm_clean, "UV254") 
# weeks_between_sampling(bp_longterm_clean, "Sulphate")
# weeks_between_sampling(bp_longterm_clean, "Conductivity")
# weeks_between_sampling(bp_longterm_clean, "pH")
# weeks_between_sampling(bp_longterm_clean, "Nitrate")
# weeks_between_sampling(bp_longterm_clean, "Organic N")
# weeks_between_sampling(bp_longterm_clean, "SRP")
# weeks_between_sampling(bp_longterm_clean, "TP")
# weeks_between_sampling(bp_longterm_clean, "Blue Green Algae")
# weeks_between_sampling(bp_longterm_clean, "Green Algae")
# weeks_between_sampling(bp_longterm_clean, "Total BG + G")



