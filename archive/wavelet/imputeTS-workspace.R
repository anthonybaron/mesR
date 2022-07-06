library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(forcats)
library(imputeTS)
library(patchwork)

theme_set(theme_bw(base_size = 12))

DOC_lab <- expression(paste("DOC (mg L"^-1*")"))

BP_longterm_raw <- read_csv("./R_data-cleaning/bpwtp/data/raw/BPWTP_labdat_current.csv") 

BP_longterm <- BP_longterm_raw %>% 
  filter(datasheet == "RawWater" & grepl("DOC", parm_unit)) %>%
  separate(datetime_ymd.hms, into = c("date_ymd", "time"), sep = " ") %>% 
  select(-c(datasheet, station, sheet_year, parameter, unit, parm_eval, 
            result_org, result_flag, time)) %>% 
  mutate(parm_unit = ifelse(parm_unit == "DOC.GFdiss_mg.L.C", "DOC_mg.L", parm_unit),
         date_ymd = as.Date(date_ymd),
         DOY = yday(date_ymd)) %>% 
  filter(date_ymd >= "1994-01-01" & date_ymd <= "2019-12-31") %>% 
  select(date_ymd, result) %>% 
  mutate(date_ymd = as.character(date_ymd)) %>% 
  add_row(date_ymd = "1996-12-30", result = NA) %>%
  add_row(date_ymd = "2001-12-31", result = NA) %>% 
  add_row(date_ymd = "2002-12-30", result = NA) %>% 
  add_row(date_ymd = "2018-01-01", result = NA) %>% 
  add_row(date_ymd = "2019-01-07", result = NA) %>% 
  arrange(date_ymd) %>% 
  mutate(date_ymd = ymd(date_ymd),
         obs_num = row_number()) %>% 
  select(obs_num, date_ymd, result) 


statsNA(BP_longterm$result)

ggplot_na_distribution(BP_longterm$result)

ggplot_na_intervals(BP_longterm$result, number_intervals = 26, measure = "count")

ggplot_na_gapsize(BP_longterm$result)


# na_mean() ---------------------------------------------------------------

bp_na_mean <- na_mean(BP_longterm$result)

bp_na_mean_cc <- BP_longterm %>% 
  mutate(result_cc = bp_na_mean,
         result_imp = ifelse(is.na(result), bp_na_mean, NA),
         Method = ifelse(is.na(result), "Mean value", "Observed"))

p_mean <- bp_na_mean_cc %>% 
  ggplot(aes(date_ymd, result, colour = Method)) + 
  geom_point(alpha = 1/3) +
  geom_point(data = bp_na_mean_cc, aes(date_ymd, result_imp, colour = Method)) + 
  labs(y = DOC_lab, subtitle = "Overall mean") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())


# na_interpolation() ------------------------------------------------------

# Linear 
bp_na_linear <- na_interpolation(BP_longterm$result, option = "linear")

bp_na_linear_cc <- BP_longterm %>% 
  mutate(result_cc = bp_na_linear,
         result_imp = ifelse(is.na(result), bp_na_linear, NA),
         Method = ifelse(is.na(result), "Linear interpolation", "Observed"))

p_linear <- bp_na_linear_cc %>% 
  ggplot(aes(date_ymd, result_cc, colour = Method)) + 
  geom_point(alpha = 1/3) +
  geom_point(data = bp_na_linear_cc, aes(date_ymd, result_imp, colour = Method)) +
  labs(y = DOC_lab, subtitle = "Linear interpolation") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())


# Spline
bp_na_spline <- na_interpolation(BP_longterm$result, option = "spline")

bp_na_spline_cc <- BP_longterm %>% 
  mutate(result_cc = bp_na_spline,
         Method = ifelse(is.na(result), "Spline interpolation", "Observed"),
         result_imp = ifelse(is.na(result), bp_na_spline, NA),
         Method = fct_rev(Method),
         doy = yday(date_ymd),
         year = year(date_ymd))

bp_na_spline_2001_2004 <- bp_na_spline_cc %>% 
  filter(year %in% c(2001:2004))

p_spline <- bp_na_spline_cc %>% 
  ggplot(aes(date_ymd, result_cc, colour = Method)) + 
  geom_point(alpha = 1/3) +
  geom_point(data = bp_na_spline_cc, aes(date_ymd, result_imp, colour = Method)) +
  labs(y = DOC_lab, subtitle = "Spline interpolation") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())

bp_na_spline_2001_2004 %>% 
  ggplot(aes(doy, result_cc, colour = Method)) + 
  facet_wrap(~ year, nrow = 1) + 
  geom_point(alpha = 1/3) +
  geom_point(data = bp_na_spline_2001_2004, aes(doy, result_imp, colour = Method)) +
  labs(y = DOC_lab, subtitle = "Spline interpolation") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())


# Stine
bp_na_stine <- na_interpolation(BP_longterm$result, option = "stine")

bp_na_stine_cc <- BP_longterm %>% 
  mutate(result_cc = bp_na_stine,
         Method = ifelse(is.na(result), "Stine interpolation", "Observed"),
         result_imp = ifelse(is.na(result), bp_na_stine, NA),
         Method = fct_rev(Method))

p_stine <- bp_na_stine_cc %>% 
  ggplot(aes(date_ymd, result_cc, colour = Method)) + 
  geom_point(alpha = 1/3) +
  geom_point(data = bp_na_stine_cc, aes(date_ymd, result_imp, colour = Method)) +
  labs(y = DOC_lab, subtitle = "Stine interpolation") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())



# na_kalman() -------------------------------------------------------------

# StructTS (structural model fitted by maximum likelihood)
bp_na_kalman <- na_kalman(BP_longterm$result, model = "StructTS", smooth = TRUE, nit = -1)

bp_na_kalman_cc <- BP_longterm %>% 
  mutate(result_cc = bp_na_kalman,
         result_imp = ifelse(is.na(result), bp_na_kalman, NA),
         Method = ifelse(is.na(result), "Kalman smoothing", "Observed"))

p_kalman <- bp_na_kalman_cc %>% 
  ggplot(aes(date_ymd, result_cc, colour = Method)) + 
  geom_point(alpha = 1/3) +
  geom_point(data = bp_na_kalman_cc, aes(date_ymd, result_imp, colour = Method)) +
  labs(y = DOC_lab, subtitle = "Kalman smoothing") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())


# auto.arima (state space representation of ARIMA model)
bp_na_arima <- na_kalman(BP_longterm$result, model = "auto.arima", smooth = TRUE, nit = -1)

bp_na_arima_cc <- BP_longterm %>% 
  mutate(result_cc = bp_na_arima,
         result_imp = ifelse(is.na(result), bp_na_arima, NA),
         Method = ifelse(is.na(result), "ARIMA", "Observed"))

p_arima <- bp_na_arima_cc %>% 
  ggplot(aes(date_ymd, result_cc, colour = Method)) + 
  geom_point(alpha = 1/3) +
  geom_point(data = bp_na_arima_cc, aes(date_ymd, result_imp, colour = Method)) +
  labs(y = DOC_lab, subtitle = "ARIMA model") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())



# na_seadec() -------------------------------------------------------------

# Seasonally decomposed missing value imputation

# algorith = "interpolation"
bp_na_seadec_interpolation <- na_seadec(BP_longterm$result, algorithm = "interpolation", find_frequency = TRUE)
  
bp_na_seadec_interpolation_cc <- BP_longterm %>% 
  mutate(result_cc = bp_na_seadec_interpolation,
         result_imp = ifelse(is.na(result), bp_na_seadec_interpolation, NA),
         Method = ifelse(is.na(result), "Interpolation with seasonal decomposition", "Observed"),
         day_of_year = yday(date_ymd),
         year = year(date_ymd))

p_seadec_interpolation <- bp_na_seadec_interpolation_cc %>% 
  ggplot(aes(date_ymd, result_cc, colour = Method)) + 
  geom_point(alpha = 1/3) +
  geom_point(data = bp_na_seadec_interpolation_cc, aes(date_ymd, result_imp, colour = Method)) +
  labs(y = DOC_lab, subtitle = "Interpolation with seasonal decomposition") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())

bp_na_seadec_interpolation_cc %>% 
  ggplot(aes(day_of_year, result, colour = Method)) + 
  facet_wrap(~ year) + 
  geom_point(alpha = 1/3) +
  geom_point(data = bp_na_seadec_interpolation_cc, aes(day_of_year, result_imp, colour = Method)) +
  labs(y = DOC_lab, subtitle = "Interpolation with seasonal decomposition") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())

bp_na_seadec_interpolation_2001_2004 <- bp_na_seadec_interpolation_cc %>% 
  filter(year %in% c(2001:2004))

bp_na_seadec_interpolation_2001_2004 %>% 
  ggplot(aes(day_of_year, result, colour = Method)) + 
  facet_wrap(~ year, nrow = 1) + 
  geom_point(alpha = 1/2, size = 2) +
  geom_point(data = bp_na_seadec_interpolation_2001_2004, aes(day_of_year, result_imp, colour = Method), size = 2) +
  labs(y = DOC_lab, subtitle = "Interpolation with seasonal decomposition") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())


# algorith = "ma" (weighted moving average)
bp_na_seadec_ma <- na_seadec(BP_longterm$result, algorithm = "ma", find_frequency = TRUE)

bp_na_seadec_ma_cc <- BP_longterm %>% 
  mutate(result_cc = bp_na_seadec_ma,
         result_imp = ifelse(is.na(result), bp_na_seadec_ma, NA),
         Method = ifelse(is.na(result), "Weighted moving average with seasonal decomposition", "Observed"),
         Method = fct_rev(Method),
         day_of_year = yday(date_ymd),
         year = year(date_ymd))

p_seadec_ma <- bp_na_seadec_ma_cc %>% 
  ggplot(aes(date_ymd, result_cc, colour = Method)) + 
  geom_point(alpha = 1/3) +
  geom_point(data = bp_na_seadec_ma_cc, aes(date_ymd, result_imp, colour = Method)) +
  labs(y = DOC_lab, subtitle = "Weighted moving average with seasonal decomposition") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())

bp_na_seadec_ma_cc %>% 
  ggplot(aes(day_of_year, result, colour = Method)) + 
  facet_wrap(~ year) + 
  geom_point(alpha = 1/3) +
  geom_point(data = bp_na_seadec_ma_cc, aes(day_of_year, result_imp, colour = Method)) +
  labs(y = DOC_lab, subtitle = "Weighted moving average with seasonal decomposition") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())

bp_na_seadec_ma_2001_2004 <- bp_na_seadec_ma_cc %>% 
  filter(year %in% c(2001:2004))

bp_na_seadec_ma_2001_2004 %>% 
  ggplot(aes(day_of_year, result, colour = Method)) + 
  facet_wrap(~ year, nrow = 1) + 
  geom_point(alpha = 1/2, size = 2) +
  geom_point(data = bp_na_seadec_ma_2001_2004, aes(day_of_year, result_imp, colour = Method), size = 2) +
  labs(y = DOC_lab, subtitle = "Weighted moving average with seasonal decomposition") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())



# na_ma() -----------------------------------------------------------------

# weighting = "simple"
bp_na_ma_simple <- na_ma(BP_longterm$result, k = 4, weighting = "simple")

bp_na_ma_simple_cc <- BP_longterm %>% 
  mutate(result_cc = bp_na_ma_simple,
         result_imp = ifelse(is.na(result), bp_na_ma_simple, NA),
         Method = ifelse(is.na(result), "Simple weighted moving average", "Observed"),
         Method = fct_rev(Method),
         day_of_year = yday(date_ymd),
         year = year(date_ymd))

p_na_ma_simple <- bp_na_ma_simple_cc %>% 
  ggplot(aes(date_ymd, result_cc, colour = Method)) + 
  geom_point(alpha = 1/3) +
  geom_point(data = bp_na_ma_simple_cc, aes(date_ymd, result_imp, colour = Method)) +
  labs(y = DOC_lab, subtitle = "Simple weighted moving average") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())

bp_na_ma_simple_cc %>% 
  ggplot(aes(day_of_year, result, colour = Method)) + 
  facet_wrap(~ year) + 
  geom_point(alpha = 1/3) +
  geom_point(data = bp_na_ma_simple_cc, aes(day_of_year, result_imp, colour = Method)) +
  labs(y = DOC_lab, subtitle = "Simple weighted moving average") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())

bp_na_ma_simple_2001_2004 <- bp_na_ma_simple_cc %>% 
  filter(year %in% c(2001:2004))

bp_na_ma_simple_2001_2004 %>% 
  ggplot(aes(day_of_year, result, colour = Method)) + 
  facet_wrap(~ year, nrow = 1) + 
  geom_point(alpha = 1/2, size = 2) +
  geom_point(data = bp_na_ma_simple_2001_2004, aes(day_of_year, result_imp, colour = Method), size = 2) +
  labs(y = DOC_lab, subtitle = "Simple weighted moving average") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())


# weighting = "linear"
bp_na_ma_linear <- na_ma(BP_longterm$result, k = 4, weighting = "linear")

bp_na_ma_linear_cc <- BP_longterm %>% 
  mutate(result_cc = bp_na_ma_linear,
         result_imp = ifelse(is.na(result), bp_na_ma_linear, NA),
         Method = ifelse(is.na(result), "Linear weighted moving average", "Observed"),
         day_of_year = yday(date_ymd),
         year = year(date_ymd))

p_na_ma_linear <- bp_na_ma_linear_cc %>% 
  ggplot(aes(date_ymd, result_cc, colour = Method)) + 
  geom_point(alpha = 1/3) +
  geom_point(data = bp_na_ma_linear_cc, aes(date_ymd, result_imp, colour = Method)) +
  labs(y = DOC_lab, subtitle = "Linear weighted moving average") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())

bp_na_ma_linear_cc %>% 
  ggplot(aes(day_of_year, result, colour = Method)) + 
  facet_wrap(~ year) + 
  geom_point(alpha = 1/3) +
  geom_point(data = bp_na_ma_linear_cc, aes(day_of_year, result_imp, colour = Method)) +
  labs(y = DOC_lab, subtitle = "Linear weighted moving average") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())

bp_na_ma_linear_2001_2004 <- bp_na_ma_linear_cc %>% 
  filter(year %in% c(2001:2004))

bp_na_ma_linear_2001_2004 %>% 
  ggplot(aes(day_of_year, result, colour = Method)) + 
  facet_wrap(~ year, nrow = 1) + 
  geom_point(alpha = 1/2, size = 2) +
  geom_point(data = bp_na_ma_linear_2001_2004, aes(day_of_year, result_imp, colour = Method), size = 2) +
  labs(y = DOC_lab, subtitle = "Linear weighted moving average") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())



# weighting = "exponential"
bp_na_ma_exponential <- na_ma(BP_longterm$result, k = 4, weighting = "exponential")

bp_na_ma_exponential_cc <- BP_longterm %>% 
  mutate(result_cc = bp_na_ma_exponential,
         result_imp = ifelse(is.na(result), bp_na_ma_exponential, NA),
         Method = ifelse(is.na(result), "Exponential weighted moving average", "Observed"),
         day_of_year = yday(date_ymd),
         year = year(date_ymd))

p_na_ma_exponential <- bp_na_ma_exponential_cc %>% 
  ggplot(aes(date_ymd, result_cc, colour = Method)) + 
  geom_point(alpha = 1/3) +
  geom_point(data = bp_na_ma_exponential_cc, aes(date_ymd, result_imp, colour = Method)) +
  labs(y = DOC_lab, subtitle = "Exponential weighted moving average") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())

bp_na_ma_exponential_cc %>% 
  ggplot(aes(day_of_year, result, colour = Method)) + 
  facet_wrap(~ year) + 
  geom_point(alpha = 1/3) +
  geom_point(data = bp_na_ma_exponential_cc, aes(day_of_year, result_imp, colour = Method)) +
  labs(y = DOC_lab, subtitle = "Exponential weighted moving average") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())

bp_na_ma_exponential_2001_2004 <- bp_na_ma_exponential_cc %>% 
  filter(year %in% c(2001:2004))

bp_na_ma_exponential_2001_2004 %>% 
  ggplot(aes(day_of_year, result, colour = Method)) + 
  facet_wrap(~ year, nrow = 1) + 
  geom_point(alpha = 1/2, size = 2) +
  geom_point(data = bp_na_ma_exponential_2001_2004, aes(day_of_year, result_imp, colour = Method), size = 2) +
  labs(y = DOC_lab, subtitle = "Exponential weighted moving average") + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom",
        legend.title = element_blank())



# Visualize imputations ---------------------------------------------------

p_mean
p_linear <- p_linear + theme(axis.title.y = element_blank())
p_spline <- p_spline + theme(axis.title.y = element_blank())

p_stine
p_kalman <- p_kalman + theme(axis.title.y = element_blank())
p_arima <- p_arima + theme(axis.title.y = element_blank())


p_imputeTS <- (p_mean + p_linear + p_spline) / (p_stine + p_kalman + p_arima) +
  plot_annotation(title = "Imputation using imputeTS R package") 

ggsave("./R_wavelet/outputs/figures/p_imputeTS_2021-11-15.png", p_imputeTS, width = 13, height = 6)


p_spline / p_seadec_interpolation








year <- 2001

year_start <- year - 2
year_end <- year + 2

BP_longterm %>% 
  mutate(year = year(date_ymd)) %>% 
  filter(year %in% c(year_start:year_end))


tmp <- function(df = "", year = "") {
  
  year_start <- year - 2
  year_end <- year + 2
  year_range <- c(year_start:year_end)
  
  df <- df %>% 
    mutate(year = year(date_ymd)) %>% 
    filter(year %in% c(year_range) & !is.na(result))
  
  result_mean <- mean(df$result, na.rm = TRUE)
  result_sd <- sd(df$result, na.rm = TRUE)
  
  result_rnorm <- rnorm(n = length(df$result), mean = result_mean, sd = result_sd)
  
  return(result_rnorm)
  
}



tmp(df = BP_longterm, year = 2001)



bp_tmp <- BP_longterm %>% 
  mutate(year = year(date_ymd)) %>% 
  filter(year %in% c(1999:2003))

bp_tmp_cc <- bp_tmp %>% filter(!is.na(result))

tmp_mean <- mean(bp_tmp$result, na.rm = TRUE)
tmp_sd <- sd(bp_tmp$result, na.rm = TRUE)

tmp_rnorm <- rnorm(n = length(bp_tmp$result), mean = tmp_mean, sd = tmp_sd)

bp_tmp %>% 
  mutate(result_cc = ifelse(is.na(result), sample(tmp_rnorm, replace = TRUE), result),
         Method = ifelse(is.na(result), "rnorm()", "Observed")) %>% 
  ggplot(aes(date_ymd, result_cc, colour = Method)) +
  geom_point()


bp_tmp_cc %>% 
  ggplot(aes(result)) + 
  geom_histogram()





library(boot)

boot(data = bp_tmp$result, statistic = mean, R = 999)
