library(dplyr)
library(readr)
library(tidyr)
library(wsyn)
library(viridis)

# Source scripts for data sets 
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-DOC.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-Chl-a.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-TP.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-SRP.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-sulphate.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-organic-N.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-nitrate.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-ammonia-N.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-UV254.R")
source("./R_data-cleaning/flow-reconstruction/code/clean-flows.R")
#
source("./R_wavelet/code/wavelet-functions.R")


# Time series for wavelet analyses ----------------------------------------

wavelet_data <- function(ts_monthly = FALSE, wtdat = FALSE, cohdat = FALSE) {

  # Monthly time series
  
  # Read in data sets 
  bp_doc_raw <- bp_DOC_monthly()
  bp_chla_raw <- bp_Chla_monthly()
  bp_tp_raw <- bp_TP_monthly()
  bp_srp_raw <- bp_SRP_monthly()
  bp_so4_raw <- bp_sulphate_monthly()
  bp_don_raw <- bp_organicN_monthly() %>% rename(DON_mg.L = orgN_mg.L)
  bp_no3_raw <- bp_nitrate_monthly()
  bp_nh3_raw <- bp_ammonia_monthly() 
  bp_uv254_raw <- bp_UV254_monthly()
  bp_flow_raw <- station_flow_monthly() %>%  
    mutate(nn = 0,
           mm = ifelse(Month < 10, paste0(nn, Month, sep = ""), Month),
           dd = "01",
           date_ymd = paste(Year, mm, dd, sep = "-"),
           date_ymd = ymd(date_ymd)) %>% 
  select(date_ymd, year = Year, month = Month, SK05JG004_combined_cms, 
         SK05JG006_cms, RC_IC_cms) 
  
  # Join data sets 
  bp_drivers <- bp_doc_raw %>% 
    right_join(bp_chla_raw) %>% 
    right_join(bp_tp_raw) %>% 
    right_join(bp_srp_raw) %>% 
    right_join(bp_so4_raw) %>% 
    right_join(bp_don_raw) %>% 
    right_join(bp_no3_raw) %>% 
    right_join(bp_nh3_raw) %>% 
    right_join(bp_uv254_raw) %>% 
    right_join(bp_flow_raw) 

  return(bp_drivers)

}

bp_drivers <- wavelet_data()

# Wavelet transform data --------------------------------------------------

wt_data <- function() {

  # Create vectors of each time series
  ts_times <- rep(1:length(bp_drivers$date_ymd)) # length of time series (dates)
  ts_doc <- bp_drivers$DOC_mg.L
  ts_chla <- bp_drivers$chla_ug.L
  ts_tp <- bp_drivers$TP_ug.L
  ts_srp <- bp_drivers$SRP_ug.L
  ts_so4 <- bp_drivers$SO4_mg.L
  ts_don <- bp_drivers$DON_mg.L
  ts_no3 <- bp_drivers$NO3_mg.L
  ts_nh3 <- bp_drivers$NH3_mg.L
  ts_bp <- bp_drivers$SK05JG004_combined_cms
  ts_dief <- bp_drivers$SK05JG006_cms
  ts_cat <- bp_drivers$RC_IC_cms

  # Use wsyn::cleandat to apply an optimal Box-Cox normalization procedure
  # (transformation) to each time series, and linearly detrend, de-mean, and
  # standardize variances to 1.
  ts_doc_boxcox <- cleandat(dat = ts_doc, times = ts_times, clev = 5)
  ts_chla_boxcox <- cleandat(dat = ts_chla, times = ts_times, clev = 5)
  ts_tp_boxcox <- cleandat(dat = ts_tp, times = ts_times, clev = 5)
  ts_srp_boxcox <- cleandat(dat = ts_srp, times = ts_times, clev = 5)
  ts_so4_boxcox <- cleandat(dat = ts_so4, times = ts_times, clev = 5)
  ts_don_boxcox <- cleandat(dat = ts_don, times = ts_times, clev = 5)
  ts_no3_boxcox <- cleandat(dat = ts_no3, times = ts_times, clev = 5)
  ts_nh3_boxcox <- cleandat(dat = ts_nh3, times = ts_times, clev = 5)
  ts_bp_boxcox <- cleandat(dat = ts_bp, times = ts_times, clev = 5)
  ts_dief_boxcox <- cleandat(dat = ts_dief, times = ts_times, clev = 5)
  ts_cat_boxcox <- cleandat(dat = ts_cat, times = ts_times, clev = 5)

  # Apply wavelet transform using wsyn::wt and plot the magnitude of the
  # transform against time and timescale.
  wt_doc <- wt(ts_doc_boxcox$cdat, ts_times)
  wt_chla <- wt(ts_chla_boxcox$cdat, ts_times)
  wt_tp <- wt(ts_tp_boxcox$cdat, ts_times)
  wt_srp <- wt(ts_srp_boxcox$cdat, ts_times)
  wt_so4 <- wt(ts_so4_boxcox$cdat, ts_times)
  wt_don <- wt(ts_don_boxcox$cdat, ts_times)
  wt_no3 <- wt(ts_no3_boxcox$cdat, ts_times)
  wt_nh3 <- wt(ts_nh3_boxcox$cdat, ts_times)
  wt_bp <- wt(ts_bp_boxcox$cdat, ts_times)
  wt_dief <- wt(ts_dief_boxcox$cdat, ts_times)
  wt_cat <- wt(ts_cat_boxcox$cdat, ts_times)

  wtl <- list(
    wt_doc = wt_doc,
    wt_so4 = wt_so4,
    wt_tp = wt_tp,
    wt_srp = wt_srp,
    wt_chla = wt_chla,
    wt_don = wt_doc,
    wt_no3 = wt_no3,
    wt_nh3 = wt_nh3,
    wt_dief = wt_dief,
    wt_cat = wt_cat,
    wt_bp = wt_bp
  )

  return(wtl)

}

# Coherence data ----------------------------------------------------------

coherence_data <- function() {

  bp_drivers <- wavelet_data()

  # Create vectors of each time series
  ts_times <- rep(1:length(bp_drivers$date_ymd)) # length of time series (dates)
  ts_doc <- bp_drivers$DOC_mg.L
  ts_chla <- bp_drivers$chla_ug.L
  ts_tp <- bp_drivers$TP_ug.L
  ts_srp <- bp_drivers$SRP_ug.L
  ts_so4 <- bp_drivers$SO4_mg.L
  ts_don <- bp_drivers$DON_mg.L
  ts_no3 <- bp_drivers$NO3_mg.L
  ts_nh3 <- bp_drivers$NH3_mg.L
  ts_bp <- bp_drivers$SK05JG004_combined_cms
  ts_dief <- bp_drivers$SK05JG006_cms
  ts_cat <- bp_drivers$RC_IC_cms

  # Use wsyn::cleandat to apply an optimal Box-Cox normalization procedure
  # (transformation) to each time series, and linearly detrend, de-mean, and
  # standardize variances to 1.
  ts_doc_boxcox <- cleandat(dat = ts_doc, times = ts_times, clev = 5)
  ts_chla_boxcox <- cleandat(dat = ts_chla, times = ts_times, clev = 5)
  ts_tp_boxcox <- cleandat(dat = ts_tp, times = ts_times, clev = 5)
  ts_srp_boxcox <- cleandat(dat = ts_srp, times = ts_times, clev = 5)
  ts_so4_boxcox <- cleandat(dat = ts_so4, times = ts_times, clev = 5)
  ts_don_boxcox <- cleandat(dat = ts_don, times = ts_times, clev = 5)
  ts_no3_boxcox <- cleandat(dat = ts_no3, times = ts_times, clev = 5)
  ts_nh3_boxcox <- cleandat(dat = ts_nh3, times = ts_times, clev = 5)
  ts_bp_boxcox <- cleandat(dat = ts_bp, times = ts_times, clev = 5)
  ts_dief_boxcox <- cleandat(dat = ts_dief, times = ts_times, clev = 5)
  ts_cat_boxcox <- cleandat(dat = ts_cat, times = ts_times, clev = 5)

  # Apply wavelet transform using wsyn::wt and plot the magnitude of the
  # transform against time and timescale.
  wt_doc <- wt(ts_doc_boxcox$cdat, ts_times)
  wt_chla <- wt(ts_chla_boxcox$cdat, ts_times)
  wt_tp <- wt(ts_tp_boxcox$cdat, ts_times)
  wt_srp <- wt(ts_srp_boxcox$cdat, ts_times)
  wt_so4 <- wt(ts_so4_boxcox$cdat, ts_times)
  wt_don <- wt(ts_don_boxcox$cdat, ts_times)
  wt_no3 <- wt(ts_no3_boxcox$cdat, ts_times)
  wt_nh3 <- wt(ts_nh3_boxcox$cdat, ts_times)
  wt_bp <- wt(ts_bp_boxcox$cdat, ts_times)
  wt_dief <- wt(ts_dief_boxcox$cdat, ts_times)
  wt_cat <- wt(ts_cat_boxcox$cdat, ts_times)

  # Then extract the numeric vectors for each cleaned time series to use with
  # wavelet coherence analysis.
  ts_doc_cleandat <- ts_doc_boxcox$cdat
  ts_chla_cleandat <- ts_chla_boxcox$cdat
  ts_tp_cleandat <- ts_tp_boxcox$cdat
  ts_srp_cleandat <- ts_srp_boxcox$cdat
  ts_so4_cleandat <- ts_so4_boxcox$cdat
  ts_don_cleandat <- ts_don_boxcox$cdat
  ts_no3_cleandat <- ts_no3_boxcox$cdat
  ts_nh3_cleandat <- ts_nh3_boxcox$cdat
  ts_bp_cleandat <- ts_bp_boxcox$cdat
  ts_dief_cleandat <- ts_dief_boxcox$cdat
  ts_cat_cleandat <- ts_cat_boxcox$cdat

  # Compute coherence using wavelet mean field and coh() function from
  # R_wavelet/code/wavelet-functions.R
  coh_doc_tp <- coh_dr(dat1 = ts_tp_cleandat, dat2 = ts_doc_cleandat, times = ts_times)
  coh_doc_chla <- coh_dr(dat1 = ts_chla_cleandat, dat2 = ts_doc_cleandat, times = ts_times)
  coh_doc_srp <- coh_dr(dat1 = ts_srp_cleandat, dat2 = ts_doc_cleandat, times = ts_times)
  coh_doc_so4 <- coh_dr(dat1 = ts_so4_cleandat, dat2 = ts_doc_cleandat, times = ts_times)
  # coh_doc_don <- coh_dr(dat1 = ts_don_cleandat, dat2 = ts_doc_cleandat, times = ts_times)
  coh_doc_no3 <- coh_dr(dat1 = ts_no3_cleandat, dat2 = ts_doc_cleandat, times = ts_times)
  coh_doc_nh3 <- coh_dr(dat1 = ts_nh3_cleandat, dat2 = ts_doc_cleandat, times = ts_times)
  coh_doc_bp <- coh_dr(dat1 = ts_bp_cleandat, dat2 = ts_doc_cleandat, times = ts_times)
  coh_doc_dief <- coh_dr(dat1 = ts_dief_cleandat, dat2 = ts_doc_cleandat, times = ts_times)
  coh_doc_cat <- coh_dr(dat1 = ts_cat_cleandat, dat2 = ts_doc_cleandat, times = ts_times)

  cohl <- list(
    coh_doc_so4 = coh_doc_so4,
    coh_doc_tp = coh_doc_tp,
    coh_doc_srp = coh_doc_srp,
    coh_doc_chla = coh_doc_chla,
    coh_doc_no3 = coh_doc_no3,
    coh_doc_nh3 = coh_doc_nh3,
    coh_doc_dief = coh_doc_dief,
    coh_doc_cat = coh_doc_cat,
    coh_doc_bp = coh_doc_bp
  )

  return(cohl)

}






# 
# bp_drivers <- bp_drivers %>% 
#   mutate(CNratio = DOC_mg.L / DON_mg.L,
#          CNratio = ifelse(CNratio > 100, NA, CNratio))
# bp_drivers %>% 
#   ggplot(aes(DON_mg.L, DOC_mg.L, col = year)) + 
#   # facet_wrap(~ year, ncol = 10) +
#   scale_color_viridis(option = 'plasma') +
#   geom_point() + 
#   labs(x = 'TON_mg.L')
# bp_drivers %>% 
#   ggplot(aes(yday(date_ymd), CNratio)) + 
#   facet_wrap(~ year, ncol = 10) +
#   geom_line()
# bp_drivers %>% 
#   ggplot(aes(yday(date_ymd), CNratio, group = year, col = year)) + 
#   geom_line() +
#   scale_color_viridis_c(option = 'plasma') + 
#   labs(x = 'Day of year')
# bp_drivers %>% 
#   ggplot(aes(date_ymd, CNratio)) + 
#   geom_line(size = 1, col = 'steelblue3') + 
#   ylim(c(0, 30)) +
#   labs(x = NULL)
# 
# 
# p_cn <- bp_drivers %>% 
#   ggplot(aes(yday(date_ymd), CNratio, group = year, col = year)) + 
#   geom_line() +
#   lims(y = c(0, 25)) +
#   scale_color_viridis_c(option = 'plasma') + 
#   labs(x = 'Day of year', y = "DOC:DON", col = "Year")
# ggsave("./R_bpwtp/outputs/figures/p_CNratio.png", p_cn, width = 8, height = 4)
