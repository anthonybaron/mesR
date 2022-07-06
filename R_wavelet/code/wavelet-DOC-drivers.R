library(wsyn)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(patchwork)

theme_set(theme_bw(base_size = 12))

# Source scripts for data sets --------------------------------------------

source("./R_data-cleaning/bpwtp/code/clean-bpwtp-DOC.R")
source("./R_data-cleaning/flow-reconstruction/code/clean-flows.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-TP.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-SRP.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-sulphate.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-organic-N.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-nitrate.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-ammonia-N.R")

# Read in data sets -------------------------------------------------------

bp_doc_raw <- bp_DOC_monthly()
bp_flow_raw <- station_flow_monthly()
bp_tp_raw <- bp_TP_monthly()
bp_srp_raw <- bp_SRP_monthly()
bp_so4_raw <- bp_sulphate_monthly()
bp_orgn_raw <- bp_organicN_monthly()
bp_no3_raw <- bp_nitrate_monthly()
bp_nh3_raw <- bp_ammonia_monthly()

bp_flow <- bp_flow_raw %>% 
  select(year = Year, month = Month, SK05JG004_combined_cms, SK05JG006_cms, RC_IC_cms)


# Join data sets ----------------------------------------------------------

bp_drivers <- bp_doc_raw %>% 
  right_join(bp_tp_raw) %>% 
  right_join(bp_srp_raw) %>% 
  right_join(bp_so4_raw) %>% 
  right_join(bp_orgn_raw) %>% 
  right_join(bp_no3_raw) %>% 
  right_join(bp_nh3_raw) %>% 
  right_join(bp_flow) 


# Check autocorrelation in time series ------------------------------------

# acf(bp_drivers$DOC_mg.L)
# acf(bp_drivers$TP_ug.L)
# acf(bp_drivers$SRP_ug.L)
# acf(bp_drivers$SO4_mg.L)
# acf(bp_drivers$orgN_mg.L)
# acf(bp_drivers$NO3_mg.L)
# acf(bp_drivers$NH3_mg.L)
# acf(bp_drivers$SK05JG004_combined_cms)
# acf(bp_drivers$SK05JG006_cms)
# acf(bp_drivers$RC_IC_cms)


# Wavelet analyses --------------------------------------------------------

# Create vectors of each time series
ts_times <- rep(1:length(bp_drivers$date_ymd)) # length of time series (dates)
ts_doc <- bp_drivers$DOC_mg.L
ts_tp <- bp_drivers$TP_ug.L
ts_srp <- bp_drivers$SRP_ug.L
ts_so4 <- bp_drivers$SO4_mg.L
ts_orgn <- bp_drivers$orgN_mg.L
ts_no3 <- bp_drivers$NO3_mg.L
ts_nh3 <- bp_drivers$NH3_mg.L
ts_bp_inflow <- bp_drivers$SK05JG004_combined_cms
ts_dief <- bp_drivers$SK05JG006_cms
ts_rc_ic <- bp_drivers$RC_IC_cms

# Use wsyn::cleandat to apply an optimal Box-Cox normalization procedure 
# (transformation) to each time series, and linearly detrend, de-mean, and 
# standardize variances to 1.
ts_doc_boxcox <- cleandat(dat = ts_doc, times = ts_times, clev = 5)
ts_tp_boxcox <- cleandat(dat = ts_tp, times = ts_times, clev = 5)
ts_srp_boxcox <- cleandat(dat = ts_srp, times = ts_times, clev = 5)
ts_so4_boxcox <- cleandat(dat = ts_so4, times = ts_times, clev = 5)
ts_orgn_boxcox <- cleandat(dat = ts_orgn, times = ts_times, clev = 5)
ts_no3_boxcox <- cleandat(dat = ts_no3, times = ts_times, clev = 5)
ts_nh3_boxcox <- cleandat(dat = ts_nh3, times = ts_times, clev = 5)
ts_bp_inflow_boxcox <- cleandat(dat = ts_bp_inflow, times = ts_times, clev = 5)
ts_dief_boxcox <- cleandat(dat = ts_dief, times = ts_times, clev = 5)
ts_rc_ic_boxcox <- cleandat(dat = ts_rc_ic, times = ts_times, clev = 5)

# Apply wavelet transform using wsyn::wt and plot the magnitude of the transform
# against time and timescale.
wt_doc <- wt(ts_doc_boxcox$cdat, ts_times)
wt_tp <- wt(ts_tp_boxcox$cdat, ts_times)
wt_srp <- wt(ts_srp_boxcox$cdat, ts_times)
wt_so4 <- wt(ts_so4_boxcox$cdat, ts_times)
wt_orgn <- wt(ts_orgn_boxcox$cdat, ts_times)
wt_no3 <- wt(ts_no3_boxcox$cdat, ts_times)
wt_nh3 <- wt(ts_nh3_boxcox$cdat, ts_times)
wt_bp_inflow <- wt(ts_bp_inflow_boxcox$cdat, ts_times)
wt_dief <- wt(ts_dief_boxcox$cdat, ts_times)
wt_rc_ic <- wt(ts_rc_ic_boxcox$cdat, ts_times)

# Then extract the numeric vectors for each cleaned time series to use with
# wavelet coherence analysis.
ts_doc_cleandat <- ts_doc_boxcox$cdat
ts_tp_cleandat <- ts_tp_boxcox$cdat
ts_srp_cleandat <- ts_srp_boxcox$cdat
ts_so4_cleandat <- ts_so4_boxcox$cdat
ts_orgn_cleandat <- ts_orgn_boxcox$cdat
ts_no3_cleandat <- ts_no3_boxcox$cdat
ts_nh3_cleandat <- ts_nh3_boxcox$cdat
ts_bp_inflow_cleandat <- ts_bp_inflow_boxcox$cdat
ts_dief_cleandat <- ts_dief_boxcox$cdat
ts_rc_ic_cleandat <- ts_rc_ic_boxcox$cdat


# Coherence: DOC and drivers ----------------------------------------------

# Evaluate coherence between DOC and drivers using wsyn::coh.
coh_doc_tp <- coh(dat1 = ts_tp_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                  norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                  scale.max.input = 120)
coh_doc_srp <- coh(dat1 = ts_srp_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                   norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                   scale.max.input = 120)
coh_doc_so4 <- coh(dat1 = ts_so4_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                   norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                   scale.max.input = 120)
coh_doc_orgn <- coh(dat1 = ts_orgn_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                    norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                    scale.max.input = 120)
coh_doc_no3 <- coh(dat1 = ts_no3_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                   norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                   scale.max.input = 120)
coh_doc_nh3 <- coh(dat1 = ts_nh3_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                   norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                   scale.max.input = 120)
# coh_doc_bp_inflow <- coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
#                          norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
#                          scale.max.input = 120)
# coh_doc_dief <- coh(dat1 = ts_dief_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
#                     norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
#                     scale.max.input = 120)
# coh_doc_rc_ic <- coh(dat1 = ts_rc_ic_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
#                      norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
#                      scale.max.input = 120)

par(mfrow = c(3, 3))
plotmag(coh_doc_tp, sigthresh = 0.95); title("DOC and total phosphorus")
plotmag(coh_doc_srp, sigthresh = 0.95); title("DOC and soluble reactive phosphorus")
plotmag(coh_doc_so4, sigthresh = 0.95); title("DOC and sulphate")
plotmag(coh_doc_orgn, sigthresh = 0.95); title("DOC and organic nitrogen")
# plotmag(coh_doc_no3, sigthresh = 0.95); title("DOC and nitrate")
plotmag(coh_doc_nh3, sigthresh = 0.95); title("DOC and ammonia")
plotmag(coh_doc_bp_inflow, sigthresh = 0.95); title("DOC and BP inflow")
plotmag(coh_doc_dief, sigthresh = 0.95); title("DOC and Diefenbaker outflow")
plotmag(coh_doc_rc_ic, sigthresh = 0.95); title("DOC and combined Ridge Creek + Iskwao flows")


# Coherence: Analytes and flow --------------------------------------------

# Evaluate coherence between analytes and flow using wsyn::coh.
coh_tp_bp_inflow <- coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_tp_cleandat, times = ts_times, 
                        norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                        scale.max.input = 120)
coh_srp_bp_inflow <- coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_srp_cleandat, times = ts_times, 
                         norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                         scale.max.input = 120)
coh_so4_bp_inflow <- coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_so4_cleandat, times = ts_times, 
                         norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                         scale.max.input = 120)
coh_orgn_bp_inflow <- coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_orgn_cleandat, times = ts_times, 
                         norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                         scale.max.input = 120)
coh_no3_bp_inflow <- coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_no3_cleandat, times = ts_times, 
                         norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                         scale.max.input = 120)
coh_nh3_bp_inflow <- coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_nh3_cleandat, times = ts_times, 
                         norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                         scale.max.input = 120)


coh_tp_dief <- coh(dat1 = ts_dief_cleandat, dat2 = ts_tp_cleandat, times = ts_times, 
                   norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                   scale.max.input = 120)
coh_srp_dief <- coh(dat1 = ts_dief_cleandat, dat2 = ts_srp_cleandat, times = ts_times, 
                    norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                    scale.max.input = 120)
coh_so4_dief <- coh(dat1 = ts_dief_cleandat, dat2 = ts_so4_cleandat, times = ts_times, 
                    norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                    scale.max.input = 120)
coh_orgn_dief <- coh(dat1 = ts_dief_cleandat, dat2 = ts_orgn_cleandat, times = ts_times, 
                     norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                     scale.max.input = 120)
coh_no3_dief <- coh(dat1 = ts_dief_cleandat, dat2 = ts_no3_cleandat, times = ts_times, 
                    norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                    scale.max.input = 120)
coh_nh3_dief <- coh(dat1 = ts_dief_cleandat, dat2 = ts_nh3_cleandat, times = ts_times, 
                    norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                    scale.max.input = 120)

coh_tp_rc_ic <- coh(dat1 = ts_rc_ic_cleandat, dat2 = ts_tp_cleandat, times = ts_times, 
                    norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                    scale.max.input = 120)
coh_srp_rc_ic <- coh(dat1 = ts_rc_ic_cleandat, dat2 = ts_srp_cleandat, times = ts_times, 
                     norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                     scale.max.input = 120)
coh_so4_rc_ic <- coh(dat1 = ts_rc_ic_cleandat, dat2 = ts_so4_cleandat, times = ts_times, 
                     norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                     scale.max.input = 120)
coh_orgn_rc_ic <- coh(dat1 = ts_rc_ic_cleandat, dat2 = ts_orgn_cleandat, times = ts_times, 
                      norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                      scale.max.input = 120)
coh_no3_rc_ic <- coh(dat1 = ts_rc_ic_cleandat, dat2 = ts_no3_cleandat, times = ts_times, 
                     norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                     scale.max.input = 120)
coh_nh3_rc_ic <- coh(dat1 = ts_rc_ic_cleandat, dat2 = ts_nh3_cleandat, times = ts_times, 
                     norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                     scale.max.input = 120)


# TP and flow
par(mfrow = c(1, 3))
plotmag(coh_tp_dief, sigthresh = 0.95); title("TP and Diefenbaker outflow")
plotmag(coh_tp_rc_ic, sigthresh = 0.95); title("TP and combined Ridge Creek + Iskwao flows")
# plotmag(coh_tp_bp_inflow, sigthresh = 0.95); title("TP and BP inflow")

# SRP and flow
par(mfrow = c(1, 3))
plotmag(coh_srp_dief, sigthresh = 0.95); title("SRP and Diefenbaker outflow")
# plotmag(coh_srp_rc_ic, sigthresh = 0.95); title("SRP and combined Ridge Creek + Iskwao flows")
plotmag(coh_srp_bp_inflow, sigthresh = 0.95); title("SRP and BP inflow")

# SO4 and flow
par(mfrow = c(1, 3))
plotmag(coh_so4_dief, sigthresh = 0.95); title("SO4 and Diefenbaker outflow")
plotmag(coh_so4_rc_ic, sigthresh = 0.95); title("SO4 and combined Ridge Creek + Iskwao flows")
plotmag(coh_so4_bp_inflow, sigthresh = 0.95); title("SO4 and BP inflow")

# Organic N and flow
par(mfrow = c(1, 3))
plotmag(coh_orgn_dief, sigthresh = 0.95); title("Organic N and Diefenbaker outflow")
plotmag(coh_orgn_rc_ic, sigthresh = 0.95); title("Organic N and combined Ridge Creek + Iskwao flows")
plotmag(coh_orgn_bp_inflow, sigthresh = 0.95); title("Organic N and BP inflow")

# NO3 and flow
par(mfrow = c(1, 3))
plotmag(coh_no3_dief, sigthresh = 0.95); title("NO3 and Diefenbaker outflow")
plotmag(coh_no3_rc_ic, sigthresh = 0.95); title("NO3 and combined Ridge Creek + Iskwao flows")
# plotmag(coh_no3_bp_inflow, sigthresh = 0.95); title("NO3 and BP inflow")

# NH3 and flow
par(mfrow = c(1, 3))
plotmag(coh_nh3_dief, sigthresh = 0.95); title("NH3 and Diefenbaker outflow")
plotmag(coh_nh3_rc_ic, sigthresh = 0.95); title("NH3 and combined Ridge Creek + Iskwao flows")
plotmag(coh_nh3_bp_inflow, sigthresh = 0.95); title("NH3 and BP inflow")


# Significance testing ----------------------------------------------------

# 1
plotmag(coh_doc_so4, sigthresh = 0.95); title("DOC and sulphate")
coh_doc_so4 <- bandtest(coh_doc_so4, c(8, 120))
coh_doc_so4 <- bandtest(coh_doc_so4, c(2, 3))
plotmag(coh_doc_so4, sigthresh = 0.95)
title("DOC and sulphate\n 2–3 and 8–120 months")

coh_doc_so4$bandp <- NA

# 2
plotmag(coh_doc_tp, sigthresh = 0.95); title("DOC and total phosphorus")
coh_doc_tp <- bandtest(coh_doc_tp, c(2, 60))
plotmag(coh_doc_tp, sigthresh = 0.95)
title("DOC and total phosphorus\n 2–60 months")

coh_doc_tp$bandp <- NA

# 3
plotmag(coh_doc_srp, sigthresh = 0.95); title("DOC and soluble reactive phosphorus")
coh_doc_srp <- bandtest(coh_doc_srp, c(27, 50))
plotmag(coh_doc_srp, sigthresh = 0.95)
title("DOC and soluble reactive phosphorus\n 27–50 months")

coh_doc_srp$bandp <- NA

# 4
plotmag(coh_doc_orgn, sigthresh = 0.95); title("DOC and organic nitrogen")
coh_doc_orgn <- bandtest(coh_doc_orgn, c(2, 120))
plotmag(coh_doc_orgn, sigthresh = 0.95)
title("DOC and organic nitrogen\n 2–120 months")

coh_doc_orgn$bandp <- NA

# 5
plotmag(coh_doc_nh3, sigthresh = 0.95); title("DOC and ammonia")
coh_doc_nh3 <- bandtest(coh_doc_nh3, c(3, 20))
plotmag(coh_doc_nh3, sigthresh = 0.95)
title("DOC and ammonia\n 3–20 months")

coh_doc_nh3$bandp <- NA

# 6 
plotmag(coh_doc_no3, sigthresh = 0.95); title("DOC and nitrate")

coh_doc_no3$bandp <- NA





plotmag(coh_doc_bp_inflow, sigthresh = 0.95); title("DOC and BP inflow")
plotmag(coh_doc_dief, sigthresh = 0.95); title("DOC and Diefenbaker outflow")
plotmag(coh_doc_rc_ic, sigthresh = 0.95); title("DOC and combined Ridge Creek + Iskwao flows")

plotmag(coh_tp_dief, sigthresh = 0.95); title("TP and Diefenbaker outflow")
plotmag(coh_tp_rc_ic, sigthresh = 0.95); title("TP and combined Ridge Creek + Iskwao flows")
# plotmag(coh_tp_bp_inflow, sigthresh = 0.95); title("TP and BP inflow")

plotmag(coh_srp_dief, sigthresh = 0.95); title("SRP and Diefenbaker outflow")
# plotmag(coh_srp_rc_ic, sigthresh = 0.95); title("SRP and combined Ridge Creek + Iskwao flows")
plotmag(coh_srp_bp_inflow, sigthresh = 0.95); title("SRP and BP inflow")

plotmag(coh_so4_dief, sigthresh = 0.95); title("SO4 and Diefenbaker outflow")
plotmag(coh_so4_rc_ic, sigthresh = 0.95); title("SO4 and combined Ridge Creek + Iskwao flows")
plotmag(coh_so4_bp_inflow, sigthresh = 0.95); title("SO4 and BP inflow")

plotmag(coh_orgn_dief, sigthresh = 0.95); title("Organic N and Diefenbaker outflow")
plotmag(coh_orgn_rc_ic, sigthresh = 0.95); title("Organic N and combined Ridge Creek + Iskwao flows")
plotmag(coh_orgn_bp_inflow, sigthresh = 0.95); title("Organic N and BP inflow")

plotmag(coh_no3_dief, sigthresh = 0.95); title("NO3 and Diefenbaker outflow")
plotmag(coh_no3_rc_ic, sigthresh = 0.95); title("NO3 and combined Ridge Creek + Iskwao flows")
# plotmag(coh_no3_bp_inflow, sigthresh = 0.95); title("NO3 and BP inflow")

plotmag(coh_nh3_dief, sigthresh = 0.95); title("NH3 and Diefenbaker outflow")
plotmag(coh_nh3_rc_ic, sigthresh = 0.95); title("NH3 and combined Ridge Creek + Iskwao flows")
plotmag(coh_nh3_bp_inflow, sigthresh = 0.95); title("NH3 and BP inflow")

par(mfrow = c(9, 3))


library(mgcv)
library(gratia)

gam()

# hist(bp_drivers$DOC_mg.L)
hist(log(bp_drivers$DOC_mg.L))
# hist(sqrt(bp_drivers$DOC_mg.L))

# hist(bp_drivers$TP_ug.L)
# hist(log(bp_drivers$TP_ug.L))
hist(sqrt(bp_drivers$TP_ug.L))

# hist(bp_drivers$SO4_mg.L)
hist(log(bp_drivers$SO4_mg.L))
# hist(sqrt(bp_drivers$SO4_mg.L)


bpdocw <- DOC_complete_1990_2019()

bpdocw %>% 
  summarise(median = median(DOC_mg.L),
            min = min(DOC_mg.L),
            max = max(DOC_mg.L))
