library(wsyn)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(patchwork)

source("./R_data-cleaning/bpwtp/code/clean-bpwtp-TP.R")
source("./R_data-cleaning/flow-reconstruction/code/clean-flows.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-DOC.R")
# source("./R_wavelet/code/wavelet-DOC-flow.R")

station_flow_raw <- station_flow_monthly() # n = 360
bp_tp_raw <- bp_TP_monthly() # n = 360
bp_doc_raw <- bp_DOC_monthly() # n = 360

station_flow <- station_flow_raw
bp_tp <- bp_tp_raw %>% rename(Year = year, Month = month)
bp_doc <- bp_doc_raw %>% rename(Year = year, Month = month)

tp_doc_flow <- right_join(station_flow, bp_tp) %>% 
  right_join(., bp_doc) %>% 
  select(date_ymd:Month, DOC_mg.L, TP_ug.L, everything())

# Wavelet analyses --------------------------------------------------------

# Create vectors of each time series
ts_times <- rep(1:length(tp_doc_flow$TP_ug.L)) # length of time series (dates)
ts_doc <- tp_doc_flow$DOC_mg.L # DOC concentration
ts_tp <- tp_doc_flow$TP_ug.L # SO4 concentration
ts_dief <- tp_doc_flow$SK05JG006_cms # Lake Diefenbaker outflow
ts_ridge <- tp_doc_flow$SK05JG013_cms # Ridge Creek 
ts_iskwao <- tp_doc_flow$SK05JG014_combined_cms # Iskwao Creek
ts_ungauged <- tp_doc_flow$Ungauged_predicted_cms # Ungauged contribution
ts_bp_inflow <- tp_doc_flow$SK05JG004_combined_cms # QPR above BPL
ts_ridge_iskwao <- tp_doc_flow$RC_IC_cms # Ridge Creek + Iskwao Creek

# Use wsyn::cleandat to apply an optimal Box-Cox normalization procedure 
# (transformation) to each time series, and linearly detrend, de-mean, and 
# standardize variances to 1.
ts_tp_boxcox <- cleandat(dat = ts_tp, times = ts_times, clev = 5)
ts_doc_boxcox <- cleandat(dat = ts_doc, times = ts_times, clev = 5)
ts_dief_boxcox <- cleandat(dat = ts_dief, times = ts_times, clev = 5)
ts_ridge_boxcox <- cleandat(dat = ts_ridge, times = ts_times, clev = 5)
ts_iskwao_boxcox <- cleandat(dat = ts_iskwao, times = ts_times, clev = 5)
ts_ungauged_boxcox <- cleandat(dat = ts_ungauged, times = ts_times, clev = 5)
ts_bp_inflow_boxcox <- cleandat(dat = ts_bp_inflow, times = ts_times, clev = 5)
ts_ridge_iskwao_boxcox <- cleandat(dat = ts_ridge_iskwao, times = ts_times, clev = 5)

# Apply wavelet transform using wsyn::wt and plot the magnitude of the transform
# against time and timescale.
wt_tp <- wt(ts_tp_boxcox$cdat, ts_times)
wt_doc <- wt(ts_doc_boxcox$cdat, ts_times)
wt_dief <- wt(ts_dief_boxcox$cdat, ts_times)
wt_ridge <- wt(ts_ridge_boxcox$cdat, ts_times)
wt_iskwao <- wt(ts_iskwao_boxcox$cdat, ts_times)
wt_ungauged <- wt(ts_ungauged_boxcox$cdat, ts_times)
wt_bp_inflow <- wt(ts_bp_inflow_boxcox$cdat, ts_times)
wt_ridge_iskwao <- wt(ts_ridge_iskwao_boxcox$cdat, ts_times)

# plotmag(wt_tp)
# plotmag(wt_doc)
# plotmag(wt_dief)
# plotmag(wt_ridge)
# plotmag(wt_iskwao)
# plotmag(wt_ungauged)
# plotmag(wt_bp_inflow)
# plotmag(wt_ridge_iskwao)

# Then extract the numeric vectors for each cleaned time series to use with
# wavelet coherence analysis.
ts_tp_cleandat <- ts_tp_boxcox$cdat
ts_doc_cleandat <- ts_doc_boxcox$cdat
ts_dief_cleandat <- ts_dief_boxcox$cdat
ts_ridge_cleandat <- ts_ridge_boxcox$cdat
ts_iskwao_cleandat <- ts_iskwao_boxcox$cdat
ts_ungauged_cleandat <- ts_ungauged_boxcox$cdat
ts_bp_inflow_cleandat <- ts_bp_inflow_boxcox$cdat
ts_ridge_iskwao_cleandat <- ts_ridge_iskwao_boxcox$cdat

# Coherence ---------------------------------------------------------------

# Evaluate coherence between tp and flow using wsyn::coh.
coh_dief_tp <- coh(dat1 = ts_dief_cleandat, dat2 = ts_tp_cleandat, times = ts_times, 
                   norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                   scale.max.input = 120)
coh_ridge_tp <- coh(dat1 = ts_ridge_cleandat, dat2 = ts_tp_cleandat, times = ts_times, 
                    norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                    scale.max.input = 120)
coh_iskwao_tp <- coh(dat1 = ts_iskwao_cleandat, dat2 = ts_tp_cleandat, times = ts_times, 
                     norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                     scale.max.input = 120)
coh_ungauged_tp <- coh(dat1 = ts_ungauged_cleandat, dat2 = ts_tp_cleandat, times = ts_times, 
                       norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                       scale.max.input = 120)
coh_bp_inflow_tp <- coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_tp_cleandat, times = ts_times, 
                        norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                        scale.max.input = 120)
coh_ridge_iskwao_tp <- coh(dat1 = ts_ridge_iskwao_cleandat, dat2 = ts_tp_cleandat, times = ts_times, 
                           norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                           scale.max.input = 120)

# Evaluate coherence between DOC and tp using wsyn::coh.
coh_doc_tp <- coh(dat1 = ts_tp_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                  norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                  scale.max.input = 120)


# Plot DOC and tp ---------------------------------------------------

# not coherent at any timescales
plotmag(coh_doc_tp); title("Coherence between DOC and tp")


# Plot tp and Dief --------------------------------------------------

plotmag(coh_dief_tp, sigthresh = 0.95)
title("Total phosphorus and Lake Diefenbaker outflow")
coh_dief_tp <- bandtest(coh_dief_tp, c(27, 50))
plotmag(coh_dief_tp, sigthresh = 0.95)
title("Total phosphorus and Lake Diefenbaker outflow\n 27–50 months")

coh_dief_tp$bandp <- NA

# Plot tp and Ridge -------------------------------------------------

# plotmag(coh_ridge_tp); title("Total phosphorus and Ridge Creek flow")


# Plot tp and Iskwao ------------------------------------------------

# plotmag(coh_iskwao_tp); title("Total phosphorus and Iskwao Creek flow")


# Plot tp and ungauged ----------------------------------------------

# plotmag(coh_ungauged_tp); title("Total phosphorus and ungauged flow")



# Plot tp and Ridge + Iskwao ----------------------------------------

plotmag(coh_ridge_iskwao_tp, sigthresh = 0.95)
title("Total phosphorus and combined Ridge Creek and Iskwao Creek flows")
coh_ridge_iskwao_tp <- bandtest(coh_ridge_iskwao_tp, c(20, 41))
plotmag(coh_ridge_iskwao_tp, sigthresh = 0.95)
title("Total phosphorus and combined Ridge Creek and Iskwao Creek flows\n 20–41 months")

coh_ridge_iskwao_tp$bandp <- NA


# Plot tp and BP inflow ---------------------------------------------

plotmag(coh_bp_inflow_tp, sigthresh = 0.95)
title("Total phosphorus and Buffalo Pound Lake inflow")
coh_bp_inflow_tp <- bandtest(coh_bp_inflow_tp, c(3, 3.5)) # No signifiance
plotmag(coh_bp_inflow_tp, sigthresh = 0.95)
title("Total phosphorus and Buffalo Pound Lake inflow")

coh_bp_inflow_tp$bandp <- NA




# All plots ---------------------------------------------------------------

plotmag(coh_doc_tp); title("Coherence between DOC and tp concentrations")

par(mfrow = c(2, 3))
plotmag(coh_dief_tp); title("Total phosphorus and Lake Diefenbaker outflow")
plotmag(coh_ridge_tp); title("Total phosphorus and Ridge Creek flow")
plotmag(coh_iskwao_tp); title("Total phosphorus and Iskwao Creek flow")
plotmag(coh_ungauged_tp); title("Total phosphorus and ungauged flow")
plotmag(coh_ridge_iskwao_tp); title("Total phosphorus and combined Ridge Creek + Iskwao Creek flows")
plotmag(coh_bp_inflow_tp); title("Total phosphorus and Buffalo Pound inflow")