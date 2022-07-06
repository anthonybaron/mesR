library(wsyn)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(patchwork)

source("./R_data-cleaning/bpwtp/code/clean-bpwtp-sulphate.R")
source("./R_data-cleaning/flow-reconstruction/code/clean-flows.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-DOC.R")
# source("./R_wavelet/code/wavelet-DOC-flow.R")


station_flow_raw <- station_flow_monthly()
bp_sulphate_raw <- bp_sulphate_monthly()
bp_doc_raw <- DOC_complete_1990_2019()


# Prepare data sets -------------------------------------------------------

station_flow <- station_flow_raw

bp_sulphate <- bp_sulphate_raw %>% select(Year = year, Month = month, SO4_mg.L)

bp_doc <- bp_doc_raw %>% 
  select(Year = year, Month = month, Week = week, DOC_mg.L) %>% 
  group_by(Year, Month) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE)) %>% 
  ungroup() 



sulphate_doc_flow <- right_join(station_flow, bp_sulphate) %>% 
  right_join(., bp_doc) %>% 
  select(date_ymd:Month, DOC_mg.L, SO4_mg.L, everything())



# Wavelet analyses --------------------------------------------------------

# Create vectors of each time series
ts_times <- rep(1:length(sulphate_doc_flow$SO4_mg.L)) # length of time series (dates)
ts_doc <- sulphate_doc_flow$DOC_mg.L # DOC concentration
ts_sulphate <- sulphate_doc_flow$SO4_mg.L # SO4 concentration
ts_dief <- sulphate_doc_flow$SK05JG006_cms # Lake Diefenbaker outflow
ts_ridge <- sulphate_doc_flow$SK05JG013_cms # Ridge Creek 
ts_iskwao <- sulphate_doc_flow$SK05JG014_combined_cms # Iskwao Creek
ts_ungauged <- sulphate_doc_flow$Ungauged_predicted_cms # Ungauged contribution
ts_bp_inflow <- sulphate_doc_flow$SK05JG004_combined_cms # QPR above BPL
ts_ridge_iskwao <- sulphate_doc_flow$RC_IC_cms # Ridge Creek + Iskwao Creek

# Use wsyn::cleandat to apply an optimal Box-Cox normalization procedure 
# (transformation) to each time series, and linearly detrend, de-mean, and 
# standardize variances to 1.
ts_sulphate_boxcox <- cleandat(dat = ts_sulphate, times = ts_times, clev = 5)
ts_doc_boxcox <- cleandat(dat = ts_doc, times = ts_times, clev = 5)
ts_dief_boxcox <- cleandat(dat = ts_dief, times = ts_times, clev = 5)
ts_ridge_boxcox <- cleandat(dat = ts_ridge, times = ts_times, clev = 5)
ts_iskwao_boxcox <- cleandat(dat = ts_iskwao, times = ts_times, clev = 5)
ts_ungauged_boxcox <- cleandat(dat = ts_ungauged, times = ts_times, clev = 5)
ts_bp_inflow_boxcox <- cleandat(dat = ts_bp_inflow, times = ts_times, clev = 5)
ts_ridge_iskwao_boxcox <- cleandat(dat = ts_ridge_iskwao, times = ts_times, clev = 5)

# Apply wavelet transform using wsyn::wt and plot the magnitude of the transform
# against time and timescale.
wt_sulphate <- wt(ts_sulphate_boxcox$cdat, ts_times)
wt_doc <- wt(ts_doc_boxcox$cdat, ts_times)
wt_dief <- wt(ts_dief_boxcox$cdat, ts_times)
wt_ridge <- wt(ts_ridge_boxcox$cdat, ts_times)
wt_iskwao <- wt(ts_iskwao_boxcox$cdat, ts_times)
wt_ungauged <- wt(ts_ungauged_boxcox$cdat, ts_times)
wt_bp_inflow <- wt(ts_bp_inflow_boxcox$cdat, ts_times)
wt_ridge_iskwao <- wt(ts_ridge_iskwao_boxcox$cdat, ts_times)

# plotmag(wt_sulphate)
# plotmag(wt_doc)
# plotmag(wt_dief)
# plotmag(wt_ridge)
# plotmag(wt_iskwao)
# plotmag(wt_ungauged)
# plotmag(wt_bp_inflow)
# plotmag(wt_ridge_iskwao)

# Then extract the numeric vectors for each cleaned time series to use with
# wavelet coherence analysis.
ts_sulphate_cleandat <- ts_sulphate_boxcox$cdat
ts_doc_cleandat <- ts_doc_boxcox$cdat
ts_dief_cleandat <- ts_dief_boxcox$cdat
ts_ridge_cleandat <- ts_ridge_boxcox$cdat
ts_iskwao_cleandat <- ts_iskwao_boxcox$cdat
ts_ungauged_cleandat <- ts_ungauged_boxcox$cdat
ts_bp_inflow_cleandat <- ts_bp_inflow_boxcox$cdat
ts_ridge_iskwao_cleandat <- ts_ridge_iskwao_boxcox$cdat


# Coherence ---------------------------------------------------------------

# Evaluate coherence between sulphate and flow using wsyn::coh.
coh_dief_so4 <- coh(dat1 = ts_dief_cleandat, dat2 = ts_sulphate_cleandat, times = ts_times, 
                norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                scale.max.input = 120)
# coh_ridge_so4 <- coh(dat1 = ts_ridge_cleandat, dat2 = ts_sulphate_cleandat, times = ts_times, 
#                  norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
#                  scale.max.input = 120)
# coh_iskwao_so4 <- coh(dat1 = ts_iskwao_cleandat, dat2 = ts_sulphate_cleandat, times = ts_times, 
#                   norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
#                   scale.max.input = 120)
# coh_ungauged_so4 <- coh(dat1 = ts_ungauged_cleandat, dat2 = ts_sulphate_cleandat, times = ts_times, 
#                     norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
#                     scale.max.input = 120)
coh_bp_inflow_so4 <- coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_sulphate_cleandat, times = ts_times, 
                     norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                     scale.max.input = 120)
coh_ridge_iskwao_so4 <- coh(dat1 = ts_ridge_iskwao_cleandat, dat2 = ts_sulphate_cleandat, times = ts_times, 
                        norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                        scale.max.input = 120)

# Evaluate coherence between DOC and sulphate using wsyn::coh.
coh_doc_so4 <- coh(dat1 = ts_sulphate_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                        norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                        scale.max.input = 120)


# Plot DOC and sulphate ---------------------------------------------------

plotmag(coh_doc_so4, sigthresh = 0.95); title("DOC and sulphate")
coh_doc_so4 <- bandtest(coh_doc_so4, c(8, 120)) # 8-120 months
coh_doc_so4 <- bandtest(coh_doc_so4, c(28, 120)) # 28-120 months
coh_doc_so4 <- bandtest(coh_doc_so4, c(8, 18)) # 8-18 months
coh_doc_so4 <- bandtest(coh_doc_so4, c(2, 3)) # 2-3 months
plotmag(coh_doc_so4, sigthresh = 0.95)
title("DOC and sulphate\n 2–3, 8–18, 28–120, and 8–120 months")

coh_doc_so4$bandp <- NA


# Plot sulphate and Dief --------------------------------------------------

plotmag(coh_dief_so4, sigthresh = 0.95); title("Sulphate and Lake Diefenbaker outflow")
coh_dief_so4 <- bandtest(coh_dief_so4, c(19, 120)) # 19-120 months
coh_dief_so4 <- bandtest(coh_dief_so4, c(2, 5)) # 2-5 months
plotmag(coh_dief_so4, sigthresh = 0.95)
title("Sulphate and Lake Diefenbaker outflow\n 2–5 and 19–120 months")

coh_dief_so4$bandp <- NA


# Plot sulphate and Ridge -------------------------------------------------

plotmag(coh_ridge_so4); title("Coherence between Sulphate and Ridge Creek")

coh_ridge_so4 <- bandtest(coh_ridge_so4, c(18, 42)) # 1.5—3.5 year timescales
plotmag(coh_ridge_so4); title("Coherence between Sulphate and Ridge Creek")

coh_ridge_so4$bandp <- NA


# Plot sulphate and Iskwao ------------------------------------------------

plotmag(coh_iskwao_so4); title("Coherence between Sulphate and Iskwao Creek")
coh_iskwao_so4 <- bandtest(coh_iskwao_so4, c(4, 5)) # 4 to 5 months
plotmag(coh_iskwao_so4); title("Coherence between Sulphate and Iskwao Creek")

coh_iskwao_so4$bandp <- NA


# Plot sulphate and ungauged ----------------------------------------------

plotmag(coh_ungauged_so4); title("Coherence between Sulphate and Ungauged flow")

ungauged_band1 <- c(2, 8) # 2—8 months
ungauged_band2 <- c(20, 27) # 1.67—2.25 years 
coh_ungauged_so4 <- bandtest(coh_ungauged_so4, ungauged_band1)
coh_ungauged_so4 <- bandtest(coh_ungauged_so4, ungauged_band2)
plotmag(coh_ungauged_so4); title("Coherence between Sulphate and Ungauged flow")

coh_ungauged_so4$bandp <- NA


# Plot sulphate and BP inflow ---------------------------------------------

plotmag(coh_bp_inflow_so4, sigthresh = 0.95); title("Sulphate and Buffalo Pound Lake inflow")

coh_bp_inflow_so4 <- bandtest(coh_bp_inflow_so4, c(10, 120)) # 10-120 months
coh_bp_inflow_so4 <- bandtest(coh_bp_inflow_so4, c(2, 4)) # 2-4 months
plotmag(coh_bp_inflow_so4, sigthresh = 0.95)
title("Sulphate and Buffalo Pound inflow\n 2–4 and 10–120 months")

coh_bp_inflow_so4$bandp <- NA



# Plot sulphate and Ridge + Iskwao ----------------------------------------

plotmag(coh_ridge_iskwao_so4, sigthresh = 0.95); title("Sulphate and combined Ridge Creek and Iskwao Creek flows")

coh_ridge_iskwao_so4 <- bandtest(coh_ridge_iskwao_so4, c(19, 45)) # 19-45 months
coh_ridge_iskwao_so4 <- bandtest(coh_ridge_iskwao_so4, c(2, 9)) # 2-9 months
plotmag(coh_ridge_iskwao_so4, sigthresh = 0.95)
title("Sulphate and combined Ridge Creek and Iskwao Creek flows\n 2–9 and 19–45 months")

coh_ridge_iskwao_so4$bandp <- NA


# # combine plots
# par(mfrow = c(3, 3))
# plotmag(coh_doc_so4); title("Coherence between DOC and sulphate\n 3–7 year timescales")
# plotmag(coh_dief_so4); title(main = "Coherence between Sulphate and Lake Diefenbaker outflow\n 3–5 month and 2–5 year timescales")
# plotmag(coh_ridge_so4); title("Coherence between Sulphate and Ridge Creek flow\n 1.5–3.5 year timescales")
# plotmag(coh_iskwao_so4); title("Coherence between Sulphate and Iskwao Creek flow\n 4–5 month timescales")
# plotmag(coh_ridge_iskwao_so4); title("Coherence between Sulphate and \ncombined Ridge Creek and Iskwao Creek flows\n 4–5 month and 1.5–3.5 year timescales")
# plotmag(coh_ungauged_so4); title("Coherence between Sulphate and estimated ungauged flow\n 2–8 month and 1.5–2 year timescales")
# plotmag(coh_bp_inflow_so4); title("Coherence between Sulphate and \nQu'Appelle River above Buffalo Pound Lake\n 2–3 month and 2–3 year timescales")
# 
# par(mfrow = c(2, 3))
# plotmag(coh_dief_so4); title(main = "Sulphate and Lake Diefenbaker outflow")
# plotmag(coh_ridge_so4); title("Sulphate and Ridge Creek flow")
# plotmag(coh_iskwao_so4); title("Sulphate and Iskwao Creek flow")
# plotmag(coh_ungauged_so4); title("Sulphate and ungauged flow")
# plotmag(coh_ridge_iskwao_so4); title("Sulphate and combined Ridge Creek + Iskwao Creek flows")
# plotmag(coh_bp_inflow_so4); title("Sulphate and Buffalo Pound inflow")
