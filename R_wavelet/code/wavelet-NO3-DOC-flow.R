library(wsyn)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(patchwork)

source("./R_data-cleaning/bpwtp/code/clean-bpwtp-nitrate.R")
source("./R_data-cleaning/flow-reconstruction/code/clean-flows.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-DOC.R")
# source("./R_wavelet/code/wavelet-DOC-flow.R")

NO3_lab <- expression(paste("NO"[3]*" concentration (mg L"^-1 * ")"))
DOC_lab <- expression(paste("DOC concentration (mg L"^-1*")"))


station_flow_raw <- station_flow_monthly() # n = 360
bp_nitrate_raw <- bp_nitrate_monthly() # n = 360
bp_doc_raw <- DOC_complete_1990_2019() # n = 1564

bpn <- bp_nitrate %>% 
  unite("year_month", c(Year, Month), sep = '-', remove = FALSE) %>% 
  mutate(year_month = paste0(year_month, "-01"),
         date_ymd = ymd(year_month))

p1 <- bp_nitrate %>% 
  unite("year_month", c(Year, Month), sep = '-', remove = FALSE) %>% 
  mutate(year_month = paste0(year_month, "-01"),
         date_ymd = ymd(year_month)) %>% 
  ggplot(aes(date_ymd, NO3_mg.L)) +
  # facet_wrap(~ Year) +
  geom_line(size = 1) + 
  # geom_point(col = "white", size = 2) +
  # geom_point(shape = 1) + 
  labs(x = NULL, y = NO3_lab)

p2 <- bp_doc %>% 
  unite("year_month", c(Year, Month), sep = '-', remove = FALSE) %>% 
  mutate(year_month = paste0(year_month, "-01"),
         date_ymd = ymd(year_month)) %>% 
  ggplot(aes(date_ymd, DOC_mg.L)) +
  # facet_wrap(~ Year) +
  geom_line(size = 1) + 
  geom_line(data = bpn, aes(date_ymd, NO3_mg.L), size = 1, col = "steelblue")
  # geom_point(col = "white", size = 2) +
  # geom_point(shape = 1) + 
  labs(x = NULL, y = DOC_lab)

p2/p1

# Prepare data sets -------------------------------------------------------

station_flow <- station_flow_raw

bp_nitrate <- bp_nitrate_raw %>% select(Year = year, Month = month, NO3_mg.L)

bp_doc <- bp_doc_raw %>% 
  select(Year = year, Month = month, Week = week, DOC_mg.L) %>% 
  group_by(Year, Month) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L, na.rm = TRUE)) %>% 
  ungroup() 



nitrate_doc_flow <- right_join(station_flow, bp_nitrate) %>% 
  right_join(., bp_doc) %>% 
  select(date_ymd:Month, DOC_mg.L, NO3_mg.L, everything())



# Wavelet analyses --------------------------------------------------------

# Create vectors of each time series
ts_times <- rep(1:length(nitrate_doc_flow$NO3_mg.L)) # length of time series (dates)
ts_doc <- nitrate_doc_flow$DOC_mg.L # DOC concentration
ts_nitrate <- nitrate_doc_flow$NO3_mg.L # SO4 concentration
ts_dief <- nitrate_doc_flow$SK05JG006_cms # Lake Diefenbaker outflow
ts_ridge <- nitrate_doc_flow$SK05JG013_cms # Ridge Creek 
ts_iskwao <- nitrate_doc_flow$SK05JG014_combined_cms # Iskwao Creek
ts_ungauged <- nitrate_doc_flow$Ungauged_predicted_cms # Ungauged contribution
ts_bp_inflow <- nitrate_doc_flow$SK05JG004_combined_cms # QPR above BPL
ts_ridge_iskwao <- nitrate_doc_flow$RC_IC_cms # Ridge Creek + Iskwao Creek

# Use wsyn::cleandat to apply an optimal Box-Cox normalization procedure 
# (transformation) to each time series, and linearly detrend, de-mean, and 
# standardize variances to 1.
ts_nitrate_boxcox <- cleandat(dat = ts_nitrate, times = ts_times, clev = 5)
ts_doc_boxcox <- cleandat(dat = ts_doc, times = ts_times, clev = 5)
ts_dief_boxcox <- cleandat(dat = ts_dief, times = ts_times, clev = 5)
ts_ridge_boxcox <- cleandat(dat = ts_ridge, times = ts_times, clev = 5)
ts_iskwao_boxcox <- cleandat(dat = ts_iskwao, times = ts_times, clev = 5)
ts_ungauged_boxcox <- cleandat(dat = ts_ungauged, times = ts_times, clev = 5)
ts_bp_inflow_boxcox <- cleandat(dat = ts_bp_inflow, times = ts_times, clev = 5)
ts_ridge_iskwao_boxcox <- cleandat(dat = ts_ridge_iskwao, times = ts_times, clev = 5)

# Apply wavelet transform using wsyn::wt and plot the magnitude of the transform
# against time and timescale.
wt_nitrate <- wt(ts_nitrate_boxcox$cdat, ts_times)
wt_doc <- wt(ts_doc_boxcox$cdat, ts_times)
wt_dief <- wt(ts_dief_boxcox$cdat, ts_times)
wt_ridge <- wt(ts_ridge_boxcox$cdat, ts_times)
wt_iskwao <- wt(ts_iskwao_boxcox$cdat, ts_times)
wt_ungauged <- wt(ts_ungauged_boxcox$cdat, ts_times)
wt_bp_inflow <- wt(ts_bp_inflow_boxcox$cdat, ts_times)
wt_ridge_iskwao <- wt(ts_ridge_iskwao_boxcox$cdat, ts_times)

plotmag(wt_nitrate)
plotmag(wt_doc)
plotmag(wt_dief)
plotmag(wt_ridge)
plotmag(wt_iskwao)
plotmag(wt_ungauged)
plotmag(wt_bp_inflow)
plotmag(wt_ridge_iskwao)

# Then extract the numeric vectors for each cleaned time series to use with
# wavelet coherence analysis.
ts_nitrate_cleandat <- ts_nitrate_boxcox$cdat
ts_doc_cleandat <- ts_doc_boxcox$cdat
ts_dief_cleandat <- ts_dief_boxcox$cdat
ts_ridge_cleandat <- ts_ridge_boxcox$cdat
ts_iskwao_cleandat <- ts_iskwao_boxcox$cdat
ts_ungauged_cleandat <- ts_ungauged_boxcox$cdat
ts_bp_inflow_cleandat <- ts_bp_inflow_boxcox$cdat
ts_ridge_iskwao_cleandat <- ts_ridge_iskwao_boxcox$cdat


# Coherence ---------------------------------------------------------------

# Evaluate coherence between nitrate and flow using wsyn::coh.
coh_dief_no3 <- coh(dat1 = ts_dief_cleandat, dat2 = ts_nitrate_cleandat, times = ts_times, 
                    norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                    scale.max.input = 120)
coh_ridge_no3 <- coh(dat1 = ts_ridge_cleandat, dat2 = ts_nitrate_cleandat, times = ts_times, 
                     norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                     scale.max.input = 120)
coh_iskwao_no3 <- coh(dat1 = ts_iskwao_cleandat, dat2 = ts_nitrate_cleandat, times = ts_times, 
                      norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                      scale.max.input = 120)
coh_ungauged_no3 <- coh(dat1 = ts_ungauged_cleandat, dat2 = ts_nitrate_cleandat, times = ts_times, 
                        norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                        scale.max.input = 120)
coh_bp_inflow_no3 <- coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_nitrate_cleandat, times = ts_times, 
                         norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                         scale.max.input = 120)
coh_ridge_iskwao_no3 <- coh(dat1 = ts_ridge_iskwao_cleandat, dat2 = ts_nitrate_cleandat, times = ts_times, 
                            norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                            scale.max.input = 120)

# Evaluate coherence between DOC and nitrate using wsyn::coh.
coh_doc_no3 <- coh(dat1 = ts_nitrate_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                       norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                       scale.max.input = 120)


# Plot DOC and nitrate ---------------------------------------------------

# not coherent at any timescales
plotmag(coh_doc_no3); title("Coherence between DOC and nitrate")


# Plot nitrate and Dief --------------------------------------------------

plotmag(coh_dief_no3, sigthresh = 0.95)
title("Nitrate and Lake Diefenbaker outflow")
coh_dief_no3 <- bandtest(coh_dief_no3, c(15, 19))
coh_dief_no3 <- bandtest(coh_dief_no3, c(3, 4))
plotmag(coh_dief_no3, sigthresh = 0.95)
title("Nitrate and Lake Diefenbaker outflow\n 3–4 and 15–19 months")

coh_dief_no3$bandp <- NA


# Plot nitrate and Ridge -------------------------------------------------

# plotmag(coh_ridge_no3); title("Nitrate and Ridge Creek flow")


# Plot nitrate and Iskwao ------------------------------------------------

# plotmag(coh_iskwao_no3); title("Nitrate and Iskwao Creek flow")


# Plot nitrate and ungauged ----------------------------------------------

# plotmag(coh_ungauged_no3); title("Nitrate and ungauged flow")



# Plot nitrate and Ridge + Iskwao ----------------------------------------

plotmag(coh_ridge_iskwao_no3, sigthresh = 0.95)
title("Nitrate and \ncombined Ridge Creek and Iskwao Creek flows")
coh_ridge_iskwao_no3 <- bandtest(coh_ridge_iskwao_no3, c(2.5, 3))
plotmag(coh_ridge_iskwao_no3, sigthresh = 0.95)
title("Nitrate and \ncombined Ridge Creek and Iskwao Creek flows\n 2.5–3 months")

coh_ridge_iskwao_no3$bandp <- NA

# Plot nitrate and BP inflow ---------------------------------------------

plotmag(coh_bp_inflow_no3, sigthresh = 0.95)
title("Nitrate and Buffalo Pound Lake inflow")
coh_bp_inflow_no3 <- bandtest(coh_bp_inflow_no3, c(9, 18))
plotmag(coh_bp_inflow_no3, sigthresh = 0.95)
title("Nitrate and Buffalo Pound Lake inflow\n 9–18 months")

coh_bp_inflow_no3$bandp <- NA




# All plots ---------------------------------------------------------------

plotmag(coh_doc_no3); title("Coherence between DOC and nitrate concentrations")

par(mfrow = c(2, 3))
plotmag(coh_dief_no3); title(main = "Nitrate and Lake Diefenbaker outflow")
plotmag(coh_ridge_no3); title("Nitrate and Ridge Creek flow")
plotmag(coh_iskwao_no3); title("Nitrate and Iskwao Creek flow")
plotmag(coh_ungauged_no3); title("Nitrate and Ungauged flow")
plotmag(coh_ridge_iskwao_no3); title("Nitrate and combined Ridge Creek + Iskwao Creek flows")
plotmag(coh_bp_inflow_no3); title("Nitrate and Buffalo Pound inflow")
