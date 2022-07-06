library(wsyn)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(patchwork)

# source("./R_wavelet/code/imputation.R")
source("./R_data-cleaning/bpwtp/code/clean-bpwtp-DOC.R")
source("./R_data-cleaning/flow-reconstruction/code/clean-flows.R")
source("./R_data-cleaning/flow-reconstruction/code/BP-drainage-areas.R")

theme_set(theme_bw(base_size = 12))

# Read in DOC,  flow,  and drainage areas ---------------------------------

BP_doc_raw <- DOC_complete_1990_2019() 
BP_weekly_flow_raw <- station_flow_weekly()
BP_drainage_areas_raw <- drainage_areas()

# Join DOC,  flow,  and scaled Ridge Creek flow ---------------------------

bp_doc <- BP_doc_raw %>% 
  select(date_ymd, Year = year, Week = week, DOC_mg.L)

bp_weekly_flow <- BP_weekly_flow_raw %>% 
  select(Year, Week, SK05JG004_combined_cms, SK05JG006_cms, SK05JG014_combined_cms, 
         SK05JG013_cms, Ungauged_predicted_cms, RC_IC_cms)

bp_flow_doc <- left_join(bp_weekly_flow, bp_doc) %>% filter(!is.na(DOC_mg.L))

# bp_flow_doc_fn <- function(df_doc = BP_doc_raw, 
#                            df_flow = BP_weekly_flow_raw,
#                            df_scale = BP_drainage_areas_raw) {
#   
#   # 1564 rows
#   doc_weekly <- df_doc %>% 
#     rename(Year = year, Week = week) %>% 
#     select(date_ymd, Year, Week, DOC_mg.L)
#   
#   # 1590 rows (full record, 1990—2019)
#   flow_weekly <- df_flow %>% filter(Year %in% c(1990:2019))
#   
#   bp_flow_doc <- left_join(flow_weekly, doc_weekly) %>% 
#     select(date_ymd, Year, Week, DOC_mg.L, everything()) %>% 
#     filter(!is.na(DOC_mg.L)) 
#   
#   # Scale Ridge Creek to BP effective area
#   # Use flow from Ridge Creek, Ridge Creek's effective area, and BP's effective
#   # area to scale Ridge Creek flows to the effective area of the portion of the 
#   # catchment between Diefenbaker Lake and Buffalo Pound Lake (and including BPL)
#   BP_drainage_areas <- df_scale %>% mutate(AAFC_effective_m2 = AAFC_effective * 1e6)
#   
#   area_RC1 <- BP_drainage_areas %>% filter(station_name == "Ridge Creek") %>% select(AAFC_effective_m2) 
#   area_RC <- as.vector(area_RC1$AAFC_effective_m2)
#   
#   area_BP1 <- BP_drainage_areas %>% filter(station_name == "Buffalo Pound Lake near Tuxford") %>% select(AAFC_effective_m2) 
#   area_BP <- as.vector(area_BP1$AAFC_effective_m2)
#   
#   RC_scaled <- bp_flow_doc %>% 
#     mutate(SK05JG013_scaled_cms = SK05JG013_cms / area_RC * area_BP) %>% 
#     select(date_ymd:DOC_mg.L, SK05JG013_cms, SK05JG013_scaled_cms)
#   
#   RC_scaled %>% # Ridge Creek scaled
#     ggplot(aes(date_ymd, SK05JG013_scaled_cms)) + 
#     geom_line() +
#     theme(axis.title.x = element_blank()) +
#     labs(y = "Ridge Creek scaled to \nBuffalo Pound lake effective area")
#   
#   # Incorporate scaled flow into DOC and flow data frame
#   doc_flow_scale <- bp_flow_doc %>% mutate(SK05JG013_scaled_cms = RC_scaled$SK05JG013_scaled_cms)  
#   
#   # Add Ridge Creek and Iskwao Creek flows together
#   doc_flow <- doc_flow_scale %>% mutate(RC_IC_cms = SK05JG013_cms + SK05JG014_combined_cms)
#   
#   return(doc_flow)
#   
# }



# Plot DOC and flow time series -------------------------------------------

DOC_lab <- expression(paste("DOC (mg L"^-1*")")) 
Q_lab <- expression(paste("Discharge (m"^3*" s"^-1*")")) 

p_doc <- bp_flow_doc %>% 
  ggplot(aes(date_ymd, DOC_mg.L)) + 
  geom_line(col = "forestgreen") + 
  scale_y_continuous(breaks = c(3,6,9,12,15), labels = c(3,6,9,12,15)) +
  labs(x = NULL, y = DOC_lab, subtitle = "Buffalo Pound Lake DOC conentration at Water Treatment Plant intake") 
p_qpr_bpl <- bp_flow_doc %>% # Qu'Appelle River above BPL
  ggplot(aes(date_ymd, SK05JG004_combined_cms)) + 
  geom_line(col = "steelblue") +
  labs(x = "Year", y = NULL, subtitle = "Qu'Appelle River above Buffalo Pound Lake")
p_dief <- bp_flow_doc %>% # Diefenbaker 
  ggplot(aes(date_ymd, SK05JG006_cms)) + 
  geom_line(col = "steelblue") +
  ylim(c(0, 15)) + 
  labs(x = NULL, y = Q_lab, subtitle = "Lake Diefenbaker outflow")
p_ridge <- bp_flow_doc %>% # Ridge Creek
  ggplot(aes(date_ymd, SK05JG013_cms)) + 
  geom_line(col = "steelblue") +
  labs(x = NULL, y = Q_lab, subtitle = "Ridge Creek")
p_iskwao <- bp_flow_doc %>% # Iskwao Creek
  ggplot(aes(date_ymd, SK05JG014_combined_cms)) + 
  geom_line(col = "steelblue") +
  ylim(c(0, 8)) + 
  labs(x = NULL, y = Q_lab, subtitle = "Iskwao Creek") 
p_ungauged <- bp_flow_doc %>% # Ungauged 
  ggplot(aes(date_ymd, Ungauged_predicted_cms)) + 
  geom_line(col = "steelblue") +
  ylim(c(NA, 30)) + 
  labs(x = "Year", y = Q_lab, subtitle = "Ungauged contribution") 
p_ridge_scaled <- bp_flow_doc %>% # Ridge Creek scaled
  ggplot(aes(date_ymd, SK05JG013_scaled_cms)) + 
  geom_line(col = "steelblue") +
  ylim(c(0, 100)) + 
  labs(x = NULL, y = NULL, subtitle = "Ridge Creek scaled to Buffalo Pound Lake effective area") 
p_ridge_iskwao <- bp_flow_doc %>% # Ridge Creek + Iskwao Creek
  ggplot(aes(date_ymd, RC_IC_cms)) + 
  geom_line(col = "steelblue") +
  labs(x = "Year", y = NULL, subtitle = "Ridge Creek + Iskwao Creek")

(p_doc) / (p_dief + p_ridge) / (p_iskwao + p_ridge_iskwao) / (p_ungauged + p_qpr_bpl) + plot_annotation(tag_levels = 'A', tag_suffix = ')')
# ggsave("./R_wavelet/outputs/figures/20220112_doc_flow_ts.png", p_ts, height = 7.56, width = 10.52)


# Check for normality -----------------------------------------------------


p_hist <- bp_flow_doc %>% 
  select(-c(date_ymd, Year, Week)) %>%
  rename(`DOC concentration` = DOC_mg.L,
         `Buffalo Pound Lake inflow` = SK05JG004_combined_cms,
         `Lake Diefenbaker outflow` = SK05JG006_cms,
         `Ridge Creek` = SK05JG013_cms,
         `Iskwao Creek` = SK05JG014_combined_cms,
         `Ungauged contributions` = Ungauged_predicted_cms) %>% 
  select(-contains(c("predicted", "SK05"))) %>% 
  gather(cols, value) %>%  
  ggplot(aes(x = value)) + 
  geom_histogram() +
  labs(x = "Value", y = "Count") +
  theme_bw(base_size = 14) +
  facet_wrap(~ cols, ncol = 3, scales = "free")

# ggsave("./R_wavelet/outputs/figures/20220113_ts_histograms.png", p_hist, height = 5, width = 10.52)


p_hist_log <- bp_flow_doc %>% 
  select(-c(date_ymd, Year, Week)) %>%
  select(-contains("predicted")) %>% 
  gather(cols, value) %>%  
  filter(!is.na(value)) %>% 
  mutate(value_log = log(value) + 10) %>% 
  ggplot(aes(x = value_log)) + 
  geom_histogram() +
  theme_bw(base_size = 14) +
  facet_wrap(~ cols, ncol = 2, scales = "free")

p_hist + p_hist_log

# Transforming doesn't necessarily fix issues with normality across all sites, 
# but can be dealt with using wsyn's cleandat function 


# Check autocorrelation in time series ------------------------------------

acf(bp_flow_doc$DOC_mg.L)
acf(bp_flow_doc$SK05JG006_cms)
acf(bp_flow_doc$SK05JG013_scaled_cms)
acf(bp_flow_doc$SK05JG013_cms)
acf(bp_flow_doc$Ungauged_predicted_cms)
acf(bp_flow_doc$SK05JG014_combined_cms)
acf(bp_flow_doc$SK05JG004_combined_cms)

# There is autocorrelation in all time series, which can also be handled by
# wsyn's cleandat function.


# Wavelet analyses --------------------------------------------------------


# Weekly time series ------------------------------------------------------


# Create vectors of each time series
ts_times <- rep(1:length(bp_flow_doc$DOC_mg.L)) # length of time series (dates)
ts_doc <- bp_flow_doc$DOC_mg.L # DOC concentration
ts_dief <- bp_flow_doc$SK05JG006_cms # Lake Diefenbaker outflow
ts_ridge <- bp_flow_doc$SK05JG013_cms # Ridge Creek 
ts_iskwao <- bp_flow_doc$SK05JG014_combined_cms # Iskwao Creek
ts_ungauged <- bp_flow_doc$Ungauged_predicted_cms # Ungauged contribution
ts_bp_inflow <- bp_flow_doc$SK05JG004_combined_cms # QPR above BPL
ts_ridge_iskwao <- bp_flow_doc$RC_IC_cms # Ridge Creek + Iskwao Creek
  
# Use wsyn::cleandat to apply an optimal Box-Cox normalization procedure 
# (transformation) to each time series, and linearly detrend, de-mean, and 
# standardize variances to 1.
ts_doc_boxcox <- cleandat(dat = ts_doc, times = ts_times, clev = 5)
ts_dief_boxcox <- cleandat(dat = ts_dief, times = ts_times, clev = 5)
ts_ridge_boxcox <- cleandat(dat = ts_ridge, times = ts_times, clev = 5)
ts_iskwao_boxcox <- cleandat(dat = ts_iskwao, times = ts_times, clev = 5)
ts_ungauged_boxcox <- cleandat(dat = ts_ungauged, times = ts_times, clev = 5)
ts_bp_inflow_boxcox <- cleandat(dat = ts_bp_inflow, times = ts_times, clev = 5)
ts_ridge_iskwao_boxcox <- cleandat(dat = ts_ridge_iskwao, times = ts_times, clev = 5)

# Apply wavelet transform using wsyn::wt and plot the magnitude of the transform
# against time and timescale.
wt_doc <- wt(ts_doc_boxcox$cdat, ts_times)
wt_dief <- wt(ts_dief_boxcox$cdat, ts_times)
wt_ridge <- wt(ts_ridge_boxcox$cdat, ts_times)
wt_iskwao <- wt(ts_iskwao_boxcox$cdat, ts_times)
wt_ungauged <- wt(ts_ungauged_boxcox$cdat, ts_times)
wt_bp_inflow <- wt(ts_bp_inflow_boxcox$cdat, ts_times)
wt_ridge_iskwao <- wt(ts_ridge_iskwao_boxcox$cdat, ts_times)

plotmag(wt_doc)
plotmag(wt_dief)
plotmag(wt_ridge)
plotmag(wt_iskwao)
plotmag(wt_ungauged)
plotmag(wt_bp_inflow)
plotmag(wt_ridge_iskwao)

# Then extract the numeric vectors for each cleaned time series to use with
# wavelet coherence analysis.
ts_doc_cleandat <- ts_doc_boxcox$cdat
ts_dief_cleandat <- ts_dief_boxcox$cdat
ts_ridge_cleandat <- ts_ridge_boxcox$cdat
ts_iskwao_cleandat <- ts_iskwao_boxcox$cdat
ts_ungauged_cleandat <- ts_ungauged_boxcox$cdat
ts_bp_inflow_cleandat <- ts_bp_inflow_boxcox$cdat
ts_ridge_iskwao_cleandat <- ts_ridge_iskwao_boxcox$cdat

# Evaluate coherence between DOC and flow using wsyn::coh.
coh_dief_doc <- coh(dat1 = ts_dief_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                    norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                    scale.max.input = 520)
coh_ridge_doc <- coh(dat1 = ts_ridge_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                     norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                     scale.max.input = 520)
coh_iskwao_doc <- coh(dat1 = ts_iskwao_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                      norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                      scale.max.input = 520)
coh_ungauged_doc <- coh(dat1 = ts_ungauged_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                        norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                        scale.max.input = 520)
coh_bp_inflow_doc <- coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                         norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                         scale.max.input = 520)
coh_ridge_iskwao_doc <- coh(dat1 = ts_ridge_iskwao_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                            norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                            scale.max.input = 520)

# Plot coherence

### Dief
plotmag(coh_dief_doc); title("Coherence between DOC and Lake Diefenbaker outflow")
dief_band <- bandtest(coh_dief_doc, c(78, 500))
plotmag(dief_band); title("Coherence between DOC and Lake Diefenbaker outflow")
get_bandp(dief_band)
# p = 1e-04 at timescales of 78 to 500 weeks (1.5 to 10 years) (99% CI)

coh_dief_doc$bandp <- NA

### Ridge Creek
plotmag(coh_ridge_doc); title("Coherence between DOC and Ridge Creek")
ridge_short <- c(2, 3) # 2-3 weeks
ridge_long1 <- c(440, 520) # 8.4 to 10 years
coh_ridge_doc <- bandtest(coh_ridge_doc, band = ridge_short)
coh_ridge_doc <- bandtest(coh_ridge_doc, band = ridge_long1)
plotmag(coh_ridge_doc); title("Coherence between DOC and Ridge Creek\n scaled to Buffalo Pound Lake effective area")
# p = 1e-04 at very short timescales of 2 to 3 weeks (99% CI)
# p = 0.0695 at very long timescales

coh_ridge_doc$bandp <- NA


### Iskwao Creek
plotmag(coh_iskwao_doc); title("Coherence between DOC and Iskwao Creek")
iskwao_med <- c(78, 130) # ~1.5 to 2.5 years
coh_iskwao_doc <- bandtest(coh_iskwao_doc, band = iskwao_med)
plotmag(coh_iskwao_doc); title("Coherence between DOC and Iskwao Creek")
# p = 0.0067 at timescales of 78 to 130 weeks (1.5 to 2.5 years)

coh_iskwao_doc$bandp <- NA


### Ungauged
plotmag(coh_ungauged_doc); title("Coherence between DOC and Ungauged contributions")
# no coherence b/w DOC and ungauged likely because of the negative values for 
# ungauged flow

# BP inflow
plotmag(coh_bp_inflow_doc); title("Coherence between DOC and Buffalo Pound Lake inflow")
coh_bp_inflow_doc <- bandtest(coh_bp_inflow_doc, c(130, 234)) # 2.5 to 4.5 years
coh_bp_inflow_doc <- bandtest(coh_bp_inflow_doc, c(416, 520)) # 8 to 10 years
plotmag(coh_bp_inflow_doc); title("Coherence between DOC and Buffalo Pound Lake inflow")
# p = 7e-04 at timescales of 130 to 234 weeks (2.5 to 4.5 years)

coh_bp_inflow_doc$bandp <- NA


### Ridge + Iskwao
plotmag(coh_ridge_iskwao_doc); title("Coherence between DOC and \ncombined Ridge Creek and Iskwao Creek flow")
coh_ridge_iskwao_doc <- bandtest(coh_ridge_iskwao_doc, c(2, 3))
plotmag(coh_ridge_iskwao_doc); title("Coherence between DOC and \ncombined Ridge Creek and Iskwao Creek flow")


# combine plots
par(mfrow = c(3, 2))
plotmag(dief_band); title("Coherence between DOC and Lake Diefenbaker outflow")
plotmag(coh_ridge_doc); title("Coherence between DOC and Ridge Creek flow")
plotmag(coh_iskwao_doc); title("Coherence between DOC and Iskwao Creek flow")
plotmag(coh_ridge_iskwao_doc); title("Coherence between DOC and \ncombined Ridge Creek and Iskwao Creek flows")
plotmag(coh_ungauged_doc); title("Coherence between DOC and estimated ungauged flow")
plotmag(coh_bp_inflow_doc); title("Coherence between DOC and \nQu'Appelle River above Buffalo Pound Lake")

par(mfrow = c(3, 2))
plotmag(coh_dief_doc); title("Coherence between DOC and Lake Diefenbaker outflow")
plotmag(coh_ridge_doc); title("Coherence between DOC and Ridge Creek flow")
plotmag(coh_iskwao_doc); title("Coherence between DOC and Iskwao Creek flow")
plotmag(coh_ridge_iskwao_doc); title("Coherence between DOC and \ncombined Ridge Creek and Iskwao Creek flows")
plotmag(coh_ungauged_doc); title("Coherence between DOC and\n estimated ungauged flow")
plotmag(coh_bp_inflow_doc); title("Coherence between DOC and \nQu'Appelle River above Buffalo Pound Lake")



# Monthly time series -----------------------------------------------------

BP_monthly_flow <- station_flow_monthly()
BP_monthly_DOC <- bp_DOC_monthly() %>% rename(Year = year, Month = month)

BP_doc_flow_monthly <- left_join(BP_monthly_flow, BP_monthly_DOC)

# Create vectors of each time series
ts_times <- rep(1:length(BP_doc_flow_monthly$DOC_mg.L)) # length of time series (dates)
ts_doc <- BP_doc_flow_monthly$DOC_mg.L # DOC concentration
ts_dief <- BP_doc_flow_monthly$SK05JG006_cms # Lake Diefenbaker outflow
ts_ridge <- BP_doc_flow_monthly$SK05JG013_cms # Ridge Creek 
ts_iskwao <- BP_doc_flow_monthly$SK05JG014_combined_cms # Iskwao Creek
ts_ungauged <- BP_doc_flow_monthly$Ungauged_predicted_cms # Ungauged contribution
ts_bp_inflow <- BP_doc_flow_monthly$SK05JG004_combined_cms # QPR above BPL
ts_ridge_iskwao <- BP_doc_flow_monthly$RC_IC_cms # Ridge Creek + Iskwao Creek

# Use wsyn::cleandat to apply an optimal Box-Cox normalization procedure 
# (transformation) to each time series, and linearly detrend, de-mean, and 
# standardize variances to 1.
ts_doc_boxcox <- cleandat(dat = ts_doc, times = ts_times, clev = 5)
ts_dief_boxcox <- cleandat(dat = ts_dief, times = ts_times, clev = 5)
ts_ridge_boxcox <- cleandat(dat = ts_ridge, times = ts_times, clev = 5)
ts_iskwao_boxcox <- cleandat(dat = ts_iskwao, times = ts_times, clev = 5)
ts_ungauged_boxcox <- cleandat(dat = ts_ungauged, times = ts_times, clev = 5)
ts_bp_inflow_boxcox <- cleandat(dat = ts_bp_inflow, times = ts_times, clev = 5)
ts_ridge_iskwao_boxcox <- cleandat(dat = ts_ridge_iskwao, times = ts_times, clev = 5)

# Apply wavelet transform using wsyn::wt and plot the magnitude of the transform
# against time and timescale.
wt_doc <- wt(ts_doc_boxcox$cdat, ts_times)
wt_dief <- wt(ts_dief_boxcox$cdat, ts_times)
wt_ridge <- wt(ts_ridge_boxcox$cdat, ts_times)
wt_iskwao <- wt(ts_iskwao_boxcox$cdat, ts_times)
wt_ungauged <- wt(ts_ungauged_boxcox$cdat, ts_times)
wt_bp_inflow <- wt(ts_bp_inflow_boxcox$cdat, ts_times)
wt_ridge_iskwao <- wt(ts_ridge_iskwao_boxcox$cdat, ts_times)

# plotmag(wt_doc)
# plotmag(wt_dief)
# plotmag(wt_ridge)
# plotmag(wt_iskwao)
# plotmag(wt_ungauged)
# plotmag(wt_bp_inflow)
# plotmag(wt_ridge_iskwao)

# wavelet coherence analysis.
ts_doc_cleandat <- ts_doc_boxcox$cdat
ts_dief_cleandat <- ts_dief_boxcox$cdat
ts_ridge_cleandat <- ts_ridge_boxcox$cdat
ts_iskwao_cleandat <- ts_iskwao_boxcox$cdat
ts_ungauged_cleandat <- ts_ungauged_boxcox$cdat
ts_bp_inflow_cleandat <- ts_bp_inflow_boxcox$cdat
ts_ridge_iskwao_cleandat <- ts_ridge_iskwao_boxcox$cdat

# Evaluate coherence between DOC and flow using wsyn::coh.
coh_dief_doc_monthly <- coh(dat1 = ts_dief_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                            norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                            scale.max.input = 120)
# coh_ridge_doc_monthly <- coh(dat1 = ts_ridge_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
#                              norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
#                              scale.max.input = 120)
# coh_iskwao_doc_monthly <- coh(dat1 = ts_iskwao_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
#                               norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
#                               scale.max.input = 120)
# coh_ungauged_doc_monthly <- coh(dat1 = ts_ungauged_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
#                                 norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
#                                 scale.max.input = 120)
coh_bp_inflow_doc_monthly <- coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                                 norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                                 scale.max.input = 120)
coh_ridge_iskwao_doc_monthly <- coh(dat1 = ts_ridge_iskwao_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                                    norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                                    scale.max.input = 120)


# Plot coherence

par(mfrow = c(1, 3))
plotmag(coh_dief_doc_monthly, sigthresh = 0.95); title("DOC and Lake Diefenbaker outflow")
# plotmag(coh_ridge_doc_monthly); title("DOC and Ridge Creek flow")
# plotmag(coh_iskwao_doc_monthly); title("DOC and Iskwao Creek flow")
# plotmag(coh_ungauged_doc_monthly); title("DOC and ungauged flow")
plotmag(coh_ridge_iskwao_doc_monthly, sigthresh = 0.95); title("DOC and combined Ridge Creek + Iskwao Creek flows")
plotmag(coh_bp_inflow_doc_monthly, sigthresh = 0.95); title("DOC and Buffalo Pound inflow")


# Diefenbaker
plotmag(coh_dief_doc_monthly, sigthresh = 0.95); title("DOC and Lake Diefenbaker outflow")
get_bandp(bandtest(coh_dief_doc_monthly, c(18, 120))) # 18-120 months, p = 9.999e-05
coh_dief_doc_monthly <- bandtest(coh_dief_doc_monthly, c(19, 120))
plotmag(coh_dief_doc_monthly, sigthresh = 0.95)
title("DOC and Lake Diefenbaker outflow\n 19–120 months")

# Ridge+Iskwao
plotmag(coh_ridge_iskwao_doc_monthly, sigthresh = 0.95); title("DOC and combined Ridge Creek + Iskwao Creek flows")
get_bandp(bandtest(coh_ridge_iskwao_doc_monthly, c(2, 120)))

# Buffalo Pound 
plotmag(coh_bp_inflow_doc_monthly, sigthresh = 0.95); title("DOC and Buffalo Pound inflow")
get_bandp(bandtest(coh_bp_inflow_doc_monthly, c(8, 10)))
coh_bp_inflow_doc_monthly <- bandtest(coh_bp_inflow_doc_monthly, c(9, 11))
coh_bp_inflow_doc_monthly <- bandtest(coh_bp_inflow_doc_monthly, c(30, 55))
plotmag(coh_bp_inflow_doc_monthly, sigthresh = 0.95)
title("DOC and Buffalo Pound inflow\n 9–11 and 30–55 months")

coh_bp_inflow_doc_monthly$bandp <- NA

par(mfrow = c(1, 3))
plotmag(coh_dief_doc_monthly, sigthresh = 0.95); title("DOC and Lake Diefenbaker outflow\n 19–120 months")
plotmag(coh_ridge_iskwao_doc_monthly, sigthresh = 0.95); title("DOC and combined Ridge Creek + Iskwao Creek flows")
plotmag(coh_bp_inflow_doc_monthly, sigthresh = 0.95); title("DOC and Buffalo Pound inflow\n 30–55 months")
