library(wsyn)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(patchwork)

source("./R_wavelet/code/imputation.R")
source("./R_data-cleaning/flow-reconstruction/code/clean-flows.R")
source("./R_data-cleaning/flow-reconstruction/code/BP-drainage-areas.R")


# Read in DOC,  flow,  and drainage areas ---------------------------------

BP_doc_raw <- DOC_complete_1990_2019() 
BP_weekly_flow_raw <- station_flow_weekly()
BP_drainage_areas_raw <- drainage_areas()



# Join DOC,  flow,  and scaled Ridge Creek flow ---------------------------

BP_flow_doc_fn <- function(df_doc = BP_doc_raw, 
                        df_flow = BP_weekly_flow_raw,
                        df_scale = BP_drainage_areas_raw) {
  
  # 1564 rows
  doc_weekly <- df_doc %>% 
    rename(Year = year, Week = week) %>% 
    select(date_ymd, Year, Week, DOC_mg.L)
  
  # 1590 rows (full record, 1990—2019)
  flow_weekly <- df_flow %>% filter(Year %in% c(1990:2019))
  
  BP_flow_doc <- left_join(flow_weekly, doc_weekly) %>% 
    select(date_ymd, Year, Week, DOC_mg.L, everything()) %>% 
    filter(!is.na(DOC_mg.L)) 
  
  # Scale Ridge Creek to BP effective area
  # Use flow from Ridge Creek, Ridge Creek's effective area, and BP's effective
  # area to scale Ridge Creek flows to the effective area of the portion of the 
  # catchment between Diefenbaker Lake and Buffalo Pound Lake (and including BPL)
  BP_drainage_areas <- df_scale %>% mutate(AAFC_effective_m2 = AAFC_effective * 1e6)
  
  area_RC1 <- BP_drainage_areas %>% filter(station_name == "Ridge Creek") %>% select(AAFC_effective_m2) 
  area_RC <- as.vector(area_RC1$AAFC_effective_m2)
  
  area_BP1 <- BP_drainage_areas %>% filter(station_name == "Buffalo Pound Lake near Tuxford") %>% select(AAFC_effective_m2) 
  area_BP <- as.vector(area_BP1$AAFC_effective_m2)
  
  RC_scaled <- BP_flow_doc %>% 
    mutate(SK05JG013_scaled_cms = SK05JG013_cms / area_RC * area_BP) %>% 
    select(date_ymd:DOC_mg.L, SK05JG013_cms, SK05JG013_scaled_cms)
  
  RC_scaled %>% # Ridge Creek scaled
    ggplot(aes(date_ymd, SK05JG013_scaled_cms)) + 
    geom_line() +
    theme(axis.title.x = element_blank()) +
    labs(y = "Ridge Creek scaled to \nBuffalo Pound lake effective area")
  
  # Incorporate scaled flow into DOC and flow data frame
  doc_flow_scale <- BP_flow_doc %>% mutate(SK05JG013_scaled_cms = RC_scaled$SK05JG013_scaled_cms)  
  
  return(doc_flow_scale)
  
}

BP_flow_doc <- BP_flow_doc_fn()



# Plot DOC and flow time series -------------------------------------------

p_doc <- BP_flow_doc %>% 
  ggplot(aes(date_ymd, DOC_mg.L)) + 
  geom_line() + 
  theme(axis.title.x = element_blank()) +
  labs(y = "DOC")
p_qpr_bpl <- BP_flow_doc %>% # Qu'Appelle River above BPL
  ggplot(aes(date_ymd, SK05JG004_combined_cms)) + 
  geom_line() +
  theme(axis.title.x = element_blank()) +
  labs(y = "BPL inflow")
p_dief <- BP_flow_doc %>% # Diefenbaker 
  ggplot(aes(date_ymd, SK05JG006_cms)) + 
  geom_line() +
  theme(axis.title.x = element_blank()) +
  labs(y = "Diefenbaker")
p_ridge <- BP_flow_doc %>% # Ridge Creek
  ggplot(aes(date_ymd, SK05JG013_cms)) + 
  geom_line() +
  theme(axis.title.x = element_blank()) +
  labs(y = "Ridge Creek")
p_iskwao <- BP_flow_doc %>% # Iskwao Creek
  ggplot(aes(date_ymd, SK05JG014_combined_cms)) + 
  geom_line() +
  theme(axis.title.x = element_blank()) +
  labs(y = "Iskwao Creek")
p_ungauged <- BP_flow_doc %>% # Ungauged 
  ggplot(aes(date_ymd, Ungauged_predicted_cms)) + 
  geom_line() +
  theme(axis.title.x = element_blank()) +
  labs(y = "Ungauged")
p_ridge_scaled <- RC_scaled %>% # Ridge Creek scaled
  ggplot(aes(date_ymd, SK05JG013_scaled_cms)) + 
  geom_line() +
  theme(axis.title.x = element_blank()) +
  labs(y = "Ridge Creek scaled to \nBuffalo Pound lake effective area")



# Check for normality -----------------------------------------------------

p_hist <- BP_flow_doc %>% 
  select(-c(date_ymd:Week)) %>%
  select(-contains("predicted")) %>% 
  gather(cols, value) %>%  
  ggplot(aes(x = value)) + 
  geom_histogram() +
  facet_wrap(~ cols, ncol = 2, scales = "free")

p_hist_log <- BP_flow_doc %>% 
  select(-c(date_ymd:Week)) %>%
  select(-contains("predicted")) %>% 
  gather(cols, value) %>%  
  filter(!is.na(value)) %>% 
  mutate(value_log = log(value) + 10) %>% 
  ggplot(aes(x = value_log)) + 
  geom_histogram() +
  facet_wrap(~ cols, ncol = 2, scales = "free")

p_hist + p_hist_log

# Transforming doesn't necessarily fix issues with normality across all sites, 
# but can be dealt with using wsyn's cleandat function 




# Wavelet analysis workflow -----------------------------------------------

# Use BP Inflow to illustrate the wavelet analysis workflow 
BP_Inflow <- BP_flow_doc %>% select(date_ymd:DOC_mg.L, SK05JG004_combined_cms)

# ts_times is a numeric vector equal to the length of the full time series and 
# is used in place of dates for use with wsyn functions
ts_times <- rep(1:length(BP_flow_doc$DOC_mg.L))

# ts_doc is a numeric vector equal to the lenght of the full time series and 
# contains DOC concentrations
ts_doc <- BP_flow_doc$DOC_mg.L

##### Step 1:
# Apply wavelet transform on DOC, obtaining a class of wt uisng the
# wsyn::cleandat() and wsyn::wt() functions.

# ### cleandat {wsyn} 
# Clean (spatio)temporal data matrices to make them ready for analyses.
# A data cleaning function for optimal Box-Cox transformation, detrending,
# standarizing variance, de-meaning.
# 
# cleandat(
#   dat,
#   times,
#   clev,
#   lambdas = seq(-10, 10, by = 0.01),
#   mints = NA
# )
# 
# Specifying cleandat(dat, times, clev = 5):
# If clev==5, an optimal Box-Cox normalization procedure is applied to each time 
# series individually (again after individually shifting according to mints),
# and transformed time series are then individually linearly detrended, 
# de-meaned, and variances are standardized to 1.
# When mints==NA (the default) cleandat uses the smallest difference between
# consecutive, distinct sorted values.
# 
# Default  parameter values for scale.min, scale.max.input, sigma, and f0 are 
# usually good enough for initial data exploration.
ts_doc_cleandat <- cleandat(dat = ts_doc, times = ts_times, clev = 5,
                            lambdas = seq(-10, 10, by = 0.01), mints = NA)
# List of 3
# $ cdat      : num [1:1564] -1.87 -2.11 -1.95 -1.91 -1.78 ...
# $ clev      : num 5
# $ optlambdas: num 0.43


### wt {wsyn}
# Computes the wavelet transform of a timeseries. Also the creator function for 
# the wt class. The wt class inherits from the tts class, which inherits from 
# the list class.
# 
# wt(
#   t.series,
#   times,
#   scale.min = 2,
#   scale.max.input = NULL,
#   sigma = 1.05,
#   f0 = 1
# )
# 
# # Default  parameter values for scale.min, scale.max.input, sigma, and f0 are 
# usually good enough for initial data exploration.
wtres_doc <- wt(t.series = ts_doc_cleandat$cdat, times = ts_times,
                scale.min = 2, scale.max.input = NULL, sigma = 1.05, f0 = 1)
# List of 5
# $ values    : cplx [1:1564, 1:119] NA NA NA ...
# $ times     : int [1:1564] 1 2 3 4 5 6 7 8 9 10 ...
# $ wtopt     :List of 4
# ..$ scale.min      : num 2
# ..$ scale.max.input: NULL
# ..$ sigma          : num 1.05
# ..$ f0             : num 1
# $ timescales: num [1:119] 2 2.1 2.21 2.32 2.43 ...
# $ dat       : num [1:1564] -1.87 -2.11 -1.95 -1.91 -1.78 ...
# - attr(*, "class")= chr [1:3] "wt" "tts" "list"
class(wtres_doc)
# [1] "wt"   "tts"  "list"
names(wtres_doc)
# [1] "values"     "times"      "wtopt"      "timescales" "dat"

# Use wsyn::plotmag() and wysn::plotphase() functions to plot magnitude and 
# phase of wtres_doc. Here these functions are used with tts objects, which are
# not useful at this stage. plotmag and plotphase will become more useful with
# coherence testing between DOC and flow.
# 
### plotmag {wsyn}
# For plotting the magnitude of values in tts objects (and derived classes)
# against time and timescale, and coh and wlmtest objects against timescale
#
## S3 method for class 'tts'
# plotmag(
#   object,
#   zlims = NULL,
#   neat = TRUE,
#   colorfill = NULL,
#   colorbar = TRUE,
#   title = NULL,
#   filename = NA,
#   ...
# )
#
# Use wsyn::plotmag with the tts class to plot the magnitude of the transform 
# against time and timescale.
plotmag(wtres_doc,  zlims = NULL, colorfill = NULL, colorbar = TRUE, 
        title = NULL, filename = NA)

### plotphase {wsyn}
# For plotting the phases of values in tts objects (and derived classes) against 
# time and timescale, and coh objects against timescale.
# 
# plotphase(
#   object,
#   ...
# )
plotphase(wtres_doc)

### power {wsyn}
# Returns the power of a tts object, i.e., the mean over time of the squared 
# magnitude (which is a function of timescale)
# 
# power(object)
h_doc <- power(wtres_doc)
str(h_doc)
# 'data.frame':	119 obs. of  2 variables:
# $ timescales: num  2 2.1 2.21 2.32 2.43 ...
# $ power     : num  0.0937 0.0937 0.0938 0.0945 0.0965 ...
head(h_doc)
tail(h_doc)
#   timescales     power
# 1   2.000000 0.2734354
# 2   2.100000 0.2728805
# 3   2.205000 0.2704232
# 4   2.315250 0.2678656
# 5   2.431013 0.2677021
# 6   2.552563 0.2717357
# ...
#    timescales     power
# 114  495.9305  81.74694
# 115  520.7270  52.08101
# 116  546.7633  27.91434
# 117  574.1015  23.78729
# 118  602.8066  49.82809
# 11   632.9469 106.99045

# Plot the power of wtres_doc. The x-axis is the timescales, represented as 
# "log-frequencies." The plot basically shows that DOC (on its own) has a strong
# relationship (high power) with time at long timescales... 
plot(x = log(1/h_doc$timescales), y = h_doc$power, type = "l", 
     lty = "solid", xaxt = "n", 
     xlab = "Timescales", ylab = "Power")
xlocs <- c(min(h_doc$timescales), pretty(h_doc$timescales, n = 100))
graphics::axis(side = 1, at = log(1/xlocs), labels = xlocs)

##### Step 2:
# Now work on coherence b/w DOC and BP inflow using wsyn::coh
ts_bp_inflow <- BP_Inflow$SK05JG004_combined_cms

# Use cleandat here to clean DOC and flow times series. 
ts_doc_cleandat <- cleandat(dat = ts_doc, times = ts_times, clev = 5)$cdat
ts_bp_inflow_cleandat <- cleandat(dat = ts_bp_inflow, times = ts_times, clev = 5)$cdat


### coh {wsyn}
# Coherence.
# Wavelet coherence and wavelet phase coherence, spatial or for single time 
# series. Also the generator function for the coh class, which inherits from the
# list class.
# 
# coh(
#   dat1,
#   dat2,
#   times,
#   norm,
#   sigmethod = "none",
#   nrand = 1000,
#   scale.min = 2,
#   scale.max.input = NULL,
#   sigma = 1.05,
#   f0 = 1
# )
# 
# dat1 is the DOC time series.
# dat2 is the flow times series.
# times is the date time series.
# 
# norm is the normalization of wavelet transforms to use. Controls the version 
# of the coherence that is performed. 
# One of "none", "phase", "powall", "powind". 
# Specifying norm = "powall" means the normalization method is that which is
# described in the "Wavelet mean field" section of the Methods of 
# Sheppard et al. (2016). Changes in large-scale climate alter spatial synchrony 
# of aphid pests. Nature Climate Change. DOI: 10.1038/nclimate2881. 
# This version of coherence is simply called the wavelet coherence. 
# 
# sigmethod is the method for significance testing.
# One of "none", "fftsurrog1", "fftsurrog2", "fftsurrog12", "aaftsurrog1", 
# "aaftsurrog2", "aaftsurrog12", "fast".
# 
# 
# res_doc_BPinflow <- wsyn::coh(dat1 = ts_BPinflow_cleandat, dat2 = ts_doc_cleandat, times = ts_times,
#                               norm = "powall", sigmethod = "fftsurrog1", nrand = 1000, f0 = 0.5,
#                               scale.max.input = 521)

res_doc_BPinflow <- wsyn::coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_doc_cleandat, times = ts_times,
                              norm = "powall", sigmethod = "fftsurrog1", nrand = 1000, f0 = 0.5,
                              scale.max.input = 521)

res_doc_BPinflow <- wsyn::coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_doc_cleandat, times = ts_times,
                              norm = "powall", sigmethod = "fftsurrog1", nrand = 1000,
                              scale.max.input = 1000)

#                               
# coh object:
# times, a length 1564 numeric vector:
# 1 2 3 4 5 ... 1560 1561 1562 1563 1564 
# Number of sampling locations: 
# timescales, a length 56 numeric vector:
# 2 2.1 2.205 2.31525 2.4310125 ... 24.0815395500828 25.2856165275869 26.5498973539663 27.8773922216646 29.2712618327478 
# norm, the normalization used: powall 
# wtopt: scale.min=2; scale.max.input=28; sigma=1.05; f0=0.5
# sigmethod, the type of significance testing used: fftsurrog1 
# Number of surrogates: 1000 
# The ranks slot is: empty
# Timescale bands tested in bandp slot: none


# The normalization to be used is specificed via norm. Here we use "powall",
# which corresponds to (spatial) wavelet coherence. When there is only one site,
# "powall" is just referred to as wavelet coherence. There are several methods
# for testing the significance of coherence, and the method used is controlled
# by the sigmethod argument. All significance methods are based on "surrogate
# datasets." These are datasets that have been normalized in an appropriate way.
# See section 5 for details of surrogates and significance testing, and allowed
# values of sigmethod.
#
# Larger values of nrand produce more accurate significance results that are
# less variable on repeat runs, but also require more computational time. Using
# nrand at least 1000 or 10000 for final runs is recommended.
#
# The arguments f0 and scale.max.input control the details of the wavelet
# transform (see section 2) and are set here to agree with values used in
# Sheppard et al. (2016).

plotmag(res_doc_BPinflow)
# The red line here is coherence.
# The black lines are 95th and 99th quantiles of coherences of surrogate 
# datasets (these are the default values for sigthresh).
# 
# Coherence in significant (the red line is above the black lines) for
# timescales maybe greater than 30 weeks, but not significant for timescales 
# less than than around 30 weeks... not sure why I can't see further.......

# Typically preferable is the fast option to sigmethod because far more
# surrogates can be used in the same computational time:
# norm = c("none", "phase", "powall", "powind")
res_doc_BPinflow <- coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                        norm = "powall", sigmethod = "fast", nrand = 1000,
                        f0 = 0.5)

res_doc_BPinflow <- coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                        norm = "powall", sigmethod = "fast", nrand = 10000,
                        f0 = 0.5)


# use bandtest to test the significance across timescales (i.e., for periods
# where where the red line is above the black line)
res_doc_BPinflow_bandtest <- bandtest(res_doc_BPinflow, c(125, 200))
# coh object:
#   times, a length 1564 numeric vector:
#   1 2 3 4 5 ... 1560 1561 1562 1563 1564 
# Number of sampling locations: 
#   timescales, a length 116 numeric vector:
#   2 2.1 2.205 2.31525 2.4310125 ... 449.823553872671 472.314731566305 495.93046814462 520.726991551851 546.763341129443 
# norm, the normalization used: powall 
# wtopt: scale.min=2; scale.max.input=521; sigma=1.05; f0=0.5
# sigmethod, the type of significance testing used: fast 
# Number of surrogates: 1000 
# The ranks slot is: filled
# Timescale bands tested in bandp slot:
#   ts_low_bd ts_hi_bd
# 1       125      200

res_doc_BPinflow_bandtest <- bandtest(res_doc_BPinflow, c(100, 250))

get_bandp(res_doc_BPinflow_bandtest)
# ts_low_bd ts_hi_bd       p_val    mn_phs
# 1       125      200 0.001998002 -1.725514

plotmag(res_doc_BPinflow_bandtest) 
plotrank(res_doc_BPinflow_bandtest)
plotphase(res_doc_BPinflow_bandtest)

# res_doc_BPinflow_bandtest <- bandtest(res_doc_BPinflow, c(6, 13))
# get_bandp(res_doc_BPinflow_bandtest)
# plotmag(res_doc_BPinflow_bandtest)
# 
# res_doc_BPinflow_bandtest <- bandtest(res_doc_BPinflow, c(5, 15))
# get_bandp(res_doc_BPinflow_bandtest)
# plotmag(res_doc_BPinflow_bandtest)












# Divide Ridge Creek flow (m3/s, convert tot km3/s) by the Ridge Creek effective 
# area (km2) to get the flow flux at Ridge Creek ()

# m3/s / 