library(wsyn)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
# library(mice)
# library(VIM)
library(patchwork)

source("./R_wavelet/code/imputation.R")
source("./R_data-cleaning/flow-reconstruction/code/clean-flows.R")
source("./R_data-cleaning/flow-reconstruction/code/BP-drainage-areas.R")
source("./R_flow-reconstruction/Anthony/code/time-series-plots.R")

# Read in data sets -------------------------------------------------------

# BP_longterm_raw <- read_csv("./R_bpwtp/thesis/data/BPWTP_labdat_current.csv") 
# BP_weekly_flow_raw <- read_csv("./R_flow-reconstruction/Anthony/outputs/data_clean/station_flow_weekly.csv")

BP_doc_raw <- DOC_complete_1990_2019() 
BP_weekly_flow_raw <- station_flow_weekly()

# Identify missing data ---------------------------------------------------

# It looks like best bet for imputation is using 1994—2019 
BP_longterm <- BP_longterm_raw %>% 
  filter(datasheet == "RawWater" & grepl("DOC", parm_unit)) %>%
  separate(datetime_ymd.hms, into = c("date_ymd", "time"), sep = " ") %>% 
  select(-c(datasheet, station, sheet_year, parameter, unit, parm_eval, 
            result_org, result_flag, time)) %>% 
  mutate(parm_unit = ifelse(parm_unit == "DOC.GFdiss_mg.L.C", "DOC_mg.L", parm_unit),
         date_ymd = as.Date(date_ymd),
         DOY = yday(date_ymd)) %>% 
  filter(date_ymd >= "1994-01-01" & date_ymd <= "2019-12-31")

# See where the missing data points are
BP_longterm %>%
  mutate(result = ifelse(is.na(result), 1, result)) %>% 
  ggplot() + 
  facet_wrap(~ year, ncol = 9) +
  geom_point(aes(DOY, result, colour = ifelse(BP_longterm$result == 1, "grey", "red"))) +
  theme(legend.position = "none")

# Another plot showing where the missing data points are
n_doc <- BP_longterm %>% 
  group_by(year) %>% 
  summarise(n = n())

n_na_doc <- BP_longterm %>% 
  filter(is.na(result)) %>% 
  group_by(year) %>% 
  summarise(n_na = n())

missing_doc <- left_join(n_doc, n_na_doc) %>% mutate(n_na = ifelse(is.na(n_na), 0, n_na))
missing_doc %>% 
  mutate(na_percent = n_na / n * 100) %>% 
  ggplot(aes(year, na_percent)) +
  geom_col() +
  geom_hline(yintercept = 5, col = "green", lty = 2) +
  geom_hline(yintercept = 10, col = "yellow", lty = 2) +
  geom_hline(yintercept = 20, col = "red", lty = 2) 

# Missing data is concentrated b/w 2001 to 2006


# Imputation with mice package --------------------------------------------

# The data set is missing dates (2002-12-30, 2019-01-07, 1996-12-30, 2001-12-31, 2018-01-01)
BP_longterm_result <- BP_longterm %>%
  select(date_ymd, result) %>% 
  mutate(date_ymd = as.character(date_ymd)) %>% 
  add_row(date_ymd = "1996-12-30", result = NA) %>%
  add_row(date_ymd = "2001-12-31", result = NA) %>% 
  add_row(date_ymd = "2002-12-30", result = NA) %>% 
  add_row(date_ymd = "2018-01-01", result = NA) %>% 
  add_row(date_ymd = "2019-01-07", result = NA) %>% 
  mutate(date_ymd = ymd(date_ymd))

# Check to  see if each date has 7 days between it and the next date
tmp <- BP_longterm_result %>% 
  mutate(Year = year(date_ymd),
         Week = week(date_ymd),
         DOY = yday(date_ymd)) %>% 
  arrange(date_ymd) %>% 
  group_by(Year) %>% 
  mutate(lag_date_ymd = lag(date_ymd, 1),
         days_bw_sampling = as.numeric(difftime(date_ymd, lag_date_ymd, unit = "days")))

unique(tmp$days_bw_sampling) # 7 days b/w each sample, and checked b/w end and start of years

tmp %>% 
  group_by(Year) %>% 
  summarise(max(Week)) 

tmp %>% 
  group_by(Year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(Year, n)) + 
  geom_col() +
  geom_hline(yintercept = 52)

tmp %>% group_by(Year) %>% summarise(mindate = min(date_ymd),
                                     maxdate = max(date_ymd)) %>% 
  filter(grepl("01-08", mindate) | grepl("12-24", maxdate))


# Just some diagnostics from the package...
md.pattern(BP_longterm_result)
aggr_plot <- aggr(BP_longterm_result, col = c('steelblue', 'red2'), 
                  numbers = TRUE, sortVars = TRUE, labels = names(data), 
                  cex.axis = .7, gap = 3, 
                  ylab = c("Histogram of missing data", "Pattern"))

marginplot(BP_longterm_result[c(1,2)], 
           ylab = "DOC concentration (mg/L)", 
           xlab = "Time (weekly observations from 1994-2019)")

# Run imputation with the default method predictive mean matching (method = 'pmm') 
# using five multiple imputations (m = 5) and 50 iterations (maxit = 50)
BP_imputed <- mice(BP_longterm_result, m = 5, maxit = 50, method = 'pmm' , seed = 500)
summary(BP_imputed)

# Results from imputation 
BP_imputed$imp$result

BP_completed1 <- complete(BP_imputed, 1)
BP_completed2 <- complete(BP_imputed, 2)
BP_completed3 <- complete(BP_imputed, 3)
BP_completed4 <- complete(BP_imputed, 4)
BP_completed5 <- complete(BP_imputed, 5)

# Looks okay but a lot of variation b/w 2001 and 2005, where most of the missing
# data points are
xyplot(BP_imputed, result ~ date_ymd, pch = 18, cex = 1)
densityplot(BP_imputed)

plot(BP_completed1)
plot(BP_completed2)
plot(BP_completed3)
plot(BP_completed4)
plot(BP_completed5)

# Compare imputations
p1 <- ggplot(BP_completed1, aes(date_ymd, result)) + geom_point(colour = "blue") 
p2 <- ggplot(BP_completed2, aes(date_ymd, result)) + geom_point(colour = "red") 
p3 <- ggplot(BP_completed3, aes(date_ymd, result)) + geom_point(colour = "green") 
p4 <- ggplot(BP_completed4, aes(date_ymd, result)) + geom_point(colour = "black") 
p5 <- ggplot(BP_completed5, aes(date_ymd, result)) + geom_point(colour = "purple") 

p1/p2/p3/p4/p5

# Incorporate imputation into long-term data set, taking the mean across the 5
# imputations for each missing value
BP_imputed_df <- BP_imputed$imp$result %>% as_tibble()

BP_imputed_df %>% 
  summarise_if(is.numeric, median) %>%
  pivot_longer(cols = 1:5, names_to = "iteration", values_to = "DOC_imputed") %>% 
  mutate_if(is.character, as.numeric) %>% 
  ggplot(aes(iteration, DOC_imputed)) + 
  geom_path() + 
  geom_point()

BP_imputed_mean <- BP_imputed_df %>% 
  rowwise() %>% 
  mutate(result_mean = mean(c(`1`:`5`))) %>% 
  select(result_mean)

BP_longterm_result_NA <- BP_longterm_result %>% filter(is.na(result))

BP_imputed_dates <- bind_cols(BP_longterm_result_NA, BP_imputed_mean) %>% 
  select(-result) %>% 
  rename(result = result_mean)

BP_longterm_cc <- BP_longterm_result %>% 
  mutate(Method = ifelse(is.na(result), "Imputed", "Observed"),
         Method = factor(Method),
         result = ifelse(is.na(result), BP_imputed_dates$result, result))

# Plot the complete data set with imputations added
p_doc_imputed <- BP_longterm_cc %>% 
  ggplot(aes(date_ymd, result, colour = Method)) + 
  geom_point() + 
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank()) +
  labs(y = "DOC concentration (mg/L)",
       subtitle = "Buffalo Pound Lake weekly DOC concentration (1994-2019)")

ggsave("./R_wavelet/outputs/figures/p_doc_imputed_20211026.png", p_doc_imputed, width = 11.28, height = 6.88)


# Combine flow and DOC dfs ------------------------------------------------

BP_doc_raw
BP_weekly_flow_raw 
# BP_longterm_cc

# 1590 rows (full record, 1990—2019)
BP_weekly_flow <- BP_weekly_flow_raw %>% filter(Year %in% c(1990:2019))

# 1564 rows, no NAs
BP_doc <- BP_doc_raw %>% 
  rename(Year = year, Week = week) %>% 
  select(date_ymd, Year, Week, DOC_mg.L)

tmp_join <- left_join(BP_weekly_flow, BP_doc) %>% 
  # filter(!is.na(result)) %>% 
  select(date_ymd, Year, Week, DOC_mg.L, everything()) %>% 
  filter(!is.na(DOC_mg.L)) 

tmp_join %>% 
  ggplot(aes(SK05JG004_combined_cms, DOC_mg.L)) + 
  geom_point()

tmp_join %>% 
  ggplot(aes(SK05JG006_cms, DOC_mg.L)) + 
  geom_point()

tmp_join %>% 
  ggplot(aes(SK05JG013_cms, DOC_mg.L)) + 
  geom_point()

tmp_join %>% 
  ggplot(aes(SK05JG014_combined_cms, DOC_mg.L)) + 
  geom_point()

p1 <- tmp_join %>% 
  ggplot(aes(date_ymd, DOC_mg.L)) + 
  geom_line() + 
  theme(axis.title.x = element_blank()) +
  labs(y = "DOC")
p2 <- tmp_join %>% # Qu'Appelle River above BPL
  ggplot(aes(date_ymd, SK05JG004_combined_cms)) + 
  geom_line() +
  theme(axis.title.x = element_blank()) +
  labs(y = "BPL inflow")
p3 <- tmp_join %>% # Diefenbaker 
  ggplot(aes(date_ymd, SK05JG006_cms)) + 
  geom_line() +
  theme(axis.title.x = element_blank()) +
  labs(y = "Diefenbaker")
p4 <- tmp_join %>% # Ridge Creek
  ggplot(aes(date_ymd, SK05JG013_cms)) + 
  geom_line() +
  theme(axis.title.x = element_blank()) +
  labs(y = "Ridge Creek")
p5 <- tmp_join %>% # Iskwao Creek
  ggplot(aes(date_ymd, SK05JG014_combined_cms)) + 
  geom_line() +
  theme(axis.title.x = element_blank()) +
  labs(y = "Iskwao Creek")
p6 <- tmp_join %>% # Iskwao Creek
  ggplot(aes(date_ymd, Ungauged_predicted_cms)) + 
  geom_line() +
  theme(axis.title.x = element_blank()) +
  labs(y = "Ungauged")

p3/p4/p5/p2/p6/p1



# Wavelet analysis --------------------------------------------------------

# Wavelet  -- BP Inflow ---------------------------------------------------
ts_times <- rep(1:length(tmp_join$DOC_mg.L))

ts_doc <- tmp_join$DOC_mg.L

# Apply wavelet transform on DOC, obtaining a class of wt. Default  parameter
# values for scale.min, scale.max.input, sigma, and f0 are usually good enough 
# for initial data exploration.
ts_doc_cleandat <- cleandat(ts_doc, ts_times, clev = 5)
# List of 3
# $ cdat      : num [1:1564] -2.97 -3.13 -3.03 -3 -2.9 ...
# $ clev      : num 1
# $ optlambdas: num NA

wtres_doc <- wt(ts_doc_cleandat$cdat, ts_times)
# List of 5
# $ values    : cplx [1:1564, 1:119] NA NA NA ...
# $ times     : int [1:1564] 1 2 3 4 5 6 7 8 9 10 ...
# $ wtopt     :List of 4
# ..$ scale.min      : num 2
# ..$ scale.max.input: NULL
# ..$ sigma          : num 1.05
# ..$ f0             : num 1
# $ timescales: num [1:119] 2 2.1 2.21 2.32 2.43 ...
# $ dat       : num [1:1564] -2.97 -3.13 -3.03 -3 -2.9 ...
# - attr(*, "class")= chr [1:3] "wt" "tts" "list"
class(wtres_doc)
# [1] "wt"   "tts"  "list"
names(wtres_doc)
# [1] "values"     "times"      "wtopt"      "timescales" "dat"

# Use wsyn::plotmag with the tts class to plots the magnitude of the transform 
# against time and timescale:
plotmag(wtres_doc)
plotphase(wtres_doc)

# One can compute the power:
h_doc <- power(wtres_doc)
head(h_doc)
str(h_doc)
tail(h_doc)
#   timescales     power
# 1   2.000000 0.2734354
# 2   2.100000 0.2728805
# 3   2.205000 0.2704232
# 4   2.315250 0.2678656
# 5   2.431013 0.2677021
# 6   2.552563 0.2717357

plot(x = log(1/h_doc$timescales), y = h_doc$power, type = "l", 
     lty = "solid", xaxt = "n", 
     xlab = "Timescales", ylab = "Power")
xlocs <- c(min(h_doc$timescales), pretty(h_doc$timescales,n = 100))
graphics::axis(side = 1, at = log(1/xlocs), labels = xlocs)


print(wtres_doc)
# wt object:
# times, a length 1564 numeric vector:
# 1 2 3 4 5 ... 1560 1561 1562 1563 1564 
# timescales, a length 119 numeric vector:
# 2 2.1 2.205 2.31525 2.4310125 ... 520.726991551851 546.763341129443 574.101508185916 602.806583595211 632.946912774972 
# values, a 1564 by 119 matrix, upper left to four digits is:
#            [,1]            [,2]            [,3]            [,4]            [,5]
# [1,]         NA              NA              NA              NA              NA
# [2,]         NA              NA              NA              NA              NA
# [3,]         NA              NA              NA              NA              NA
# [4,]  0.3176+0i  0.3092-0.0191i  0.1860-0.0363i -0.0246-0.0431i -0.2704-0.0339i
# [5,] -0.3874+0i -0.3772-0.0881i -0.2751-0.1691i -0.1041-0.2521i  0.0887-0.3406i
# wtopt: scale.min=2; scale.max.input=NULL; sigma=1.05; f0=1

summary(wtres_doc)
# class: wt
# times_start: 1
# times_end: 1564
# times_increment: 1
# timescale_start: 2
# timescale_end: 632.9469
# timescale_length: 119
# scale.min: 2
# scale.max.input: NULL
# sigma: 1.05
# f0: 1

### Now work on coherence b/w DOC and BP inflow using wsyn::coh
### clev = 1 cleans time series with mean removal only
ts_BP_inflow <- tmp_join$SK05JG004_combined_cms

ts_doc_cleandat <- cleandat(ts_doc, ts_times, clev = 5)$cdat
ts_BPinflow_cleandat <- cleandat(ts_BP_inflow, ts_times, clev = 5)$cdat

# norm = c("phase", "powind", "powall", "none")
# res_doc_BPinflow <- wsyn::coh(dat1 = ts_BPinflow_cleandat, dat2 = ts_doc_cleandat, times = ts_times,
#                               norm = "powall", sigmethod = "fftsurrog1", nrand = 1000, f0 = 0.5,
#                               scale.max.input = 521)
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
# which corresponds to (spatial) wavelet coherence. There are several methods
# for testing the significance of coherence, and the method used is controlled
# by the sigmethod argument. All significance methods are based on "surrogate
# datasets." These are datasets that have been normalized in an appropriate
# way. See section 5 for details of surrogates and significance testing, and
# allowed values of sigmethod.
# 
# Larger values of nrand produce more accurate significance results that are
# less variable on repeat runs, but also require more computational time.
# Using nrand at least 1000 or 10000 for final runs is recommended. 
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
res_doc_BPinflow <- coh(dat1 = ts_BPinflow_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                        norm = "phase", sigmethod = "fftsurrog1", nrand = 1000,
                        f0 = 0.5)

plotmag(res_doc_BPinflow, neat = FALSE)
graphics::axis(side = 1, at = log(1/xlocs), labels = FALSE, tick = FALSE)
xlocs <- c(min(res_doc_BPinflow$timescales), pretty(res_doc_BPinflow$timescales, n = 80))
graphics::axis(side = 1, at = log(1/xlocs), labels = xlocs)

res_doc_BPinflow$timescales %>% as_tibble() %>% View()
res_doc_BPinflow$coher %>% as_tibble() %>% View()
Mod(res_doc_BPinflow$coher) %>% as_tibble() %>% View()

coh_df <- tibble(Timescales = res_doc_BPinflow$timescales,
                 Coherence = Mod(res_doc_BPinflow$coher))

coh_df %>% 
  ggplot(aes(Timescales, Coherence)) + 
  geom_line(col = "red") +
  scale_x_reverse() 

res_doc_BPinflow_bandtest <- bandtest(res_doc_BPinflow, c(7, 11))
# coh object:
# times, a length 1564 numeric vector:
# 1 2 3 4 5 ... 1560 1561 1562 1563 1564 
# Number of sampling locations: 
# timescales, a length 56 numeric vector:
# 2 2.1 2.205 2.31525 2.4310125 ... 24.0815395500828 25.2856165275869 26.5498973539663 27.8773922216646 29.2712618327478 
# norm, the normalization used: powall 
# wtopt: scale.min=2; scale.max.input=28; sigma=1.05; f0=0.5
# sigmethod, the type of significance testing used: fast 
# Number of surrogates: 10000 
# The ranks slot is: filled
# Timescale bands tested in bandp slot:
#   ts_low_bd ts_hi_bd
# 1         7       11

get_bandp(res_doc_BPinflow_bandtest)
#   ts_low_bd ts_hi_bd      p_val   mn_phs
# 1         7       11 0.08979102 2.480204

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






# Wavelet -- Diefenbaker --------------------------------------------------
ts_times <- rep(1:length(tmp_join$DOC_mg.L))

ts_doc <- tmp_join$DOC_mg.L

ts_doc_cleandat <- cleandat(ts_doc, ts_times, clev = 1)

wtres_doc <- wt(ts_doc_cleandat$cdat, ts_times)

ts_diefenbaker <- tmp_join$SK05JG006_cms

ts_doc_cleandat <- cleandat(ts_doc, ts_times, clev = 1)$cdat
ts_diefenbaker_cleandat <- cleandat(ts_diefenbaker, ts_times, clev = 1)$cdat

# norm = c("phase", "powind", "powall", "none")
res_doc_diefenbaker <- wsyn::coh(dat1 = ts_diefenbaker_cleandat, dat2 = ts_doc_cleandat, times = ts_times,
                                 norm = "powall", sigmethod = "fftsurrog1", nrand = 1000, f0 = 0.5,
                                 scale.max.input = 28)

plotmag(res_doc_diefenbaker)




# Wavelet -- BP Inflow (2010—2019) ----------------------------------------

# Running analysis on smaller subset of time series to try to figure out how
# timescale is calculated and displayed... So far all I can tell is that the
# equation for timescale is log frequency, i.e. timescale = log(1/x) but still 
# unsure of what x is in the equation.

# The arguments scale.min, scale.max.input, sigma, and f0, which are arguments
# to wt and several other functions, are for constructing the timescales used
# for wavelet analysis. 
# 
# The argument scale.min is the shortest timescale, and must be 2 or greater. 
# Starting from scale.min, each timescale is sigma times the previous one, up to 
# the first timescale that equals or surpasses scale.max.input.
# 
# The scalloping of wavelet transforms places additional, independently
# implemented constraints on the largest timescale examined so choosing larger 
# scale.max.input will only result in longer timescales up to the limits imposed 
# by scalloping. 
# 
# The argument f0 is the ratio of the period of fluctuation to the width of the 
# envelope. Higher values of f0 imply higher frequency resolution (i.e., a 
# wavelet component includes information from a narrower range of Fourier 
# components) but lower temporal resolution (the component includes information 
# from a wider range of times). 
# Resolution should be chosen appropriate to the characteristics of the data 
# (Addison 2002).
# 
# wsyn::coh() arguments:
# scale.min: The smallest scale of fluctuation that will be examined. At least 2.
# scale.max.input: The largest scale of fluctuation guaranteed to be examined.
# sigma: The ratio of each time scale examined relative to the next timescale. Should be greater than 1.
# f0: The ratio of the period of fluctuation to the width of the envelope.
# 
# recommendatons from wsyn vignette...
# scale.min = 2
# scale.max.imput = 28
# sigma = 1.05
# f0 = 0.5

# try...
# scale.min = 1/7 = 0.1428571 -- time unit is 1 observation per week
# scale.max.inut = floor(nrow(time_series_length)/3)*scale.min = 74.42857 -- floor of 1/3 the length of time series time scale.min

# floor(nrow(my.data)/3)*dt
dt = 1/7
my.data <- tibble(x = 1:1564)
floor(nrow(my.data)/3)*dt


BP_weekly_flow_5year <- BP_weekly_flow_raw %>% filter(Year %in% c(2015:2019))

BP_doc_5year <- BP_doc_raw %>% 
  rename(Year = year, Week = week) %>% 
  filter(Year %in% c(2010:2019)) %>% 
  select(date_ymd, Year, Week, DOC_mg.L)

tmp_join_5year <- left_join(BP_weekly_flow_5year, BP_doc_5year) %>% 
  # filter(!is.na(result)) %>% 
  select(date_ymd, Year, Week, DOC_mg.L, everything()) %>% 
  filter(!is.na(DOC_mg.L)) 

ts_times_5year <- rep(1:length(tmp_join_5year$DOC_mg.L))

ts_doc_5year <- tmp_join_5year$DOC_mg.L

ts_doc_cleandat_5year <- cleandat(ts_doc_5year, ts_times_5year, clev = 1)

wtres_doc_5year <- wt(ts_doc_cleandat_5year$cdat, ts_times_5year)

ts_BP_inflow_5year <- tmp_join_5year$SK05JG006_cms

ts_doc_cleandat_5year <- cleandat(ts_doc_5year, ts_times_5year, clev = 1)$cdat
ts_BP_inflow_cleandat_5year <- cleandat(ts_BP_inflow_5year, ts_times_5year, clev = 1)$cdat

# norm = c("phase", "powind", "powall", "none")
res_doc_BP_inflow_5year <- wsyn::coh(dat1 = ts_BP_inflow_cleandat_5year,
                                      dat2 = ts_doc_cleandat_5year, 
                                      times = ts_times_5year,
                                      norm = "powall", sigmethod = "fftsurrog1",
                                      nrand = 1000, f0 = 0.5,
                                      scale.max.input = 30)

# timescales is a numeric vector of length 56 (2 times scale.max.input) ranging
# from 2.000000 to 29.271262
str(res_doc_BP_inflow_5year$timescales)
length(res_doc_BP_inflow_5year$timescales)

plotmag(res_doc_BP_inflow_5year, sigthresh = c(0.95, 0.99))




# Wavelet -- BP Inflow (annual means) -------------------------------------

tmp_join_annual <- tmp_join %>% 
  select(-Week) %>% 
  group_by(Year) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

ts_doc <- tmp_join_annual$DOC_mg.L

ts_times <- rep(1:length(ts_doc))

# Apply wavelet transform on DOC, obtaining a class of wt. Default  parameter
# values for scale.min, scale.max.input, sigma, and f0 are usually good enough 
# for initial data exploration.
ts_doc_cleandat <- cleandat(ts_doc, ts_times, clev = 5)
# List of 3
# $ cdat      : num [1:30] -2.5642 -0.385 -0.6058 -0.7048 0.0385 ...
# $ clev      : num 5
# $ optlambdas: num 0.57

wtres_doc <- wt(ts_doc_cleandat$cdat, ts_times)
# List of 5
# $ values    : cplx [1:30, 1:37] NA NA NA ...
# $ times     : int [1:30] 1 2 3 4 5 6 7 8 9 10 ...
# $ wtopt     :List of 4
# ..$ scale.min      : num 2
# ..$ scale.max.input: NULL
# ..$ sigma          : num 1.05
# ..$ f0             : num 1
# $ timescales: num [1:37] 2 2.1 2.21 2.32 2.43 ...
# $ dat       : num [1:30] -2.5642 -0.385 -0.6058 -0.7048 0.0385 ...
# - attr(*, "class")= chr [1:3] "wt" "tts" "list"
class(wtres_doc)
# [1] "wt"   "tts"  "list"
names(wtres_doc)
# [1] "values"     "times"      "wtopt"      "timescales" "dat"

# Use wsyn::plotmag with the tts class to plots the magnitude of the transform 
# against time and timescale:
plotmag(wtres_doc)
plotphase(wtres_doc)

# One can compute the power:
h_doc <- power(wtres_doc)
head(h_doc)
tail(h_doc)
str(h_doc)
#    timescales     power
# 1    2.000000 0.2734354
# 2    2.100000 0.2728805
# 3    2.205000 0.2704232
# 4    2.315250 0.2678656
# 5    2.431013 0.2677021
# 6    2.552563 0.2717357
# ...
# 32   9.076079 2.2634235
# 33   9.529883 1.7815116
# 34  10.006377 1.1529078
# 35  10.506696 0.6136462
# 36  11.032031 0.5893122
# 37  11.583632 1.2599852
# 
# 'data.frame':	37 obs. of  2 variables:
# $ timescales: num  2 2.1 2.21 2.32 2.43 ...
# $ power     : num  0.147 0.146 0.135 0.133 0.163 ...

plot(x = log(1/h_doc$timescales), y = h_doc$power, type = "l", 
     lty = "solid", xaxt = "n", 
     xlab = "Timescales", ylab = "Power")
xlocs <- c(min(h_doc$timescales), pretty(h_doc$timescales,n = 20))
graphics::axis(side = 1, at = log(1/xlocs), labels = xlocs)


print(wtres_doc)
# wt object:
#   times, a length 30 numeric vector:
#   1 2 3 4 5 ... 26 27 28 29 30 
# timescales, a length 37 numeric vector:
#   2 2.1 2.205 2.31525 2.4310125 ... 9.52988293720722 10.0063770840676 10.506695938271 11.0320307351845 11.5836322719437 
# values, a 30 by 37 matrix, upper left to four digits is:
#   [,1]            [,2]            [,3]            [,4]            [,5]
# [1,]         NA              NA              NA              NA              NA
# [2,]         NA              NA              NA              NA              NA
# [3,]         NA              NA              NA              NA              NA
# [4,]  0.3919+0i  0.3820-0.2257i  0.2198-0.4299i -0.0593-0.5880i -0.3931-0.6843i
# [5,] -0.1860+0i -0.1803+0.0351i -0.0315+0.0649i  0.2257+0.0562i  0.5220-0.0093i
# wtopt: scale.min=2; scale.max.input=NULL; sigma=1.05; f0=1

summary(wtres_doc)
# class: wt
# times_start: 1
# times_end: 30
# times_increment: 1
# timescale_start: 2
# timescale_end: 11.58363
# timescale_length: 37
# scale.min: 2
# scale.max.input: NULL
# sigma: 1.05
# f0: 1

### Now work on coherence b/w DOC and BP inflow using wsyn::coh
### clev = 1 cleans time series with mean removal only
ts_BP_inflow <- tmp_join_annual$SK05JG004_combined_cms

ts_doc_cleandat <- cleandat(ts_doc, ts_times, clev = 5)$cdat
ts_BPinflow_cleandat <- cleandat(ts_BP_inflow, ts_times, clev = 5)$cdat

# norm = c("phase", "powind", "powall", "none")
res_doc_BPinflow <- wsyn::coh(dat1 = ts_BPinflow_cleandat, dat2 = ts_doc_cleandat, times = ts_times,
                              norm = "phase", sigmethod = "fftsurrog1", nrand = 1000, f0 = 0.5)
# coh object:
#   times, a length 30 numeric vector:
#   1 2 3 4 5 ... 26 27 28 29 30 
# Number of sampling locations: 
#   timescales, a length 51 numeric vector:
#   2 2.1 2.205 2.31525 2.4310125 ... 18.8685163663349 19.8119421846517 20.8025392938843 21.8426662585785 22.9347995715074 
# norm, the normalization used: phase
# wtopt: scale.min=2; scale.max.input=NULL; sigma=1.05; f0=0.5
# sigmethod, the type of significance testing used: fftsurrog1 
# Number of surrogates: 1000 
# The ranks slot is: empty
# Timescale bands tested in bandp slot: none

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
res_doc_BPinflow <- coh(dat1 = ts_BPinflow_cleandat, dat2 = ts_doc_cleandat, times = ts_times, 
                        norm = "phase", sigmethod = "fftsurrog1", nrand = 10000,
                        f0 = 0.5)

plotmag(res_doc_BPinflow)
plotphase(res_doc_BPinflow)




