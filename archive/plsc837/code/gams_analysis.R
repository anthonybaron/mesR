# Anthony Baron
# PLSC-837 term paper analysis
# 2021-06-15

# Packages, plot theme ----------------------------------------------------
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(readr, dplyr, tidyr, purrr, lubridate, ggplot2, viridis, patchwork,
       mgcv, gratia)

# set theme for plotting
ggplot2::theme_set(theme_bw(base_size = 12))



# Read in data ------------------------------------------------------------
bp_longterm <- read_csv("./anthony_masters/plsc-837/data/bp_longterm.csv")

bp_longterm <- bp_longterm %>% mutate(year = factor(year)) 



# Explore data set --------------------------------------------------------

pairs(bp_longterm[4:11]) 
# Sulphate and conductivity are highly correlated so will remove conductivity 
# because sulphate is of greater interest.
# TP and SRP are also highly correlated so will keep SRP because TP and Chl a
# correlate.

bp_longterm <- bp_longterm %>% select(-c(conductivity, TP))

summary(bp_longterm)
# — 2107 potential observations for each parameter
# — Missing data: DOC = 524 NAs (25%),
#                 pH = 47 NAs (2.2%)
#                 sulphate = 721 NAs (34%),
#                 Chl a = 912 NAs (42%),
#                 SRP = 882 NAs (43%),
#                 colour = 715 NAs (34%)
#   A lot of missing data results from weekly vs. monthly sampling:
#   Weekly measurements: DOC, pH, Chl a
#   Monthly measurements: sulphate, SRP, colour

# Here I'll average the weekly measurements for each month and add a date entry 
# as the first of each month
bp_monthly <- bp_longterm %>% 
  mutate(month = month(date_ymd)) %>% 
  group_by(year, month) %>% 
  summarise_at(c("DOC", "pH", "sulphate", "chl_a", "SRP", "colour"), 
               mean, na.rm = TRUE) %>% 
  mutate(day = 1) %>%
  unite("date_ymd", c(year, month, day), sep = "-", remove = FALSE) %>% 
  mutate(date_ymd = ymd(date_ymd)) %>% 
  select(date_ymd, year, month, day, everything()) %>% 
  ungroup()


# Check number of missing values again
summary(bp_monthly)
# DOC = 97 NAs out of 487 potential obesrvations (20%),
# pH = 5 NAs (1.0%),
# sulphate = 6 NAs (1.2%), 
# Chl a = 36 NAs (7.4%),
# SRP = 37 NAs (7.4%),
# colour = 12 NAs (2.5%)

# Still a lot of missing DOC values. Check out DOC time series plot.
# The plot shows DOC measurements begin a year or two after 1980 and a gap in
# the early 1990s. 
bp_monthly %>% 
  ggplot(aes(date_ymd, DOC)) + 
  geom_point()

# When do DOC measurements start? 
bp_monthly %>% filter(!is.na(DOC)) 
# The first DOC measurements are from May 1983, whereas most of the measurements 
# of other variables begin January 1980. 

# Subset for start date of DOC measurements.
bp_monthly %>%
  select(date_ymd, DOC) %>% 
  filter(!is.na(DOC)) %>% 
  summarise(min(date_ymd, na.rm = TRUE)) # 1983-05-01
  
bp_83 <- bp_monthly %>% subset(date_ymd >= "1983-05-01")

summary(bp_83)
# Now for DOC there are only 57 NAs out odf 447 potential observations (~13%). 
# This is a much more acceptable number of missing values. 

# Check pairs again...
pairs(bp_83[5:11])
# It  looks like DOC correlates positively to varying degrees with all
# explanatory variables. What are the correlation strengths and p-values? And do
# any variables need transformation at this point?

# Check distributions of variables
bp_83 %>%
  select(-c(date_ymd:day)) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# Chl a, colour, sulphate, and SRP might benefit from transformation
bp_transform <- bp_83 %>% 
  mutate(chl_a_log = log(chl_a + 1),
         colour_log = log(colour + 1),
         sulphate_log = log(sulphate + 1), 
         SRP_log = log(SRP + 1)) 
  
bp_transform %>% 
  select(-c(date_ymd:day)) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

# Chl a, colour, and SRP look like they approach a normal distribution after 
# log-transformation but sulphate still has a long tail. 


# Now check correlations
cor.test(bp_transform$pH, bp_transform$DOC, method = "pearson") # r = 0.14, p = 0.007205
cor.test(bp_transform$chl_a_log, bp_transform$DOC, method = "pearson") # r = 0.36, p = 3.306e-13
cor.test(bp_transform$colour_log, bp_transform$DOC, method = "pearson") # r = 0.49, p < 2.2e-16
cor.test(bp_transform$sulphate_log, bp_transform$DOC, method = "pearson") # r = 0.49, p < 2.2e-16 
cor.test(bp_transform$SRP_log, bp_transform$DOC, method = "pearson") # r = 0.16, p = 0.001624

# All significantly positively correlated, albeit weakly in some cases... 



# Plot time series --------------------------------------------------------

DOC_lab <- expression(paste("DOC  (mg L"^-1*")")) 
pH_lab <- "pH"
Chla_lab <- expression(paste("Chl a (µg L"^-1*")")) 
colour_lab <- expression(paste("Colour (Pt Co"^-1*")")) 
sulphate_lab <- expression(paste("SO" ["4"] ^" 2–", " (mg", " L" ^"–1", ")"))
SRP_lab <- expression(paste("SRP (µg L"^-1*")")) 

theme_set(theme_bw(base_size = 12))
p_DOC <- bp_83 %>% ggplot(aes(date_ymd, DOC)) + geom_point(size = 0.5, colour = "steelblue") + labs(x = "", y = DOC_lab)
p_pH <- bp_83 %>% ggplot(aes(date_ymd, pH)) + geom_point(size = 0.5, colour = "steelblue") + labs(x = "", y = pH_lab)
p_chla <- bp_83 %>% ggplot(aes(date_ymd, chl_a)) + geom_point(size = 0.5, colour = "steelblue") + labs(x = "", y = Chla_lab)
p_colour <- bp_83 %>% ggplot(aes(date_ymd, colour)) + geom_point(size = 0.5, colour = "steelblue") + labs(x = "", y = colour_lab)
p_sulphate <- bp_83 %>% ggplot(aes(date_ymd, sulphate)) + geom_point(size = 0.5, colour = "steelblue") + labs(x = "", y = sulphate_lab)
p_TP <- bp_83 %>% ggplot(aes(date_ymd, SRP)) + geom_point(size = 0.5, colour = "steelblue") + labs(x = "", y = SRP_lab)

p_ts <- p_DOC/p_pH/p_chla/p_colour/p_sulphate/p_TP 
p_ts



# Data preparation for modeling -------------------------------------------

# Here, it is useful to make two changes for the time components of the data set:
# 1. Convert date of observation to a numeric variable for the year data using
#    R's Date class. This class counts the number of days since Jan 1st, 1970.
#    These numbers can get large so I will divide by 1000. The variable is 
#    labelled 'Time'. 

bp_gamdata <- bp_transform %>% 
  mutate(Time = as.numeric(date_ymd) / 1000) %>% 
  select(Time, everything()) %>% 
  mutate(year = as.character(year),
         year = as.integer(year),
         month = as.integer(month)) %>% 
  select(-day)

str(bp_gamdata)



# Model 1 -------------------------------------------------

# model1 is the maximal model (DOC with all explanatory variables), and since we
# are dealing with time series, the time components that affect the trends 
# (e.g., seasonality) also need to be accounted for, so month with a cyclic cubic
# smoother and 12 knots (k = 12, one knot for each month) and year will also be 
# estimated/added to the model. 

model1 <- gamm(DOC ~ s(pH) + s(chl_a_log) + s(colour_log) + s(sulphate_log) + s(SRP_log) +
                s(month, bs = 'cc', k = 12) + s(Time, k = 20),
              method = 'REML', data = bp_gamdata)

summary(model1$gam)
draw(model1$gam, residuals = TRUE) 
appraise(model1$gam)

layout(matrix(1:2, ncol = 2))
acf(resid(model1$lme), lag.max = 36, main = "ACF")
pacf(resid(model1$lme), lag.max = 36, main = "pACF")

# Highly autocorrelated errors... Low-order autoregressive (AR) model is needed.


# Autoregressive models ---------------------------------------------------

# For the AR models (AR(1), AR(2), AR(3)) below, the 'correlation' argument is
# fitting an autoregressive moving average (ARMA) process to the residuals,
# where 'p' indicates the order for the AR part of the ARMA model, and 
# 'form = ~ 1|Year' means that the ARMA is nested with each year.

# AR(1)
mAR1 <- gamm(DOC ~ s(pH) + s(chl_a_log) + s(colour_log) + s(sulphate_log) + s(SRP_log) +
      s(month, bs = 'cc', k = 12) + s(Time, k = 20),
    method = 'REML', data = bp_gamdata,
    correlation = corARMA(form = ~ 1|year, p = 1))

# AR(2)
mAR2 <- gamm(DOC ~ s(pH) + s(chl_a_log) + s(colour_log) + s(sulphate_log) + s(SRP_log) +
            s(month, bs = 'cc', k = 12) + s(Time, k = 20),
          method = 'REML', data = bp_gamdata,
          correlation = corARMA(form = ~ 1|year, p = 2))

# AR(3)
mAR3 <- gamm(DOC ~ s(pH) + s(chl_a_log) + s(colour_log) + s(sulphate_log) + s(SRP_log) +
            s(month, bs = 'cc', k = 12) + s(Time, k = 120),
          method = 'REML', data = bp_gamdata,
          correlation = corARMA(form = ~ 1|year, p = 3))


# To see which model fits the data best, use generalized likelihood ratio test
# via the anova() method for 'lme' objects.
anova(model1$lme, mAR1$lme, mAR2$lme, mAR3$lme)
# Based on the output (AIC, BIC, and p-values), it looks like AR(1) provides the
# best fit and greatest improvement of the three AR models.

summary(mAR1$gam) # R2adj = 0.810
draw(mAR1$gam, residuals = TRUE) 
appraise(mAR1$gam, point_col = "steelblue", point_alpha = 0.4)
anova(mAR1$gam)
gam.check(mAR1$gam)

layout(matrix(1:2, ncol = 2))
res <- resid(mAR1$lme, type = "normalized")
acf(res, lag.max = 36, main = "ACF - AR(3) errors")
pacf(res, lag.max = 36, main = "pACF - AR(3) errors")

