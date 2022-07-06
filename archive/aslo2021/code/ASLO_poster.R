# ASLO2021 Virtual Meeting poster
# Submission deadline: Friday, June 18, 2021
# Anthony Baron
#
# Changing DOM in a crucial drinking water supply reservoir: Understanding
# drivers of change in a semi-arid region subject to high climatic variability


# Packages, plot theme ----------------------------------------------------

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(readr, dplyr, tidyr, purrr, lubridate, ggplot2, patchwork,
       mgcv, gratia)

# set theme for plotting
ggplot2::theme_set(theme_bw(base_size = 12))


# Read in data ------------------------------------------------------------

bp_longterm <- read_csv("./anthony_masters/plsc-837/data/bp_longterm.csv")

bp_longterm <- bp_longterm %>% mutate(Year = factor(year)) 


# Data processing ---------------------------------------------------------

# subset() start date of DOC measurements
bp_83 <- bp_longterm %>% subset(date_ymd >= "1983-05-09")

# Get annual means of each variable
bp_means <- bp_83 %>% 
  select(-DOY) %>% 
  group_by(Year) %>% 
  summarise_if(is.numeric, list(~ mean(., na.rm = TRUE))) %>% 
  mutate(Year = as.numeric(as.character(Year))) 

bp_monthly <- bp_83 %>% 
  mutate(month = month(date_ymd)) %>% 
  group_by(year, month) %>% 
  summarise_at(c("DOC", "pH", "sulphate", "chl_a", "SRP", "colour"), 
               mean, na.rm = TRUE) %>% 
  mutate(day = 1,
         chl_a_log = log(chl_a + 1),
         colour_log = log(colour + 1),
         SRP_log = log(SRP + 1),
         sulphate_log = log(sulphate + 1)) %>%
  unite("date_ymd", c(year, month, day), sep = "-", remove = FALSE) %>% 
  mutate(date_ymd = ymd(date_ymd)) %>% 
  select(date_ymd, year, month, day, everything()) %>% 
  ungroup()


# Plot labels -------------------------------------------------------------

DOC_lab <- expression(paste("DOC concentration (mg L"^-1*")")) 
pH_lab <- "pH"
Chla_lab <- expression(paste("Chl ",italic("a "), "(",italic("µ"),"g ","L"^"–1",")"))
colour_lab <- expression(paste("Colour (Pt/Co)")) 
sulphate_lab <- expression(paste("SO"["4"]^"2–  ", "(mg"," L"^"–1",")"))
SRP_lab <- expression(paste("SRP ", "(",italic("µ"),"g L"^-1*")")) 

Chla_subtitle <- expression(paste(bold("Chlorophyll "), bolditalic("a")))



# Time series plots -------------------------------------------------------

p_doc_ts <- bp_means %>% 
  ggplot(aes(Year, DOC)) +
  geom_line(colour = "steelblue") +
  geom_point(colour = "white", size = 2.75) +
  geom_point(colour = "steelblue", size = 2) +
  theme(axis.title.x = element_blank()) +
  labs(y = DOC_lab, subtitle = expression(bold("Dissolved organic carbon")))
ggsave("./anthony_masters/outputs/ASLO/20210616_p_doc.png", p_doc_ts, w = 50, h = 16.5, units = 'cm')

p_pH_ts <- bp_means %>% 
  ggplot(aes(Year, pH)) +
  geom_line(colour = "steelblue") +
  geom_point(colour = "white", size = 2.25) +
  geom_point(colour = "steelblue", size = 1.75, alpha = 0.8) +
  theme(axis.title.x = element_blank()) +
  labs(y = pH_lab, subtitle = expression(bold("pH"))) 

p_colour_ts <- bp_means %>% 
  ggplot(aes(Year, colour)) +
  geom_line(colour = "steelblue") +
  geom_point(colour = "white", size = 2.25) +
  geom_point(colour = "steelblue", size = 1.75, alpha = 0.8) +
  theme(axis.title.x = element_blank()) +
  labs(y = colour_lab, subtitle = expression(bold("Colour")))

p_sulphate_ts <- bp_means %>% 
  ggplot(aes(Year, sulphate)) +
  geom_line(colour = "steelblue") +
  geom_point(colour = "white", size = 2.5) +
  geom_point(colour = "steelblue", size = 1.75, alpha = 0.8) +
  theme(axis.title.x = element_blank()) +
  labs(y = sulphate_lab, subtitle = expression(bold("Sulphate")))

p_chla_ts <- bp_means %>% 
  ggplot(aes(Year, chl_a)) +
  geom_line(colour = "steelblue") +
  geom_point(colour = "white", size = 2.5) +
  geom_point(colour = "steelblue", size = 1.75, alpha = 0.8) +
  theme(axis.title.x = element_blank()) +
  labs(y = Chla_lab, subtitle = Chla_subtitle)

p_SRP_ts <- bp_means %>% 
  ggplot(aes(Year, SRP)) +
  geom_line(colour = "steelblue") +
  geom_point(colour = "white", size = 2.5) +
  geom_point(colour = "steelblue", size = 1.75, alpha = 0.8) +
  theme(axis.title.x = element_blank()) +
  labs(y = SRP_lab, subtitle = expression(bold("Soluble reactive phosphorus")))

p_ts <- p_pH_ts/p_sulphate_ts/p_colour_ts/p_chla_ts/p_SRP_ts
ggsave("./anthony_masters/outputs/ASLO/20210616_p_ts.png", p_ts, w = 5.62, h = 8.27)

p_ts2 <- p_chla_ts/p_SRP_ts/p_sulphate_ts
ggsave("./anthony_masters/outputs/ASLO/20210617_p_ts2.png", p_ts2, w = 6.29, h = 6.82)



# DOC time series GAM -----------------------------------------------------

doc_gam1 <- gamm(DOC ~ s(Year, k = 36), 
                method = 'REML', data = bp_means, family = tw)
                
summary(doc_gam1$gam)
draw(doc_gam1$gam, residuals = TRUE)
appraise(doc_gam1$gam)
gam.check(doc_gam1$gam)
anova(doc_gam1$gam)



# DOC with explanatory variables GAM --------------------------------------

bp_means %>%
  select(-c(Year, year, alkalinity, conductivity, hardness, SUVA, temperature, turbidity, TP)) %>% 
  mutate(SRP_log = log(SRP + 1),
         sulphate_log = log(sulphate + 1)) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

bp_monthly %>%
  select(-c(date_ymd, year, month, day)) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

doc_gam2 <- gamm(DOC ~ s(pH) + s(sulphate_log) + s(colour_log) + s(chl_a_log) + 
                  s(SRP_log) + s(month, bs = 'cc', k = 12),
                method = 'REML', data = bp_monthly)

summary(doc_gam2)
draw(doc_gam2$gam, residuals = TRUE)
appraise(doc_gam2$gam)
gam.check(doc_gam2$gam)

layout(matrix(1:2, ncol = 2))
acf(resid(doc_gam2$lme), lag.max = 36, main = "ACF")
pacf(resid(doc_gam2$lme), lag.max = 36, main = "pACF")

# For the AR models (AR(1), AR(2), AR(3)) below, the 'correlation' argument is
# fitting an autoregressive moving average (ARMA) process to the residuals,
# where 'p' indicates the order for the AR part of the ARMA model, and 
# 'form = ~ 1|Year' means that the ARMA is nested with each year.

# AR(1)
mAR1 <- gamm(DOC ~ s(pH) + s(sulphate_log) + s(colour_log, k = 20) + s(chl_a_log) + 
               s(SRP_log) + s(month, bs = 'cc', k = 12),
             method = 'REML', data = bp_monthly,
             correlation = corARMA(form = ~ 1|year, p = 1))

# AR(2)
mAR2 <- gamm(DOC ~ s(pH) + s(sulphate_log) + s(colour_log) + s(chl_a_log) + 
               s(SRP_log) + s(month, bs = 'cc', k = 12),
             method = 'REML', data = bp_monthly,
             correlation = corARMA(form = ~ 1|year, p = 2))

# AR(3)
mAR3 <- gamm(DOC ~ s(pH) + s(sulphate_log) + s(colour_log) + s(chl_a_log) + 
               s(SRP_log) + s(month, bs = 'cc', k = 12),
             method = 'REML', data = bp_monthly,
             correlation = corARMA(form = ~ 1|year, p = 3))

anova(doc_gam2$lme, mAR1$lme, mAR2$lme, mAR3$lme) # mAR1 best

summary(mAR1$gam) # R2adj = 0.284
draw(mAR1$gam, residuals = TRUE) 
appraise(mAR1$gam, point_col = "steelblue", point_alpha = 0.4)
anova(mAR1$gam)
gam.check(mAR1$gam)



# Another GAM -------------------------------------------------------------

bp_gam3_data <- bp_means %>% 
  mutate(SRP_log = log(SRP + 1),
         sulphate_log = log(sulphate + 1)) %>% 
  rename(Chlorophyll_a = chl_a,
         Sulphate = sulphate)

doc_gam3 <- gam(DOC ~ s(Chlorophyll_a) + s(SRP_log) + s(sulphate_log), 
                data = bp_gam3_data, method = 'REML', family = tw)
summary(doc_gam3)
draw(doc_gam3, nrow = 1, scales = "fixed")
appraise(doc_gam3)



doc_gam4 <- gam(DOC ~ s(Chlorophyll_a) + s(SRP) + s(Sulphate), 
                data = bp_gam3_data, method = 'REML', family = tw)
summary(doc_gam4)
p_doc_gam4 <- draw(doc_gam4, ncol = 1, scales = "fixed") 
appraise(doc_gam4)
anova(doc_gam4)
gam.check(doc_gam4)
ggsave("./anthony_masters/outputs/ASLO/20210617_p_doc_gam4.png", p_doc_gam4, w = 10.72, h = 3.85)

p_ts2 | p_doc_gam4

# Correlations ------------------------------------------------------------

cor.test(bp_means$chl_a, bp_means$DOC, method = 'pearson') # r = 0.67, p < 0.0001
cor.test(bp_means$SRP, bp_means$DOC, method = 'pearson') # r = 0.13, p = 0.45
cor.test(bp_means$sulphate, bp_means$DOC, method = 'pearson') # r = 0.52, p = 0.0011  


# Summary stats -----------------------------------------------------------

bp_means %>% 
  summarise(DOC_mean = mean(DOC, na.rm = TRUE),
            DOC_sd = sd(DOC, na.rm = TRUE),
            DOC_min = min(DOC, na.rm = TRUE),
            DOC_max = max(DOC, na.rm = TRUE))

bp_means %>% 
  summarise(chl_a_mean = mean(chl_a, na.rm = TRUE),
            chl_a_sd = sd(chl_a, na.rm = TRUE),
            chl_a_min = min(chl_a, na.rm = TRUE),
            chl_a_max = max(chl_a, na.rm = TRUE))

bp_means %>% 
  summarise(SRP_mean = mean(SRP, na.rm = TRUE),
            SRP_sd = sd(SRP, na.rm = TRUE),
            SRP_min = min(SRP, na.rm = TRUE),
            SRP_max = max(SRP, na.rm = TRUE))

bp_means %>% 
  summarise(sulphate_mean = mean(sulphate, na.rm = TRUE),
            sulphate_sd = sd(sulphate, na.rm = TRUE),
            sulphate_min = min(sulphate, na.rm = TRUE),
            sulphate_max = max(sulphate, na.rm = TRUE))
