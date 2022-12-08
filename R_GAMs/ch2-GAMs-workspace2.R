library(mgcv)
library(gratia)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)
library(viridis)
library(patchwork)

theme_set(theme_bw(base_size = 12) + theme(panel.grid = element_blank()))
DOC_lab <- expression(paste("DOC concentration (mg L"^-1 * ")"))
options(pillar.sigfig = 5)

p_outname <- "./R_GAMs/outputs/figures/"
dd <- "20221020_"

source("./R_data-cleaning/bpwtp/code/clean-wavelet.R")
gbp_raw <- wavelet_data(ts_monthly = TRUE)

# Finlay et al.  DOC_mg.L GAMs ------------------------------------------------
gbp <- select(gbp_raw, date_ymd:DOC_mg.L)
gbp <- mutate(gbp, date = paste(year, month, 15, sep = "-"),
                   date = ymd(date))
gbp <- select(gbp, date, year, month, DOC_mg.L)

# format the `date` var to %j, %Y, %m for modelling
meanDate <- with(gbp, mean(as.numeric(date))) # 12781.69
gbp <- mutate(gbp,
              modYear  = as.numeric(format(date, '%Y')),
              modMonth = as.numeric(format(date, '%m')),
              modDoy   = as.numeric(format(date, '%j')),
              cDate    = (as.numeric(date) - mean(as.numeric(date))) / 1000)
gbp <- select(gbp, date:month, modYear:cDate, DOC_mg.L)

# plot the data
# ggplot(gbp, aes(modDoy, DOC_mg.L)) + 
#   facet_wrap(~ year) +
#   geom_line()
# 
# ggplot(gbp, aes(date, DOC_mg.L)) + 
#   geom_line()

# use more threads
ctrl <- gam.control(nthreads = 7, newton = list(maxHalf = 200))

# try some models
m1 <- gam(log(DOC_mg.L) ~ s(modYear, k = 30) + s(modDoy, k = 24, bs = 'cc'),
          data = gbp, family = gaussian, method = 'REML', knots = list(modDoy = c(0.5, 366.5)),
          control = ctrl)

m2 <- gam(log(DOC_mg.L) ~ s(modYear, k = 10) + s(modDoy, k = 10, bs = 'ad', xt = list(bs = 'cc')),
          data = gbp, family = gaussian, method = 'REML', knots = list(modDoy = c(0.5, 366.5)),
          control = ctrl)

m3 <- gam(log(DOC_mg.L) ~ s(modDoy, factor(modYear), k = 12, bs = 'fs', xt = list(bs = 'cc')),
          data = gbp, family = gaussian, method = 'REML', knots = list(modDoy = c(0.5, 366.5)),
          control = ctrl)

m4 <- gam(log(DOC_mg.L) ~ s(cDate, k = 30) + s(modDoy, factor(modYear), k = 12, bs = 'fs', xt = list(bs = 'cc')),
          data = gbp, family = gaussian, method = 'REML', knots = list(modDoy = c(0.5, 366.5)),
          control = ctrl)

m5 <- gam(log(DOC_mg.L) ~ te(modYear, modDoy, k = c(10, 20), bs = c('tp', 'cc')),
          data = gbp, family = gaussian, method = 'REML', knots = list(modDoy = c(0.5, 366.5)),
          control = ctrl)

m6 <- gam(log(DOC_mg.L) ~ s(cDate, k = 30) + s(modDoy, by = factor(modYear), id = 1, k = 10, bs = 'cc'),
          data = gbp, family = gaussian, method = 'REML', knots = list(modDoy = c(0.5, 366.5)),
          control = ctrl)

m7 <- gam(log(DOC_mg.L) ~ s(modDoy, factor(modYear), bs = 'fs', xt = list(bs = 'cr')),
          data = gbp, family = gaussian, method = 'REML', knots = list(modDoy = seq(0.5, 366.5, length = 10)),
          control = ctrl)

m8 <- gam(log(DOC_mg.L) ~ te(cDate, modDoy, bs = c('tp', 'cc'), k = c(10, 10)),
          data = gbp, family = gaussian, method = 'REML', knots = list(modDoy = c(0.5, 366.5)))

## just try to fit the entire time series as a smooth of time
m9 <- gam(log(DOC_mg.L) ~ s(cDate, bs = 'tp', k = 100),
          data = gbp, family = gaussian, method = 'REML', control = ctrl)

## adaptive smoother version of m9
m10 <- gam(log(DOC_mg.L) ~ s(cDate, bs = 'ad', k = 250, m = 30),
           data = gbp, family = gaussian, method = 'REML', control = ctrl)

## adaptive smoother version of m9 but with a seasonal cycle
m11 <- gam(log(DOC_mg.L) ~ s(cDate, k = 250) + s(modDoy, bs = 'cc', k = 22),
           data = gbp, family = gaussian, method = 'REML', control = ctrl)

## Gaussian process
m12 <- gam(log(DOC_mg.L) ~ s(cDate, bs = 'gp', m = c(3, r = 200), k = 150),
           data = gbp, family = gaussian, method = 'REML', control = ctrl,
           optimizer = c('outer', 'nlm'))

# compare models with AIC
AICv <- as_tibble(AIC(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12)) %>% 
  mutate(mod = paste("m", 1:12, sep = "")) %>% 
  rename(EDF = df)
AICv %>% arrange(AIC) # M4 best
AICv %>% arrange(desc(EDF)) # M4 most complex

M <- m4
summary(M)
# R2adj = 0.958
# Deviance explain = 97.7%
# -REML = -268.24
gam.check(M)
sum(pen.edf(M))
### The best-fitting GAM to model the DOC time series comprised a TPRS of Date
### and cyclic CRS of Day of Year with Year as a random effect. This model
### explained 98% of the deviance in DOC concentration with an adjusted R2 of
### 0.96. Out of the twelve models estimated, the best-fitting model was the
### most complex of in terms of effective degrees of freedom (EDF = 176.0). It
### also provided the best fit to the DOC data (AIC = —956.3) compared to the
### next best model (AIC = —914.0). By allowing the seasonal pattern to vary
### between years (i.e., not fixing a reccurring within-year trend) the
### best-fitting model was able to explain years that had a strong seasonal
### pattern (e.g., 1991, 1998, 2004, 2012, 2019; FigureGAMc) without imposing
### this pattern on years with increasing, decreasing, or stagnant patterns
### (e.g., 1994, 1996, 2002, 2007, 2014, 2015; Figure GAMc). Adding a smoothly
### varying between-year trend allowed the best-fitting model to capture
### multi-annual trends in DOC concentration while maintaining within-year
### variation. In years with rapid month-to-month change (e.g., 1991, 1996,
### 1999, 2000, 2011, 2012) the best-fitting model did not suffer from severe
### overfitting (FigureGAMa,c).


pred <- with(gbp, data.frame(date = seq(min(date), max(date), by = 1L)))
pred <- mutate(pred,
               modYear = as.numeric(format(date, '%Y')),
               modDoy  = as.numeric(format(date, '%j')),
               cDate   = (as.numeric(date) - meanDate) / 1000)
predvals <- predict(M, newdata = pred, se.fit = TRUE)
pred <- cbind(pred, as.data.frame(predvals))
pred <- mutate(pred, Fitted = exp(fit),
               Upper = exp(fit + (2 * se.fit)),
               Lower = exp(fit - (2 * se.fit)))

fit_plt <- ggplot(gbp, aes(x = date, y = DOC_mg.L)) +
  geom_hline(yintercept = mean(gbp$DOC_mg.L), col = 'navy', lty = 3) + 
  geom_ribbon(data = pred,
              mapping = aes(x = date, ymin = Lower, ymax = Upper),
              alpha = 0.2, fill = 'black', colour = NA, inherit.aes = FALSE) +
  geom_line(data = pred,
            mapping = aes(y = Fitted),
            colour = 'red') +
  geom_point(size = 1, alpha = 1/2) +
  geom_rug(aes(y = DOC_mg.L), sides = 'l', length = grid::unit(0.008, "npc"), alpha = 0.3) + 
  # ylim(c(2.5, 12.5)) +
  theme(plot.tag = element_text(face = 'bold')) +
  labs(x = "Year", y = DOC_lab, tag = 'a')
  
doy_plt <- ggplot(pred, aes(x = modDoy, y = Fitted, group = modYear, colour = modYear)) +
  geom_line() +
  scale_colour_viridis(option = 'inferno', begin = 0, name = 'Year') +
  ylim(c(2.5, NA)) +
  theme(legend.position = 'none',
        legend.key.height = unit(0.75, "cm"),
        plot.tag = element_text(face = 'bold')) +
  labs(x = 'Day of year', y = DOC_lab, tag = 'b')

doy_legend <- ggplot(pred, aes(x = modDoy, y = Fitted, group = modYear, colour = modYear)) +
  geom_line() +
  scale_colour_viridis(option = 'inferno', begin = 0, name = 'Year') +
  ylim(c(2.5, NA)) +
  annotate("rect", xmin = 0, xmax = 366, ymin = 2.5, ymax = 12.5, fill = 'white') +
  theme(legend.position = c(0.5, 0.5),
        legend.key.height = unit(0.75, "cm"),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) 

facetted_fit <- ggplot(pred, aes(modDoy, Fitted, group = modYear)) +
  facet_wrap(~ modYear, ncol = 10) +
  # geom_hline(yintercept = median(gbp$DOC_mg.L), col = 'pink', lty = 2) + 
  geom_hline(yintercept = mean(gbp$DOC_mg.L), col = 'navy', lty = 3) + 
  geom_point(data = gbp, aes(y = DOC_mg.L), size = 0.6) +
  geom_ribbon(data = pred, aes(modDoy, ymin = Lower, ymax = Upper),
              alpha = 0.2, fill = 'black', col = NA, inherit.aes = FALSE) +
  geom_line(col = 'red') + 
  ylim(c(NA, 14)) +
  theme(plot.tag = element_text(face = 'bold')) +
  labs(x = 'Day of year', y = DOC_lab, tag = 'c')

pdesign <- "
1112223
4444444
"

((fit_plt + doy_plt + doy_legend) + plot_layout(design = pdesign)) / facetted_fit

pdesign <- "
11111222223
"
p1 <- (fit_plt + doy_plt + doy_legend) + plot_layout(design = pdesign)
p2 <- facetted_fit
pf <- p1/p2 + plot_layout(heights = c(1, 1.5))

# pf <- (fit_plt + doy_plt) / facetted_fit + guide_area() + plot_layout(design = pdesign, guides = '')
# pf <- (fit_plt + doy_plt + guide_area() + plot_layout(design = pdesign, guides = 'collect')) / facetted_fit
# pf <- (fit_plt + doy_plt) / facetted_fit + plot_layout(heights = c(1, 2))
# pf

ggsave(paste0(p_outname, dd, "p_longtermGAM.png"), pf, w = 12, h = 10)

draw(M)

pd1 <- draw(M, select = "s(cDate)") &
  geom_line(col = 'red') & 
  labs(title = 's(Date)', x = "Date")

pd2 <- draw(M, select = "s(modDoy,factor(modYear))") &
  scale_color_viridis_d(option = "inferno") &
  theme(legend.position = 'right') &
  labs(col = 'Year', title = "s(Day of year, factor(Year))", x = "Day of year") 

(pd1+pd2) & plot_layout(nrow = 2) &
  plot_annotation(tag_levels = 'a') + theme(plot.tag = element_text(face = 'bold'))

p_appraise <- appraise(M, point_col = "steelblue", point_alpha = 0.5, n_bins = 'scott') & 
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold')) 
ggsave(paste0(p_outname, dd, "p_appraise.png"), p_appraise, w = 7.2, h = 5.5)






# extras ------------------------------------------------------------------
pred <- with(gbp, data.frame(date = seq(min(date), max(date), by = 1L)))
pred <- mutate(pred,
               modYear = as.numeric(format(date, '%Y')),
               modDoy  = as.numeric(format(date, '%j')),
               cDate   = (as.numeric(date) - meanDate) / 1000)
predvals <- predict(M, newdata = pred, se.fit = TRUE)
pred <- cbind(pred, as.data.frame(predvals))
pred <- mutate(pred, Fitted = fit,
               Upper = fit + (2 * se.fit),
               Lower = fit - (2 * se.fit))

# fit_plt
ggplot(gbp, aes(x = date, y = DOC_mg.L)) +
  geom_point(size = 1) +
  geom_ribbon(data = pred,
              mapping = aes(x = date, ymin = Lower, ymax = Upper),
              alpha = 0.2, fill = 'black', colour = NA, inherit.aes = FALSE) +
  geom_line(data = pred,
            mapping = aes(y = Fitted),
            colour = 'red') +
  labs(x = NULL, y = DOC_lab)

# doy_plt
ggplot(pred, aes(x = modDoy, y = Fitted, group = modYear, colour = modYear)) +
  geom_line() +
  scale_colour_viridis(option = 'plasma', begin = 0, end = 0.95, name = 'Year') +
  labs(x = 'Day of year', y = DOC_lab)

# facetted_fit
ggplot(pred, aes(modDoy, Fitted, group = modYear)) +
  facet_wrap(~ modYear, ncol = 10) +
  geom_point(data = gbp, aes(y = DOC_mg.L), size = 0.6) +
  geom_ribbon(data = pred, aes(modDoy, ymin = Lower, ymax = Upper),
              alpha = 0.2, fill = 'black', col = NA, inherit.aes = FALSE) +
  geom_line(col = 'red') + 
  labs(x = 'Day of year', y = DOC_lab)

draw(M)
appraise(M, method = 'uniform', type = 'deviance',
         n_bins = 'scott')
appraise(M, point_col = "steelblue", point_alpha = 0.4)
appraise(M, point_col = "steelblue", point_alpha = 0.4,
         line_col = "black")



# Model 4 -----------------------------------------------------------------

g4a <- gam(DOC_mg.L ~ s(cDate, k = 29) + s(modDoy, factor(modYear), k = 9, bs = 'fs', xt = list(bs = 'cc')),
           data = gbp, family = gaussian, method = 'REML', knots = list(modDoy = c(0.5, 366.5)),
           control = ctrl)

g4b_ML <- gam(log(DOC_mg.L) ~ s(cDate, k = 29) + s(modDoy, factor(modYear), k = 9, bs = 'fs', xt = list(bs = 'cc')),
              data = gbp, family = gaussian, method = 'REML', knots = list(modDoy = c(0.5, 366.5)),
              control = ctrl)

g4b_REML <- gam(log(DOC_mg.L) ~ s(cDate, k = 30) + s(modDoy, factor(modYear), k = 12, bs = 'fs', xt = list(bs = 'cc')),
                data = gbp, family = gaussian, method = 'REML', knots = list(modDoy = c(0.5, 366.5)),
                control = ctrl)
AIC(g4b_ML, g4b_REML)

AIC(g4a, g4b) # log-transformation seems to provide better fit (much lower AIC)














M <- m4
summary(M)
# R2adj = 0.958
# Deviance explain = 97.7%
# -REML = -268.24
gam.check(M)

pred <- with(gbp, data.frame(date = seq(min(date), max(date), by = 1L)))
pred <- mutate(pred,
               modYear = as.numeric(format(date, '%Y')),
               modDoy  = as.numeric(format(date, '%j')),
               cDate   = (as.numeric(date) - meanDate) / 1000)
predvals <- predict(M, newdata = pred, se.fit = TRUE)
pred <- cbind(pred, as.data.frame(predvals))
pred <- mutate(pred, Fitted = exp(fit),
               Upper = exp(fit + (2 * se.fit)),
               Lower = exp(fit - (2 * se.fit)))

fit_plt <- ggplot(gbp, aes(x = date, y = DOC_mg.L)) +
  geom_ribbon(data = pred,
              mapping = aes(x = date, ymin = Lower, ymax = Upper),
              alpha = 0.2, fill = 'black', colour = NA, inherit.aes = FALSE) +
  geom_line(data = pred,
            mapping = aes(y = Fitted),
            colour = 'red') +
  geom_point(size = 1) +
  # ylim(c(2.5, 12.5)) +
  theme(plot.tag = element_text(face = 'bold')) +
  labs(x = "Year", y = DOC_lab, tag = 'a')

ggplot(gbp, aes(x = date, y = DOC_mg.L)) +
  geom_rug(sides = "l") +
  geom_ribbon(data = pred,
              mapping = aes(x = date, ymin = Lower, ymax = Upper),
              alpha = 0.2, fill = 'black', colour = NA, inherit.aes = FALSE) +
  geom_line(data = pred,
            mapping = aes(y = Fitted),
            colour = 'red') +
  # geom_point(size = 1) +
  # ylim(c(2.5, 12.5)) +
  theme(plot.tag = element_text(face = 'bold')) +
  labs(x = "Year", y = DOC_lab, tag = 'a')

geom_rug(aes(x = x2),
         data = eg1,
         sides = "b", length = grid::unit(0.02, "npc")) +

doy_plt <- ggplot(pred, aes(x = modDoy, y = Fitted, group = modYear, colour = modYear)) +
  geom_line() +
  scale_colour_viridis(option = 'inferno', begin = 0, name = 'Year') +
  ylim(c(2.5, NA)) +
  theme(legend.position = 'right',
        legend.key.height = unit(1.5, "cm"),
        plot.tag = element_text(face = 'bold')) +
  labs(x = 'Day of year', y = DOC_lab, tag = 'b')

facetted_fit <- ggplot(pred, aes(modDoy, Fitted, group = modYear)) +
  facet_wrap(~ modYear, ncol = 10) +
  geom_point(data = gbp, aes(y = DOC_mg.L), size = 0.6) +
  geom_ribbon(data = pred, aes(modDoy, ymin = Lower, ymax = Upper),
              alpha = 0.2, fill = 'black', col = NA, inherit.aes = FALSE) +
  geom_line(col = 'red') + 
  ylim(c(2.5, NA)) +
  theme(plot.tag = element_text(face = 'bold')) +
  labs(x = 'Day of year', y = DOC_lab, tag = 'c')

pf <- (fit_plt + doy_plt) / facetted_fit
pf

draw(M)

pd1 <- draw(M, select = "s(cDate)") &
  geom_line(col = 'red') & 
  labs(title = 's(Date)', x = "Date")

pd2 <- draw(M, select = "s(modDoy,factor(modYear))") &
  scale_color_viridis_d(option = "inferno") &
  theme(legend.position = 'right') &
  labs(col = 'Year', title = "s(Day of year, factor(Year))", x = "Day of year") 

(pd1+pd2) & plot_layout(nrow = 2) &
  plot_annotation(tag_levels = 'a') + theme(plot.tag = element_text(face = 'bold'))

p_appraise <- appraise(M, point_col = "steelblue", point_alpha = 0.5, n_bins = 'scott') & 
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = 'bold')) 

