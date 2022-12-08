library(mgcv)
library(gratia)
library(ggplot2)
library(lubridate)
library(dplyr)
library(tidyr)

theme_set(theme_bw(base_size = 12) + theme(panel.grid = element_blank()))

source("./R_data-cleaning/bpwtp/code/clean-wavelet.R")
gbp <- wavelet_data(ts_monthly = TRUE)
names(gbp[, c(8, 6, 5, 11)])

DOC_lab <- expression(paste("DOC concentration (mg L"^-1 * ")"))

coha <- c(names(gbp[, c(8, 6, 5, 11, 13)])) # SO4, TP, Chl a, NH4, QLD 
cohs <- c(names(gbp[, c(8, 6, 5, 11)]))     # SO4, TP, Chl a, NH4
cohl <- c(names(gbp[, c(8, 6, 13)]))        # SO4, TP, QLD 

gbpa <- gbp[c(1:4, 8, 6, 5, 11, 13)]
gbps <- gbp[c(1:4, 8, 6, 5, 11)]
gbpl <- gbp[c(1:4, 8, 6, 13)]

names(gbpa)[9] <- "QLD_cms"
names(gbpl)[7] <- "QLD_cms"

# gbp %>% 
#   ggplot(aes(date_ymd, DOC_mg.L)) + 
#   geom_line(size = 1, col = "steelblue3")
# 
# gbp %>% 
#   ggplot(aes(yday(date_ymd), DOC_mg.L)) + 
#   facet_wrap(~ year, ncol = 10) +
#   geom_line(size = 1, col = "steelblue3")

summaryDOC <- gbp %>% 
  group_by(month) %>% 
  summarise(medianDOC = median(DOC_mg.L),
            meanDOC = mean(DOC_mg.L)) %>% 
  mutate(doy = unique(yday(gbp$date_ymd))[1:12],
         year = NA)

# gbp %>% 
#   ggplot(aes(yday(date_ymd), DOC_mg.L, group = year, col = year)) + 
#   geom_line(alpha = 1/2) +
#   scale_colour_viridis_c() +
#   geom_line(data = summaryDOC, aes(doy, meanDOC, group = year),
#             size = 1.2, col = "red", lty = 2) +
#   geom_line(data = summaryDOC, aes(doy, medianDOC, group = year), 
#             size = 1.2, col = "black", lty = 4) +
#   lims(y = c(2.5, NA)) +
#   labs(x = "Julian day", y = DOC_lab, col = "Year")

# par(mfrow = c(1, 2))
# hist(gbp$DOC_mg.L)
# hist(log(gbp$DOC_mg.L))



# Modelling seasonal data with GAMs | G.  Simpson -------------------------
# https://fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/#fnref1

gbp$Time <- as.numeric(gbp$date_ymd) / 1000
gbp <- select(gbp, date_ymd:month, Time, everything())

# Fit naive model that assumes uncorrelated errors
# Use a cyclic cubic spline with 12 knots to model monthly data
m <- gamm(DOC_mg.L ~ s(month, bs = "cc", k = 12) + s(Time),
          data = gbp)

summary(m$gam)
layout(matrix(1:2, ncol = 2))
plot(m$gam, scale = 0)

acf(resid(m$lme), lag.max = 36, main = "ACF")
pacf(resid(m$lme), lag.max = 36, main = "pACF")
ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod = "L-BFGS-B")

## AR(1)
mar1 <- gamm(DOC_mg.L ~ s(month, bs = "cc", k = 12) + s(Time, k = 20),
             data = gbp,
             correlation = corARMA(form = ~1|year, p = 1),
             control = ctrl)

## AR(2)
m2 <- gamm(DOC_mg.L ~ s(month, bs = "cc", k = 12) + s(Time, k = 20),
           data = gbp,
           correlation = corARMA(form = ~1|year, p = 2),
           control = ctrl)

## AR(3)
m3 <- gamm(DOC_mg.L ~ s(month, bs = "cc", k = 12) + s(Time, k = 20),
           data = gbp,
           correlation = corARMA(form = ~1|year, p = 3),
           control = ctrl)

anova(m$lme, m1$lme, m2$lme, m3$lme)
#        Model df  AIC  BIC logLik   Test L.Ratio p-value
# m$lme      1  5 1106 1126   -548                       
# m1$lme     2  6  760  783   -374 1 vs 2     348  <.0001 --- winner
# m2$lme     3  7  761  789   -374 2 vs 3       0   0.689 
# m3$lme     4  8  763  794   -373 3 vs 4       1   0.378

layout(matrix(1:2, ncol = 2))
plot(m1$gam, scale = 0)

res <- resid(m1$lme, type = "normalized")
acf(res, lag.max = 36, main = "ACF - AR(1) errors")
pacf(res, lag.max = 36, main = "ACF - AR(1) errors")

want <- seq(1, nrow(gbp), length.out = 200)
pdat <- with(gbp,
             data.frame(Time = Time[want], date_ymd = date_ymd[want], month = month[want]))

## predict trend contributions
p <- predict(m$gam, newdata = pdat, type = "terms", se.fit = TRUE)
p1 <- predict(m1$gam, newdata = pdat, type = "terms", se.fit = TRUE)
p2 <- predict(m2$gam, newdata = pdat, type = "terms", se.fit = TRUE)
p3 <- predict(m3$gam, newdata = pdat, type = "terms", se.fit = TRUE)

## combine with the predictions data, including fitted and SEs
pdat <- transform(pdat,
                  p  = p$fit[, 2],  se = p$se.fit[, 2],
                  p1 = p1$fit[, 2], se = p1$se.fit[, 2],
                  p2 = p3$fit[, 2], se = p3$se.fit[, 2],
                  p3 = p3$fit[, 2], se = p3$se.fit[, 2])

op <- par(mar = c(5, 4, 2, 2) + 0.1)
ylim <- with(pdat, range(p, p1, p2, p3))
ylim[1] <- floor(ylim[1])
ylim[2] <- ceiling(ylim[2])
ylab <- DOC_lab
layout(1)
plot(DOC_mg.L - mean(DOC_mg.L) ~ date_ymd, data = gbp, type = "l", col = "grey",
     ylab = ylab, ylim = ylim)
lines(p  ~ date_ymd, data = pdat, col = "black")
lines(p1 ~ date_ymd, data = pdat, col = "red")
lines(p2 ~ date_ymd, data = pdat, col = "blue")
lines(p3 ~ date_ymd, data = pdat, col = "forestgreen", lwd = 1)
legend("topleft",
       legend = c("Uncorrelated errors", paste0("AR(", 1:3, ") errors")),
       bty = "n", col = "black", "red", "blue", "forestgreen",
       lty = 1, lwd = c(1, 1, 1))
par(op)

# gbp %>% 
#   ggplot(aes(p1, )) 


## AR(1)
m1 <- gamm(DOC_mg.L ~ s(month, bs = "cc", k = 12) + s(Time, k = 20),
           data = gbp,
           correlation = corARMA(form = ~1|year, p = 1),
           control = ctrl)

gratia::draw(m1$gam)
gratia::appraise(m1$gam)

## AR(1) + SO4
ms <- gamm(DOC_mg.L ~ s(month, bs = "cc", k = 12) + s(Time, k = 20) + s(SO4_mg.L, k = 12),
           data = gbp,
           correlation = corARMA(form = ~1|year, p = 1),
           control = ctrl)

gratia::draw(ms$gam)
gratia::appraise(ms$gam)
plot(ms$gam)
summary(ms$gam)

anova(m$lme, m1$lme, ms$lme)


gbp2 %>% 
  ggplot(aes(yday(date_ymd), DOC_mg.L)) + 
  facet_wrap(~ Year, ncol = 10) + 
  geom_point() 


# Finlay et al.  pCO2 GAMs ------------------------------------------------

# ctrl_old <- list(niterEM = 0, msVerbose = TRUE, optimMethod = "L-BFGS-B")
ctrl_new <- gam.control(nthreads = 7, newton = list(maxHalf = 200))



gbp$Time <- as.numeric(gbp$date_ymd) / 1000
gbp <- select(gbp, date_ymd:month, Time, everything())
meanDate <- with(gbp, mean(as.numeric(date_ymd)))
gbp2 <- rename(gbp, Year = year, Month = month)
gbp2 <- mutate(gbp2, cDate = (as.numeric(date_ymd) - mean(as.numeric(date_ymd))) / 1000)


m1 <- gam(DOC_mg.L ~ s(Year, k = 30) + s(Month, k = 12, bs = 'cc'),
          data = gbp2, family = gaussian, method = 'REML', knots = list(Month = c(0.5, 12.5)),
          control = ctrl_new)

m2 <- gam(DOC_mg.L ~ s(cDate, k = 30) + s(Month, k = , bs  = 'ad', xt = list(bs = 'cc')),
          data = gbp2, family = gaussian, method = 'REML', knots = list(Month = c(0.5, 12.5)),
          control = ctrl_new)

m3 <- gam(DOC_mg.L ~ s(Month, factor(Year), k = 4, bs = 'fs', xt = list(bs = 'cc')),
          data = gbp2, family = gaussian, method = 'REML', knots = list(Month = c(0.5, 12.5)),
          control = ctrl_new)

m4 <- gam(DOC_mg.L ~ s(cDate, k = 30) + s(Month, factor(Year), k = 12, bs = 'fs', xt = list(bs = 'cc')),
          data = gbp2, family = gaussian, method = 'ML', knots = list(Month = c(0.5, 12.5)),
          control = ctrl_new)

m5 <- gam(DOC_mg.L ~ te(Year, Month, k = c(27, 14), bs = c('tp', 'cc')),
          data = gbp2, family = gaussian, method = 'REML', knots = list(Month = c(0.5, 12.5)),
          control = ctrl_new)

m6 <- gam(DOC_mg.L ~ s(cDate, k = 11) + s(Month, by = factor(Year), id = 1, k = 13, bs = 'cc'),
          data = gbp2, family = gaussian, method = 'REML', knots = list(Month = c(0.5, 12.5)),
          control = ctrl_new)

m7 <- gam(DOC_mg.L ~ s(Month, factor(Year), k = 11, bs = 'fs', xt = list(bs = 'cr')),
          data = gbp2, family = gaussian, method = 'REML', knots = list(Month = seq(0.5, 12.5, length = 11)),
          control = ctrl_new)

m8 <- gam(DOC_mg.L ~ te(cDate, Month, bs = c('tp', 'cc'), k = c(30, 12)),
          data = gbp2, family = gaussian, method = 'REML', knots = list(Month = c(0.5, 12.5)),
          select = TRUE)

## just try to fit the entire time series as a smooth of time
m9 <- gam(DOC_mg.L ~ s(cDate, bs = 'tp', k = 360),
          data = gbp2, family = gaussian, method = 'REML', control = ctrl_new)

## adaptive smoother version of m9
m10 <- gam(DOC_mg.L ~ s(cDate, bs = 'ad', k = 250, m = 30),
           data = gbp2, family = gaussian, method = 'REML', control = ctrl_new)

## adaptive smoother version of m9 but with a seasonal cycle
m11 <- gam(DOC_mg.L ~ s(cDate, k = 250) + s(Month, bs = 'cc', k = 12),
           data = gbp2, family = gaussian, method = 'REML', control = ctrl_new)

## Gaussian process
m12 <- gam(DOC_mg.L ~ s(cDate, bs = 'gp', m = c(3, r = 200), k = 200),
           data = gbp2, family = gaussian, method = 'REML', control = ctrl_new,
           optimizer = c('outer', 'nlm'))
m13 <- gam(DOC_mg.L ~ s(cDate, bs = 'gp', k = 10),
           data = gbp2, family = gaussian, method = 'REML',
           optimizer = c('outer', 'nlm'), knots = list(Month = c(0.5, 12.5)))

# plot(pCO2 ~ date, data = bp, pch = 16, cex = 0.6)
# lines(predict(m8, type = "response") ~ date, data = bp, col = "red")

# par(mfrow = c(3, 4))
par(mfrow = c(3, 1))
plot(DOC_mg.L ~ date_ymd, data = gbp2, pch = 16, cex = 0.6, main = "M1")
lines(predict(m1, type = "response") ~ date_ymd, data = gbp2, col = "red")
plot(DOC_mg.L ~ date_ymd, data = gbp2, pch = 16, cex = 0.6, main = "M2")
lines(predict(m2, type = "response") ~ date_ymd, data = gbp2, col = "red")
plot(DOC_mg.L ~ date_ymd, data = gbp2, pch = 16, cex = 0.6, main = "M3")
lines(predict(m3, type = "response") ~ date_ymd, data = gbp2, col = "red")
plot(DOC_mg.L ~ date_ymd, data = gbp2, pch = 16, cex = 0.6, main = "M4")
lines(predict(m4, type = "response") ~ date_ymd, data = gbp2, col = "red")
plot(DOC_mg.L ~ date_ymd, data = gbp2, pch = 16, cex = 0.6, main = "M5")
lines(predict(m5, type = "response") ~ date_ymd, data = gbp2, col = "red")
plot(DOC_mg.L ~ date_ymd, data = gbp2, pch = 16, cex = 0.6, main = "M6")
lines(predict(m6, type = "response") ~ date_ymd, data = gbp2, col = "red")
plot(DOC_mg.L ~ date_ymd, data = gbp2, pch = 16, cex = 0.6, main = "M7")
lines(predict(m7, type = "response") ~ date_ymd, data = gbp2, col = "red")
plot(DOC_mg.L ~ date_ymd, data = gbp2, pch = 16, cex = 0.6, main = "M8")
lines(predict(m8, type = "response") ~ date_ymd, data = gbp2, col = "red")
plot(DOC_mg.L ~ date_ymd, data = gbp2, pch = 16, cex = 0.6, main = "M9")
lines(predict(m9, type = "response") ~ date_ymd, data = gbp2, col = "red")
plot(DOC_mg.L ~ date_ymd, data = gbp2, pch = 16, cex = 0.6, main = "M10")
lines(predict(m10, type = "response") ~ date_ymd, data = gbp2, col = "red")
plot(DOC_mg.L ~ date_ymd, data = gbp2, pch = 16, cex = 0.6, main = "M11")
lines(predict(m11, type = "response") ~ date_ymd, data = gbp2, col = "red")
plot(DOC_mg.L ~ date_ymd, data = gbp2, pch = 16, cex = 0.6, main = "M12")
lines(predict(m12, type = "response") ~ date_ymd, data = gbp2, col = "red")

AIC(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12)
#            df      AIC
# m1   32.38241 992.4365
# m2   33.42404 997.1237
# m3  162.95990 590.4423
# m4  173.98177 436.2100 **
# m5  109.54645 848.6272
# m6   68.14388 996.6582
# m7  158.33867 558.0323
# m8   87.03716 803.7183
# m9   84.92397 674.1449
# m10 108.51647 494.8052 *
# m11  98.22643 504.3386
# m12 174.59863 410.2019 ***

summary(m4)  # R2 = 0.958, Dev expl = 97.8%, AIC = 436
summary(m10) # R2 = 0.949, Dev expl = 96.2%, AIC = 495
summary(m12) # R2 = 0.961, Dev expl = 97.9%, AIC = 410

m4 <- gam(DOC_mg.L ~ s(cDate, k = 30) + s(Month, factor(Year), k = 12, bs = 'fs', xt = list(bs = 'cc')),
          data = gbp2, family = gaussian, method = 'ML', knots = list(Month = c(0.5, 12.5)),
          control = ctrl_new)
M <- m4
M41 <- gam(DOC_mg.L ~ s(cDate, k = 30) + s(Month, factor(Year), k = 12, bs = 'fs', xt = list(bs = 'cc')),
           data = gbp2, family = gaussian, method = 'ML', knots = list(Month = c(0.5, 12.5)),
           control = ctrl_new, select = TRUE)
M <- M41 # improves model R2 and lowers AIC but still bumpy between months
# M42 <- gam(DOC_mg.L ~ s(cDate, k = 30) + s(Month, factor(Year), k = 12, bs = 'fs', xt = list(bs = 'cc')),
#            data = gbp2, family = gaussian, method = 'ML', knots = list(Month = c(0.5, 12.5)),
#            control = ctrl_new, select = TRUE, gamma = 1.5)
# M <- M42
M43 <- gam(DOC_mg.L ~ s(cDate, k = 4, m = 1) + s(Month, factor(Year), k = 12, bs = 'fs', xt = list(bs = 'cc')),
           data = gbp2, family = gaussian, method = 'REML', knots = list(Month = c(0.5, 12.5)),
           control = ctrl_new, select = TRUE)
M <- M43

m8 <- gam(DOC_mg.L ~ te(cDate, Month, bs = c('tp', 'cc'), k = c(30, 12), m = 1),
          data = gbp2, family = gaussian, method = 'REML', knots = list(Month = c(0.5, 12.5)),
          select = TRUE)
M <- m8


mar1 <- gamm(DOC_mg.L ~ s(cDate),
             data = gbp2, method = 'REML',
             correlation = corARMA(form = ~1|Year, p = 1),
             control = ctrl)



gbp2$DOY <- as.numeric(format(gbp2$date_ymd, '%j'))

# pred <- with(gbp2, data.frame(date_ymd = seq(min(date_ymd), max(date_ymd), by = 1L))) 
pred <- with(gbp2, data.frame(date_ymd = seq(min(date_ymd), max(date_ymd), by = 1L))) 
pred <- mutate(pred,
               Year = as.numeric(format(date_ymd, '%Y')),
               Month = as.numeric(format(date_ymd, '%m')),
               DOY = as.numeric(format(date_ymd, '%j')),
               cDate = (as.numeric(date_ymd) - meanDate) / 1000)
# pred$Time2 <- (as.numeric(pred$date_ymd) - as.numeric(mean(gbp2$date_ymd))) / 1000
predvals <- predict(M, newdata = pred, se.fit = TRUE)
pred <- cbind(pred, as.data.frame(predvals))
pred <- mutate(pred,
               Upper = fit + (2 * se.fit),
               Lower = fit - (2 * se.fit))

pred <- as_tibble(pred)
predvals_df <- as_tibble(predvals)

ma <- gam(DOC_mg.L ~ s(cDate),
          data = gbp2, method = 'REML')

# fit plot
gbp2 %>% 
  ggplot(aes(cDate, DOC_mg.L)) + 
  geom_point(size = 0.9) + 
  geom_ribbon(data = pred, aes(cDate, ymin = Lower, ymax = Upper),
              alpha = 0.2, fill = 'black', col = NA, inherit.aes = FALSE) +
  geom_line(data = pred, aes(y = fit), col = 'red') + 
  labs(x = NULL, y = DOC_lab)

# month plot
pred %>% 
  ggplot(aes(Month, fit, group = Year, col = Year)) + 
  geom_line() + 
  scale_colour_viridis_c(option = 'plasma', begin = 0, end = 0.95, name = 'Year') + 
  labs(x = "Month", y = DOC_lab)

# pred %>% 
#   ggplot(aes(Month, fit)) + 
#   facet_wrap(~ Year) + 
#   geom_point(size = 0.1) + 
#   geom_ribbon(data = pred, aes(Month, ymin = Lower, ymax = Upper),
#               alpha = 0.2, fill = 'black', col = NA, inherit.aes = FALSE) +
#   geom_line(data = pred, aes(y = fit), col = 'red') + 
#   labs(x = NULL, y = DOC_lab)

gbp3 %>% 
  mutate(Year = year(date_ymd),
         Month = month(date_ymd)) %>% 
  ggplot(aes(Month, DOC)) + 
  facet_wrap(~ Year, ncol = 10) + 
  geom_point(size = 0.9) + 
  geom_ribbon(data = pred, aes(Month, ymin = Lower, ymax = Upper),
              alpha = 0.2, fill = 'black', col = NA, inherit.aes = FALSE) +
  geom_line(data = pred, aes(y = fit), col = 'red') + 
  labs(x = NULL, y = DOC_lab)
  

pred %>% 
  ggplot(aes(yday(date_ymd), fit, group = Year)) + 
  facet_wrap(~ Year, ncol = 10) + 
  geom_point(data = gbp2, aes(y = DOC_mg.L), size = 0.6) + 
  geom_line(col = 'red') + 
  labs(x = "Day of year", y = DOC_lab)


### trying diff predict
predvals2 <- predict(mar1$gam, type = "response", se.fit = TRUE)
predvals2_df <- as_tibble(predvals2)

pred <- with(gbp2, data.frame(date_ymd = seq(min(date_ymd), max(date_ymd), by = 1L))) 
pred <- mutate(pred,
               Year = as.numeric(format(date_ymd, '%Y')),
               Month = as.numeric(format(date_ymd, '%m')),
               DOY = as.numeric(format(date_ymd, '%j')),
               cDate = (as.numeric(date_ymd) - meanDate) / 1000)
# pred$Time2 <- (as.numeric(pred$date_ymd) - as.numeric(mean(gbp2$date_ymd))) / 1000
predvals2 <- predict(mar1$gam, type = "response", se.fit = TRUE)
pred <- cbind(pred, as.data.frame(predvals))
pred <- mutate(pred,
               Upper = fit + (2 * se.fit),
               Lower = fit - (2 * se.fit),
               Monthc = month(Month, label = TRUE, abbr = TRUE))
gbp2 <- mutate(gbp2, Monthc = month(Month, label = TRUE, abbr = TRUE))
 

pred %>% 
  ggplot(aes(Monthc, fit, group = Year)) +
  facet_wrap(~ Year, ncol = 10) + 
  geom_point(data = gbp2, aes(y = DOC_mg.L), size = 0.6) + 
  geom_line(col = 'red') + 
  ylim(c(2.5, NA)) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x = NULL, y = DOC_lab)






# GAM with drivers --------------------------------------------------------
gbp3 <- select(gbp, date_ymd, 
               DOC = DOC_mg.L,
               SO4 = SO4_mg.L,
               Chla = chla_ug.L, 
               TP = TP_ug.L, 
               NH4 = NH3_mg.L,
               Dief = SK05JG006_cms)

gbp3 %>%
  select(DOC:Dief) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

g1 <- gam(DOC ~ s(SO4, k = 20) + s(Chla, k = 20) + s(TP, k = 20) + s(NH4, k = 20) + s(Dief, k = 20),
          data = gbp3, family = tw, method = "REML", select = TRUE)
summary(g1)
draw(g1)
appraise(g1)
gam.check(g1)

g2 <- gam(DOC ~ s(SO4, k = 80) + s(TP, k = 80) + s(NH4, k = 80) + s(Dief, k = 80),
          data = gbp3, family = tw, method = "REML", select = TRUE)
summary(g2)
draw(g2)
appraise(g2)
gam.check(g2, rep = 100)
g2_fit <- fitted_values(g2)
g2_fit %>% 
  mutate(date_ymd = gbp3$date_ymd) %>% 
  ggplot(aes(date_ymd, fitted)) + 
  geom_line(col = 'red') + 
  geom_point(data = gbp3, aes(y = DOC))



g3 <- gam(DOC ~ s(SO4) + s(TP) + s(NH4) + s(Dief),
          data = gbp3, family = tw, method = "REML", select = TRUE)
summary(g3)
draw(g3)
appraise(g3)
layout(matrix(1:4, ncol = 2, byrow = TRUE))
gam.check(g3, rep = 100)
g3_fit <- fitted_values(g3)
g3_fit %>% 
  mutate(date_ymd = gbp3$date_ymd) %>% 
  ggplot(aes(date_ymd, fitted)) + 
  geom_line(col = 'red') + 
  geom_point(data = gbp3, aes(y = DOC))
