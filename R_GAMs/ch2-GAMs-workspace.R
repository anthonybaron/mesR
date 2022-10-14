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

gbp %>% 
  ggplot(aes(date_ymd, DOC_mg.L)) + 
  geom_line(size = 1, col = "steelblue3")

gbp %>% 
  ggplot(aes(yday(date_ymd), DOC_mg.L)) + 
  facet_wrap(~ year, ncol = 10) +
  geom_line(size = 1, col = "steelblue3")

summaryDOC <- gbp %>% 
  group_by(month) %>% 
  summarise(medianDOC = median(DOC_mg.L),
            meanDOC = mean(DOC_mg.L)) %>% 
  mutate(doy = unique(yday(gbp$date_ymd))[1:12],
         year = NA)

gbp %>% 
  ggplot(aes(yday(date_ymd), DOC_mg.L, group = year, col = year)) + 
  geom_line(alpha = 1/2) +
  scale_colour_viridis_c() +
  geom_line(data = summaryDOC, aes(doy, meanDOC, group = year),
            size = 1.2, col = "red", lty = 2) +
  geom_line(data = summaryDOC, aes(doy, medianDOC, group = year), 
            size = 1.2, col = "black", lty = 4) +
  lims(y = c(2.5, NA)) +
  labs(x = "Julian day", y = DOC_lab, col = "Year")



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
m1 <- gamm(DOC_mg.L ~ s(month, bs = "cc", k = 12) + s(Time, k = 20),
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
plot(DOC_mg.L - mean(DOC_mg.L) ~ date_ymd, data = gbp, type = "n",
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
