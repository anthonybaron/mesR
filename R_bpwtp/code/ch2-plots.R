library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(scales)
library(patchwork)
library(viridis)
library(wsyn)

source("./R_data-cleaning/bpwtp/code/clean-wavelet.R")
source("./R_data-cleaning/flow-reconstruction/code/clean-flows.R")

theme_set(theme_bw(base_size = 12) + theme(panel.grid = element_blank()))
p_outname <- "./R_wavelet/outputs/figures/"
dd <- format(Sys.time(), "%Y%m%d")

wtdoc <- wt_data()$wt_doc
bp_drivers <- wavelet_data() %>% mutate(SUVA = UV254 / DOC_mg.L * 10)
bp_long <- bp_drivers %>% pivot_longer(cols = -c(date_ymd, year, month), 
                                       names_to = "parameter", values_to = "result")

bp_drivers %>% 
  ggplot(aes(yday(date_ymd), DOC_mg.L)) + 
  facet_wrap(~ year, ncol = 10) + 
  geom_line()
# years with distinct seasonal pattern:
# 1991 (kinda), 1998, 1999, 2004, 2005, 2008, 2010, 2012, 2018, 2019

ffr <- station_flow_monthly()
ff <- select(ffr, date_ymd, Year, Month, SK05JG004_combined_cms, SK05JG006_cms, RC_IC_cms)
ff <- rename(ff, LD = SK05JG006_cms, LC = RC_IC_cms, BP = SK05JG004_combined_cms)
ff_long <- ff %>% pivot_longer(cols = -c(date_ymd, Month, Year),
                               names_to = "station", values_to = "result")




# Wavelet transform example plot (DOC) ------------------------------------

p1 <- bp_drivers %>% 
  ggplot(aes(date_ymd, DOC_mg.L)) + 
  geom_line(size = 1.5, col = "steelblue") + 
  ylim(c(2.5, NA)) +
  labs(x = "Year", y = expression(paste("DOC concentration (mg L"^-1*")")))

p2 <- data.frame(x = 1.5:2.5, y = 1.5:2.5) %>%
  ggplot(aes(x, y)) + 
  geom_point(col = "transparent") +
  geom_segment(x = 1.75, y = 2, xend = 2.25, yend = 2,
              lineend = "butt", 
              linejoin = "round",
              size = 5, 
              arrow = arrow(length = unit(0.5, "inches")),
              colour = "forestgreen") +
  annotate("text", x = 2, y = 2.125, label = "Wavelet transform", fontface = "italic", size = 7) +
  theme_void()

# p3 is the wt plot... 


# Wavelet transform example plot ------------------------------------------

# Start with a sine wave of amplitude 1 and period 15 that operates for
# t = 1,...,100 but then disappears.
time1 <- 1:100
time2 <- 101:200
times <- c(time1, time2)

ts1p1 <- sin(2*pi*time1/15)
ts1p2 <- 0*time2
ts1 <- c(ts1p1, ts1p2)

ts <- ts1
plot(ts1)

# Then add a sine wave of amplitude 1 and period 8 that operates for 
# t = 101,...,200 but before that is absent.
ts2p1 <- 0*time1
ts2p2 <- sin(2*pi*time2/8)
ts2 <- c(ts2p1, ts2p2)

ts <- ts + ts2
plot(ts)

# Then add normally distributed white noise of mean 0 and standard deviation 0.5.
ts3 <- rnorm(200, mean = 0, sd = 0.5)

ts <- ts + ts3
plot(ts, ylab = "Signal", xlab = "Time", type = 'l', lwd = 2)

# Apply wavelet transform
tsc <- cleandat(ts, times, clev = 1)
wtres <- wt(tsc$cdat, times)

plotmag(wtres)

tiff("R_wavelet/outputs/figures/p_wavelet-transform.tif", units = "in", res = 300, width = 9, height = 4)

laymat <- matrix(1, nrow = 1, ncol = 2)
laymat[1, ] <- 1:2
layout(laymat)
par(mar = c(4, 4, 3, 1.5) + 0.1, mgp = c(3, 1, 0))

plot(ts, ylab = "Signal", xlab = "Time", type = 'l', lwd = 2)
# text(0.4, 2.5, substitute(paste(bold("a"))), cex = 1.2, col = 'pink')
mtext(substitute(paste(bold("a"))), cex = 1.2,  adj = -0.21, line = 1)

plotmag(wtres)
mtext(substitute(paste(bold("b"))), cex = 1.2, adj = -0.25, line = 1)
#
dev.off()

# Coherence and phase example plot ----------------------------------------

tmax <- 50
res <- 0.1
tt <- seq(1, tmax, res)

pp1 <- 2
sig1 <- sin(seq(0, 2*pi*tmax/pp1, length.out = length(tt)))
pp2 <- 10
sig2 <- sin(seq(0, 2*pi*tmax/pp2, length.out = length(tt)))

comb1 <- sig1 + 0.7*sig2 + 3.5
comb2 <- sig1 + -0.7*sig2

sig3 <- sig2[tt <= 20]
sig4 <- sig3*0.9
sig5 <- sin(seq(-pi/2, 2*pi*20/pp2 - (pi/2), length.out = length(tt[tt <= 20])))
sig6 <- sig3*-1

cd <- c("#00BFC4", "#F8766D")

tiff("R_wavelet/outputs/figures/p_peda.tif", units = "in", res = 300, width = 6.5, height = 4)

laymat <- matrix(1, nrow = 2, ncol = 3)
laymat[2, ] <- 2:4
layout(laymat)
par(mar = c(1.5, 1.5, 2, 1.5), mgp = c(1, 1, 0), oma = c(2, 2, 0, 0))

# a
plot(NA, NA, ylim = c(-2, 5.2), xlim = range(tt),
     xlab = "", ylab = "", xaxt = "n", yaxt = "n")
lines(tt, comb1, lwd = 2, col = cd[1])
lines(tt, comb2, lwd = 2, col = cd[2])
axis(1, at = c(0, 10, 20, 30, 40, 50), labels = NA)
axis(2, at = c(-1, 1.5, 4), labels = NA)
mtext("Timescale-specific relationship", 3, line = 0.25)
text(0.4, 4.9, substitute(paste(bold("a"))), cex = 1.2)

# b
plot(NA, NA, ylim = c(-1, 1), xlim = c(0, 20),
     xaxt = "n", yaxt = "n", xlab = "", ylab = "")
axis(1, at = c(0, 10, 20), labels = NA)
axis(2, at = c(-1, 0, 1), labels = NA)
lines(tt[tt <= 20], sig3, lwd = 2, col = cd[1])
lines(tt[tt <= 20], sig4, lwd = 2, col = cd[2])
mtext(expression(paste("in-phase (", phi, " = 0)")))
text(1, 0.9, substitute(paste(bold("b"))), cex = 1.2)

# c
plot(NA, NA, ylim = c(-1, 1), xlim = c(0, 20),
     xaxt = "n", yaxt = "n", xlab = "", ylab = "")
axis(1, at = c(0, 10, 20), labels = NA)
axis(2, at = c(-1, 0, 1), labels = NA)
lines(tt[tt <= 20], sig3, lwd = 2, col = cd[2])
lines(tt[tt <= 20], sig5, lwd = 2, col = cd[1])
mtext(expression(paste("lagged positive (", phi, " = ", pi, "/2)")))
text(1, 0.9, substitute(paste(bold("c"))), cex = 1.2)

# d
plot(NA, NA, ylim = c(-1, 1), xlim = c(0, 20),
     xaxt = "n", yaxt = "n", xlab = "", ylab = "")
axis(1, at = c(0, 10, 20), labels = NA)
axis(2, at = c(-1, 0, 1), labels = NA)
lines(tt[tt <= 20], sig3, lwd = 2, col = cd[1])
lines(tt[tt <= 20], sig6, lwd = 2, col = cd[2])
mtext(expression(paste("anti-phase (", phi, " = ", pi, ")")))
text(1, 0.9, substitute(paste(bold("d"))), cex = 1.2)

mtext("Time", 1, outer = T)
mtext("Signal", 2, outer = T)

dev.off()


# Water chemistry time series ---------------------------------------------


p_mon <- function(df = bp_long, var = "", lc = "", ylab = "") {
  
  df <- filter(df, parameter == var)  
  
  p <- df %>% 
    ggplot(aes(x = date_ymd, y = result)) +
    geom_line(size = 0.6, col = lc) + 
    theme(plot.tag = element_text(face = "bold")) +
    labs(x = NULL, y = ylab) 
  
  return(p)
  
}

p_mon_DOC <- function(df = bp_long, var = "", lc = "", ylab = "") {
  
  df <- filter(df, parameter == var)  
  
  p <- df %>% 
    ggplot(aes(x = date_ymd, y = result)) +
    geom_line(size = 1, col = lc) + 
    theme(plot.tag = element_text(face = "bold")) +
    labs(x = NULL, y = ylab) 
  
  return(p)
  
}

DOC_lab <- expression(paste("DOC concentration (mg L"^-1*")")) 
SO4_lab <- expression(paste("SO"["4"]^" 2–", " (mg L"^-1*")"))
chla_lab <- expression(paste("Chl ", italic("a"), " (µg L"^-1*")")) 
TP_lab <- expression(paste("TP (µg L"^-1*")")) 
SRP_lab <- expression(paste("SRP (µg L"^-1*")")) 
DON_lab <- expression(paste("DON (mg L"^-1*")")) 
NO3_lab <- expression(paste("NO"[3]^" –", " (mg L"^-1*")")) 
NH3_lab <- expression(paste("NH"[4]^" +", " (mg L"^-1*")")) 


# black and white
# p1 <- p_mon(var = "DOC_mg.L", lc = "grey30", ylab = DOC_lab) + lims(y = c(2.5, NA)) + labs(tag = "a")
# p2 <- p_mon(var = "SO4_mg.L", lc = "grey30", ylab = SO4_lab) + scale_y_continuous(breaks = seq(50, 350, 100)) + labs(tag = "b")
# p3 <- p_mon(var = "chla_ug.L", lc = "grey30", ylab = chla_lab) + labs(tag = "e")
# p4 <- p_mon(var = "TP_ug.L", lc = "grey30", ylab = TP_lab) + lims(y = c(0, 250)) + labs(tag = "c")
# p5 <- p_mon(var = "SRP_ug.L", lc = "grey30", ylab = SRP_lab) + labs(tag = "d")
# p6 <- p_mon(var = "DON_mg.L", lc = "grey30", ylab = DON_lab)  + labs(tag = "f")
# p7 <- p_mon(var = "NO3_mg.L", lc = "grey30", ylab = NO3_lab) + labs(x = "Year", tag = "g")
# p8 <- p_mon(var = "NH3_mg.L", lc = "grey30", ylab = NH3_lab) + lims(y = c(0, 0.5)) + labs(x = "Year", tag = "h")

# colour
scales::show_col(viridis(5, option = "C", end = 0.8))
scales::show_col(viridis(5, option = "C"))
cc <- viridis(5, option = "C", end = 0.9)

scales::show_col(colorblind_pal()(8))
cb <- colorblind_pal()(8)
cb[2] # NO3, NH4
cb[5] # Chl a
cb[6] # BP, LD, LC
cb[7] # SO4
cb[9] # TP and SRP

  
p1 <- p_mon_DOC(var = "DOC_mg.L", lc = 'forestgreen', ylab = DOC_lab) +
  lims(y = c(2.5, NA)) + labs(x = 'Year') +
  theme(plot.tag = element_text(face = 'bold')) + 
  labs(tag = 'a')
ggsave(paste0(p_outname, dd, "p_DOC.png"), p1, w = 7, h = 3)
p1a <- bp_drivers %>% 
  ggplot(aes(yday(date_ymd), DOC_mg.L, group = year, col = year)) + 
  geom_line(size = 0.85, alpha = 2/3) + 
  scale_color_viridis_c() + 
  lims(y = c(2.5, 12.5)) + 
  theme(plot.tag = element_text(face = 'bold')) +
  labs(x = "Day of year", y = DOC_lab, col = "Year", tag = 'b')

pa <- bp_drivers %>% 
  filter(year %in% c(1990:1999)) %>% 
  ggplot(aes(yday(date_ymd), DOC_mg.L, group = year, col = year)) + 
  geom_line(size = 0.85, alpha = 2/3) + 
  scale_color_viridis_c() + 
  lims(y = c(2.5, 12.5)) 
pb <- bp_drivers %>% 
  filter(year %in% c(2000:2009)) %>% 
  ggplot(aes(yday(date_ymd), DOC_mg.L, group = year, col = year)) + 
  geom_line(size = 0.85, alpha = 2/3) + 
  scale_color_viridis_c() + 
  lims(y = c(2.5, 12.5)) 
pc <- bp_drivers %>% 
  filter(year %in% c(2010:2019)) %>% 
  ggplot(aes(yday(date_ymd), DOC_mg.L, group = year, col = year)) + 
  geom_line(size = 0.85, alpha = 2/3) + 
  scale_color_viridis_c() + 
  lims(y = c(2.5, 12.5)) 

pa/pb/pc

  
p1f <- p1 / p1a
ggsave(paste0(p_outname, dd, "p_DOCf.png"), p1f, w = 7, h = 6)


p2 <- p_mon(var = "SO4_mg.L", lc = cc[5], ylab = SO4_lab) + scale_y_continuous(breaks = seq(50, 350, 100)) + labs(tag = "b")
p3 <- p_mon(var = "chla_ug.L", lc = cc[4], ylab = chla_lab) + labs(tag = "e")
p4 <- p_mon(var = "TP_ug.L", lc = cc[3], ylab = TP_lab) + lims(y = c(0, 250)) + labs(tag = "c")
p5 <- p_mon(var = "SRP_ug.L", lc = cc[3], ylab = SRP_lab) + labs(tag = "d")
p6 <- p_mon(var = "DON_mg.L", lc = cc[2], ylab = DON_lab)  + labs(tag = "f")
p7 <- p_mon(var = "NO3_mg.L", lc = cc[2], ylab = NO3_lab) + labs(x = "Year", tag = "g")
p8 <- p_mon(var = "NH3_mg.L", lc = cc[2], ylab = NH3_lab) + lims(y = c(0, 0.5)) + labs(x = "Year", tag = "h")

p_all <- (p1 + p2) / (p4 + p5) / (p3 + p6) / (p7 + p8)
# ggsave(paste0(p_outname, dd, "p_ts.png"), p_all, w = 8, h = 9)

p2 <- p_mon(var = "SO4_mg.L", lc = "#999999", ylab = SO4_lab) + scale_y_continuous(breaks = seq(50, 350, 100)) + labs(tag = "d")
p4 <- p_mon(var = "TP_ug.L", lc = cb[8], ylab = TP_lab) + lims(y = c(0, 250)) + labs(tag = "e")
p5 <- p_mon(var = "SRP_ug.L", lc = cb[8], ylab = SRP_lab) + labs(tag = "f")
p3 <- p_mon(var = "chla_ug.L", lc = cb[4], ylab = chla_lab) + labs(tag = "g")
p7 <- p_mon(var = "NO3_mg.L", lc = cb[7], ylab = NO3_lab) + labs(tag = "h")
p8 <- p_mon(var = "NH3_mg.L", lc = cb[7], ylab = NH3_lab) + lims(y = c(0, 0.5)) + labs(tag = "i")

p_ff <- function(df = ff_long, var = "", lc = "", ylab = "") {
  
  df <- filter(df, station == var)  
  
  p <- df %>% 
    ggplot(aes(x = date_ymd, y = result)) +
    geom_line(size = 0.6, col = lc) + 
    theme(plot.tag = element_text(face = "bold")) +
    labs(x = NULL, y = ylab) 
  
  return(p)
  
}
BP_lab <- expression(paste(italic("Q"), ""[BP]*" (m"^-3*" s"^-1*")"))
LD_lab <- expression(paste(italic("Q"), ""[LD]*" (m"^-3*" s"^-1*")"))
LC_lab <- expression(paste(italic("Q"), ""[LC]*" (m"^-3*" s"^-1*")"))
p_ld <- p_ff(var = "LD", lc = cb[6], ylab = LD_lab) + labs(x = "Year", tag = "a")
p_bp <- p_ff(var = "BP", lc = cb[6], ylab = BP_lab) + lims(y = c(NA, 20)) + labs(x = "Year", tag = "b")
p_lc <- p_ff(var = "LC", lc = cb[6], ylab = LC_lab) + labs(x = "Year", tag = "c")

p_all2 <- (p_ld + p_bp + p_lc) / (p2 + p4 + p5) / (p3 + p7 + p8) 
ggsave(paste0(p_outname, dd, "p_ts.png"), p_all2, w = 11, h = 7)

bp_long %>% 
  filter(parameter %in% c("SK05JG006_cms", "RC_IC_cms", "SK05JG004_combined_cms")) %>% 
  mutate(parameter = case_when(
    parameter == "SK05JG006_cms" ~ "Lake Diefenbaker",
    parameter == "SK05JG004_combined_cms" ~ "Buffalo Pound Lake",
    parameter == "RC_IC_cms" ~ "Local catchment",
    TRUE ~ as.character(parameter)
  )) %>% 
  ggplot(aes(yday(date_ymd), result, col = parameter)) + 
  facet_wrap(~ year, ncol = 10) + 
  geom_line(size = 2, alpha = 2/3) + 
  scale_color_viridis_d(end = 0.8, option = 'plasma', direction = -1) + 
  theme(legend.position = 'bottom') + 
  labs(x = 'Day of year', y = 'Flow (cms)', col = 'Station')

bp_long %>% 
  filter(parameter %in% c("SK05JG006_cms", "RC_IC_cms", "SK05JG004_combined_cms")) %>% 
  mutate(parameter = case_when(
    parameter == "SK05JG006_cms" ~ "Lake Diefenbaker",
    parameter == "SK05JG004_combined_cms" ~ "Buffalo Pound Lake",
    parameter == "RC_IC_cms" ~ "Local catchment",
    TRUE ~ as.character(parameter)
  )) %>% 
  ggplot(aes(date_ymd, result, col = parameter)) + 
  geom_line(size = 2, alpha = 2/3) + 
  scale_color_viridis_d(end = 0.8, option = 'plasma', direction = -1) + 
  theme(legend.position = 'bottom') + 
  labs(x = 'Year', y = 'Flow (cms)', col = 'Station')

bp_long %>% 
  filter(parameter %in% c("SK05JG006_cms", "RC_IC_cms", "SK05JG004_combined_cms",
                          "DOC_mg.L")) %>% 
  mutate(parameter = case_when(
    parameter == "SK05JG006_cms" ~ "Lake Diefenbaker",
    parameter == "SK05JG004_combined_cms" ~ "Buffalo Pound Lake",
    parameter == "RC_IC_cms" ~ "Local catchment",
    parameter == "DOC_mg.L" ~ "DOC concentration",
    TRUE ~ as.character(parameter)
  )) %>% 
  ggplot(aes(date_ymd, result, col = parameter)) + 
  geom_line(size = 2, alpha = 2/3) + 
  scale_color_colorblind() +
  theme(legend.position = 'bottom') + 
  labs(x = 'Year', y = 'Flow (cms) or DOC concentration (mg/L)', col = 'Station')

bp_long %>% 
  filter(parameter %in% c("SK05JG006_cms", "RC_IC_cms", "SK05JG004_combined_cms",
                          "DOC_mg.L")) %>% 
  mutate(parameter = case_when(
    parameter == "SK05JG006_cms" ~ "Lake Diefenbaker",
    parameter == "SK05JG004_combined_cms" ~ "Buffalo Pound Lake",
    parameter == "RC_IC_cms" ~ "Local catchment",
    parameter == "DOC_mg.L" ~ "DOC concentration",
    TRUE ~ as.character(parameter)
  )) %>% 
  ggplot(aes(yday(date_ymd), result, col = parameter)) + 
  facet_wrap(~ year, ncol = 10) +
  geom_line(size = 1.5, alpha = 2/3) + 
  scale_color_colorblind() +
  theme(legend.position = 'bottom') + 
  labs(x = 'Day of year', y = 'Flow (cms) or DOC concentration (mg/L)', col = 'Station')

# Flow time series --------------------------------------------------------

p_ff <- function(df = ff_long, var = "", lc = "", ylab = "") {
  
  df <- filter(df, station == var)  
  
  p <- df %>% 
    ggplot(aes(x = date_ymd, y = result)) +
    geom_line(size = 0.6, col = lc) + 
    theme(plot.tag = element_text(face = "bold")) +
    labs(x = NULL, y = ylab) 
  
  return(p)
  
}

BP_lab <- expression(paste(italic("Q"), ""[BP]*" (m"^-3*" s"^-1*")"))
LD_lab <- expression(paste(italic("Q"), ""[LD]*" (m"^-3*" s"^-1*")"))
LC_lab <- expression(paste(italic("Q"), ""[LC]*" (m"^-3*" s"^-1*")"))

p_bp <- p_ff(var = "BP", lc = "steelblue3", ylab = BP_lab) + lims(y = c(NA, 20)) + labs(x = "Year", tag = "c")
p_ld <- p_ff(var = "LD", lc = "grey30", ylab = LD_lab) + labs(tag = "a")
p_lc <- p_ff(var = "LC", lc = "forestgreen", ylab = LC_lab) + labs(tag = "b")

p_flow <- p_ld / p_lc / p_bp
ggsave(paste0(p_outname, dd, "p_flow.png"), p_flow, w = 8, h = 6)



# C:N ratio (workspace) ---------------------------------------------------


bp_drivers %>% 
  ggplot(aes(DOC_mg.L, DON_mg.L, col = year)) + 
  geom_point() +
  scale_color_viridis()

bp_drivers %>% 
  ggplot(aes(DOC_mg.L, DON_mg.L, col = year, group = year)) + 
  facet_wrap(~ year, ncol = 10) +
  geom_line() +
  scale_color_viridis() +
  theme(legend.position = 'bottom')

cor.test(bp_drivers$DOC_mg.L, bp_drivers$DON_mg.L)

bp_drivers %>% 
  mutate(CN_ratio = DOC_mg.L/DON_mg.L) %>% 
  filter(CN_ratio < 100) %>% 
  ggplot(aes(date_ymd, CN_ratio, col = year)) + 
  geom_point() +
  scale_color_viridis()

bp_drivers %>% 
  mutate(CN_ratio = DOC_mg.L/DON_mg.L) %>% 
  filter(CN_ratio < 20) %>% 
  ggplot(aes(yday(date_ymd), CN_ratio)) + 
  facet_wrap(~ year, ncol = 10) +
  geom_point(col = "#E34234") +
  geom_line(col = "#E34234") +
  geom_line(data = bp_drivers, aes(yday(date_ymd), SK05JG004_combined_cms), col = 'grey') +
  geom_line(data = bp_drivers, aes(yday(date_ymd), RC_IC_cms), col = 'orange') +
  geom_line(data = bp_drivers, aes(yday(date_ymd), SK05JG006_cms), col = 'steelblue') +
  theme(legend.position = 'bottom')


CNR <- bp_drivers %>% 
  mutate(CN_ratio = DOC_mg.L/DON_mg.L) %>% 
  filter(CN_ratio < 100) %>% 
  group_by(month) %>% 
  summarise(CNRmean = mean(CN_ratio, na.rm = TRUE),
            CNRsd = sd(CN_ratio))

doy <- bp_drivers %>% 
  group_by(month) %>% 
  summarise(doymean = mean(yday(date_ymd), na.rm = TRUE))

CNR <- CNR %>% right_join(doy) %>% mutate(year = NA)

p <- bp_drivers %>% 
  mutate(CN_ratio = DOC_mg.L/DON_mg.L) %>% 
  filter(CN_ratio < 100) %>% 
  ggplot(aes(yday(date_ymd), CN_ratio, col = year, group = year)) + 
  geom_line(size = 0.85, alpha = 1/2) + 
  scale_color_viridis_c() +
  labs(x = 'Day of year', y = 'DOC:DON', col = 'Year')
p + geom_line(data = CNR, aes(doymean, CNRmean, group = year), col = 'red', size = 1.5) 

DOCmean <- bp_drivers %>% 
  group_by(month) %>% 
  summarise(meanDOC = mean(DOC_mg.L),
            sdDOC = sd(DOC_mg.L),
            meanDON = mean(DON_mg.L)) %>% 
  mutate(meanDON = meanDON * 10)

CNR <- CNR %>% right_join(DOCmean)

CNR %>% 
  ggplot(aes(doymean, CNRmean)) +
  geom_line() + 
  geom_line(data = CNR, aes(doymean, meanDOC), col = 'forestgreen') +
  geom_line(data = CNR, aes(doymean, meanDON), col = 'red')


df <- eems %>% 
  # select(-c(Fmax, TSS_mg.L, source, site_altname, PeakA_percent:PeakT_percent,
  #           site_num, distHaversine_m)) %>% 
  pivot_longer(cols = c(TDN_mg.L:spT), 
               names_to = "parameter", 
               values_to = "result") %>% 
  mutate(parameter = factor(parameter),
         Site = site_code_long) %>% 
  group_by(Site, site_abbr1, Year, dist_km, parameter) %>% 
  summarise(mean_parameter = mean(result, na.rm = TRUE),
            sd_parameter = sd(result, na.rm = TRUE)) %>% 
  mutate(lower = mean_parameter - sd_parameter,
         upper = mean_parameter + sd_parameter) %>% 
  ungroup() 

p_mean_sd <- df %>% 
  filter(parameter == parm & !is.na(mean_parameter)) %>%
  # filter(parameter == parm) %>%
  ggplot(aes(dist_km, mean_parameter)) + 
  facet_wrap(~ Year, nrow = 1) +
  xlim(c(0, 30)) +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper, col = site_abbr1), width = 0.33) +
  geom_point(aes(col = site_abbr1), size = 3) + 
  scale_color_viridis_d(begin = 0, end = 0.8) +
  theme(legend.position = "bottom", plot.tag = element_text(face = "bold")) +
  guides(col = guide_legend(nrow = 1)) + 
  labs(x = dist_lab, y = parm_lab, col = "Site")

CNR %>% 
  mutate(upper = meanDOC + sdDOC,
         lower = meanDOC - sdDOC) %>% 
  ggplot(aes(doymean, meanDOC)) +
  geom_point(size = 3, col = 'forestgreen') +
  geom_line(size = 1, col = 'forestgreen') +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 8, size = 1, col = 'forestgreen')
  

dd <- bp_drivers %>% select(1:4)
dds <- dd %>% 
  group_by(year) %>% 
  summarise(max = max(DOC_mg.L),
            min = min(DOC_mg.L),
            mean = mean(DOC_mg.L)) %>% 
  mutate(diff = max - min)
dds %>% 
  ggplot(aes(year, diff)) + 
  geom_col()
dd %>% 
  ggplot(aes(year, DOC_mg.L, group = year)) + 
  geom_boxplot()
dds %>% arrange(desc(diff)) %>% View()
hist(dds$diff)
hist(dds$mean)


# SUVA (workspace) --------------------------------------------------------

SUVA_lab <- expression(paste("SUVA"[254]*" (L mg-C"^-1*" m"^-1*")"))

SUVA <- bp_drivers %>% 
  select(date_ymd:DOC_mg.L, DON_mg.L, UV254) %>% 
  mutate(CN_ratio = DOC_mg.L/DON_mg.L,
         SUVA = UV254 / DOC_mg.L * 10) %>% 
  filter(CN_ratio < 20) 
ps <- SUVA %>% 
  ggplot(aes(date_ymd, SUVA)) +
  geom_line(size = 1, col = 'navyblue') + 
  lims(y = c(NA, 2.6)) + 
  labs(x = NULL, y = SUVA_lab)
ggsave("./R_bpwtp/outputs/figures/p_SUVAbp.png", ps, width = 8, height = 4)

SUVA %>% filter(is.na(SUVA)) %>% tail()

# pd <- bp_drivers %>% ggplot(aes(date_ymd, DOC_mg.L)) + geom_line()
# pd/ps


SUVAmean <- SUVA %>% 
  group_by(month) %>% 
  summarise(SUVAmean = mean(SUVA, na.rm = TRUE)) %>% 
  mutate(year = NA,
         doy = CNR$doymean)
psy <- SUVA %>% 
  ggplot(aes(yday(date_ymd), SUVA, col = year, group = year)) + 
  geom_line() + 
  scale_color_viridis()
psy + geom_line(data = SUVAmean, aes(doy, SUVAmean, group = year), size = 2, col = 'red')
