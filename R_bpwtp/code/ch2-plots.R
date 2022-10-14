library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(scales)
library(patchwork)
library(viridis)
library(wsyn)

theme_set(theme_bw(base_size = 14) + theme(panel.grid = element_blank()))

source("./R_data-cleaning/bpwtp/code/clean-wavelet.R")
source("./R_data-cleaning/flow-reconstruction/code/clean-flows.R")

p_outname <- "./R_wavelet/outputs/figures/"
dd <- "20220928_"

wtdoc <- wt_data()$wt_doc
bp_drivers <- wavelet_data()
bp_long <- bp_drivers %>% pivot_longer(cols = -c(date_ymd, year, month), 
                                       names_to = "parameter", values_to = "result")


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
# plot(ts1)

# Then add a sine wave of amplitude 1 and period 8 that operates for 
# t = 101,...,200 but before that is absent.
ts2p1 <- 0*time1
ts2p2 <- sin(2*pi*time2/8)
ts2 <- c(ts2p1, ts2p2)

ts <- ts + ts2
# plot(ts)

# Then add normally distributed white noise of mean 0 and standard deviation 0.5.
ts3 <- rnorm(200, mean = 0, sd = 0.5)

ts <- ts + ts3
# plot(ts)

# Apply wavelet transform
ts <- cleandat(ts, times, clev = 1)
wtres <- wt(ts$cdat, times)

plotmag(wtres)




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

DOC_lab <- expression(paste("DOC (mg L"^-1*")")) 
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

show_col(colorblind_pal()(8))
cb <- colorblind_pal()(8)
cb[2] # NO3, NH4
cb[5] # Chl a
cb[6] # BP, LD, LC
cb[7] # SO4
cb[9] # TP and SRP

  
p1 <- p_mon(var = "DOC_mg.L", lc = cc[1], ylab = DOC_lab) + lims(y = c(2.5, NA)) + labs(tag = "a")
p2 <- p_mon(var = "SO4_mg.L", lc = cc[5], ylab = SO4_lab) + scale_y_continuous(breaks = seq(50, 350, 100)) + labs(tag = "b")
p3 <- p_mon(var = "chla_ug.L", lc = cc[4], ylab = chla_lab) + labs(tag = "e")
p4 <- p_mon(var = "TP_ug.L", lc = cc[3], ylab = TP_lab) + lims(y = c(0, 250)) + labs(tag = "c")
p5 <- p_mon(var = "SRP_ug.L", lc = cc[3], ylab = SRP_lab) + labs(tag = "d")
p6 <- p_mon(var = "DON_mg.L", lc = cc[2], ylab = DON_lab)  + labs(tag = "f")
p7 <- p_mon(var = "NO3_mg.L", lc = cc[2], ylab = NO3_lab) + labs(x = "Year", tag = "g")
p8 <- p_mon(var = "NH3_mg.L", lc = cc[2], ylab = NH3_lab) + lims(y = c(0, 0.5)) + labs(x = "Year", tag = "h")

p_all <- (p1 + p2) / (p4 + p5) / (p3 + p6) / (p7 + p8)
ggsave(paste0(p_outname, dd, "p_ts.png"), p_all, w = 8, h = 9)

p2 <- p_mon(var = "SO4_mg.L", lc = "#999999", ylab = SO4_lab) + scale_y_continuous(breaks = seq(50, 350, 100)) + labs(tag = "a")
p4 <- p_mon(var = "TP_ug.L", lc = cb[8], ylab = TP_lab) + lims(y = c(0, 250)) + labs(tag = "b")
p5 <- p_mon(var = "SRP_ug.L", lc = cb[8], ylab = SRP_lab) + labs(tag = "c")
p3 <- p_mon(var = "chla_ug.L", lc = cb[4], ylab = chla_lab) + labs(tag = "d")
p7 <- p_mon(var = "NO3_mg.L", lc = cb[7], ylab = NO3_lab) + labs(tag = "e")
p8 <- p_mon(var = "NH3_mg.L", lc = cb[7], ylab = NH3_lab) + lims(y = c(0, 0.5)) + labs(tag = "f")
p_ld <- p_ff(var = "LD", lc = cb[6], ylab = LD_lab) + labs(x = "Year", tag = "g")
p_bp <- p_ff(var = "BP", lc = cb[6], ylab = BP_lab) + lims(y = c(NA, 20)) + labs(x = "Year", tag = "h")
p_lc <- p_ff(var = "LC", lc = cb[6], ylab = LC_lab) + labs(x = "Year", tag = "i")

p_all2 <- (p2 + p4 + p5) / (p3 + p7 + p8) / (p_ld + p_bp + p_lc)
ggsave(paste0(p_outname, dd, "p_ts.png"), p_all2, w = 11, h = 7)

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

