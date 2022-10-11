library(wsyn)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(patchwork)
library(cowplot)
library(gridGraphics)
library(spatstat)
library(rstatix)

theme_set(theme_bw(base_size = 12)) #+ theme(panel.grid = element_blank())) 

# bp_drivers %>% 
#   ggplot(aes(date_ymd, DOC_mg.L)) + 
#   geom_line(col = "steelblue3", size = 1) + 
#   lims(y = c(2.5, NA)) +
#   labs(x = "Year", y = expression(paste("DOC concentration (mg L"^-1*")")),
#        subtitle = "Average monthly DOC concentration (1990–2019)")


# Source data -------------------------------------------------------------

source("./R_wavelet/code/wavelet-functions.R")
source("./R_data-cleaning/bpwtp/code/clean-wavelet.R")

bp_drivers <- wavelet_data()
wtl <- wt_data()
cohl <- coherence_data()

bp_drivers %>% 
  ggplot(aes(date_ymd, TP_ug.L)) + 
  geom_line(size = 1, col = "steelblue3") + 
  geom_line(data = bp_drivers, aes(date_ymd, mean(TP_ug.L)), lty = 2) + 
  geom_smooth(method = 'lm')
# TP concentrations were slightly higher after 2005 and generally followed a 
# seasonal pattern, peaking during the summer months (as high as 200 + ug L) and
# falling during the winter months. However, some of the peaks in TP also 
# coincide with peaks in DOC concentration (e.g., 1991, 1999—200, )

bp_drivers %>% 
  ggplot(aes(date_ymd, SRP_ug.L)) + 
  geom_line(size = 1, col = "steelblue3") + 
  geom_line(data = bp_drivers, aes(date_ymd, mean(SRP_ug.L)), lty = 2) + 
  geom_smooth(method = 'lm')
# Peaks in SRP were higher in the 1990s and mid 2000s compared to SRP peaks in
# the 2010s. Average SRP concentrations decreased from the 1990s to 2010s.

bp_drivers %>% 
  ggplot(aes(date_ymd, NO3_mg.L)) + 
  geom_line(size = 1, col = "steelblue3") + 
  geom_line(data = bp_drivers, aes(date_ymd, mean(NO3_mg.L)), lty = 2) +
  geom_smooth(method = 'lm')
# NO3 concentrations higher in the 1990s compared to the 2000s and 2010s, 
# especially from 1997?—2000. From 2000—2010 NO3 was often <DL. 

bp_drivers %>% 
  ggplot(aes(date_ymd, NH3_mg.L)) + 
  geom_line(size = 1, col = "steelblue3") + 
  geom_line(data = bp_drivers, aes(date_ymd, mean(NH3_mg.L)), lty = 2) +
  geom_smooth(method = 'lm')
# no clear interannual patterns in NH4 over length of ts; however concentrations
# were generally higher after ~2005 compared to the 1990s and early 2000s. 

bp_drivers %>% 
  ggplot(aes(date_ymd, chla_ug.L)) + 
  geom_line(size = 1, col = "steelblue3") + 
  geom_line(data = bp_drivers, aes(date_ymd, mean(chla_ug.L)), lty = 2) +
  geom_smooth(method = 'lm')
# also peaks in 1991
# lower during drought years (late 1990s early 2000s)

bp_drivers %>% 
  ggplot(aes(date_ymd, SK05JG006_cms)) +
  geom_smooth(method = 'lm', col = "grey30") +
  geom_line(size = 1, col = "steelblue3") + 
  geom_line(data = bp_drivers, aes(date_ymd, mean(SK05JG006_cms)), lty = 2) 

bp_drivers %>% 
  ggplot(aes(date_ymd, RC_IC_cms)) +
  geom_smooth(method = 'lm', col = "grey30") +
  geom_line(size = 1, col = "steelblue3") + 
  geom_line(data = bp_drivers, aes(date_ymd, mean(RC_IC_cms)), lty = 2) 

bp_drivers %>% 
  ggplot(aes(date_ymd, SK05JG004_combined_cms)) +
  geom_smooth(method = 'lm', col = "grey30") +
  geom_line(size = 1, col = "steelblue3") + 
  geom_line(data = bp_drivers, aes(date_ymd, mean(SK05JG004_combined_cms)), lty = 2) 

bp_drivers %>% group_by(year) %>% get_summary_stats(SO4_mg.L, type = "common") %>% View()
bp_drivers %>% get_summary_stats(type = "common") %>% View()
bp_drivers %>% select(date_ymd:month, DOC_mg.L, SK05JG006_cms, SK05JG004_combined_cms, RC_IC_cms) %>% View()

bp_drivers %>% 
  filter(year <= 1999) %>% 
  summarise(meanNO3 = mean(NH3_mg.L),
            sd = sd(NH3_mg.L))

bp_drivers %>% 
  filter(year >= 1999) %>% 
  summarise(meanNO3 = mean(NH3_mg.L),
            sd = sd(NH3_mg.L))



# Summary stats -----------------------------------------------------------

bd_long <- bp_drivers %>% 
  select(-c(date_ymd:month)) %>% 
  mutate(tmpcol = "tmp") %>% 
  pivot_longer(cols = -c(tmpcol),
               names_to = "parameter",
               values_to = "result") %>% 
  select(-tmpcol)

ss <- bd_long %>% 
  group_by(parameter) %>% 
  get_summary_stats(type = "full")
# write.csv(ss, "./R_wavelet/outputs/data/ts_summary_stats.csv")



# Check autocorrelation in time series ------------------------------------

# acf(bp_drivers$DOC_mg.L)
# acf(bp_drivers$chla_ug.L)
# acf(bp_drivers$TP_ug.L)
# acf(bp_drivers$SRP_ug.L)
# acf(bp_drivers$SO4_mg.L)
# acf(bp_drivers$DON_mg.L)
# acf(bp_drivers$NO3_mg.L)
# acf(bp_drivers$NH3_mg.L)
# acf(bp_drivers$SK05JG004_combined_cms)
# acf(bp_drivers$SK05JG006_cms)
# acf(bp_drivers$RC_IC_cms)


# Wavelet analyses --------------------------------------------------------

# Plot wavelet transform --------------------------------------------------

# tiff("R_wavelet/outputs/figures/p_wt.tif", units = "in", res = 150, width = 11, height = 5)
# 
# laymat <- matrix(1, nrow = 2, ncol = 5)
# laymat[1, ] <- 1:5
# laymat[2, ] <- 6:10
# layout(laymat)
# par(mar = c(1.5, 1.5, 2, 1.5), mgp = c(3, 1, 0), oma = c(3, 3, 0, 1))
# 
# plotmag_wt_dr(wt_doc, xaxs = "r", yaxs = "r", colorbar = FALSE, zlims = c(0, 8),
#               ylocs = c(0, 6, 12, 24, 48, 96, 144),
#               ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
#               xlocs = seq(0, 360, by = 60),
#               xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
# # title("DOC", font.main = 2)
# # legend("topleft", "a) DOC", text.font = 2, bty = "n", cex = 1)
# text(50, 7, substitute(paste(bold("a) DOC"))), cex = 1.1)
# # p1 <- recordPlot()
# # 
# plotmag_wt_dr(wt_so4, xaxs = "r", yaxs = "r", colorbar = FALSE, zlims = c(0, 8),
#               ylocs = c(0, 6, 12, 24, 48, 96, 144),
#               ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
#               xlocs = seq(0, 360, by = 60),
#               xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
# # title(expression(bold("d) SO"["4"]^" 2–")))
# # legend("topleft", expression(bold("d) SO"["4"]^" 2–")), text.font = 2, bty = "n", cex = 1.15)
# text(50, 7, expression(bold("b) SO"["4"]^" 2–")), cex = 1.1)
# # p4 <- recordPlot()
# # 
# plotmag_wt_dr(wt_tp, xaxs = "r", yaxs = "r", colorbar = FALSE, zlims = c(0, 8),
#               ylocs = c(0, 6, 12, 24, 48, 96, 144),
#               ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
#               xlocs = seq(0, 360, by = 60),
#               xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
# # title("TP", font.main = 2)
# # legend("topleft", "b) TP", text.font = 2, bty = "n", cex = 1.15)
# text(50, 7, substitute(paste(bold("c) TP"))), cex = 1.1)
# # p2 <- recordPlot()
# # 
# plotmag_wt_dr(wt_srp, xaxs = "r", yaxs = "r", colorbar = FALSE, zlims = c(0, 8),
#               ylocs = c(0, 6, 12, 24, 48, 96, 144),
#               ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
#               xlocs = seq(0, 360, by = 60),
#               xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
# # title("SRP", font.main = 2)
# # legend("topleft", "c) SRP", text.font = 2, bty = "n", cex = 1.15)
# text(50, 7, substitute(paste(bold("d) SRP"))), cex = 1.1)
# # p3 <- recordPlot()
# # 
# plotmag_wt_dr(wt_chla, xaxs = "r", yaxs = "r", colorbar = TRUE, zlims = c(0, 8),
#               ylocs = c(0, 6, 12, 24, 48, 96, 144),
#               ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
#               xlocs = seq(0, 360, by = 60),
#               xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
# # title("Chlorophyll a", font.main = 2)
# # legend("topleft", expression(paste(bold("e) Chl"), bolditalic(" a"))), text.font = 2, bty = "n", cex = 1.15)
# text(50, 7, expression(paste(bold("e) Chl"), bolditalic(" a"))), cex = 1.1)
# # p10 <- recordPlot()
# # 
# plotmag_wt_dr(wt_no3, xaxs = "r", yaxs = "r", colorbar = FALSE, zlims = c(0, 8),
#               ylocs = c(0, 6, 12, 24, 48, 96, 144),
#               ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
#               xlocs = seq(0, 360, by = 60),
#               xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
# # title(expression(bold("NO"[3]^" –")))
# # legend("topleft", expression(bold("f) NO"[3]^" –")), text.font = 2, bty = "n", cex = 1.15)
# text(50, 7, expression(bold("f) NO"[3]^" –")), cex = 1.1)
# # p5 <- recordPlot()
# # 
# plotmag_wt_dr(wt_nh3, xaxs = "r", yaxs = "r", colorbar = FALSE, zlims = c(0, 8),
#               ylocs = c(0, 6, 12, 24, 48, 96, 144),
#               ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
#               xlocs = seq(0, 360, by = 60),
#               xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
# # title(expression(bold("NH"["3"])))
# # legend("topleft", expression(bold("f) NH"[4]^" +")), text.font = 2, bty = "n") #, cex = 1.15)
# text(50, 7, expression(bold("g) NH"[4]^" +")), cex = 1.1)
# # p6 <- recordPlot()
# # 
# plotmag_wt_dr(wt_dief, xaxs = "r", yaxs = "r", colorbar = FALSE, zlims = c(0, 8),
#               ylocs = c(0, 6, 12, 24, 48, 96, 144),
#               ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
#               xlocs = seq(0, 360, by = 60),
#               xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
# # title(expression(paste(bolditalic("Q"), bold(""["LD"]))), font.main = 2)
# # legend("topleft", expression(paste(bold("h)"), bolditalic(" Q"), bold(""["LD"]))), text.font = 2, bty = "n", cex = 1.15)
# text(50, 7, expression(paste(bold("h)"), bolditalic(" Q"), bold(""["LD"]))), cex = 1.1)
# # p8 <- recordPlot()
# # 
# plotmag_wt_dr(wt_bp, xaxs = "r", yaxs = "r", colorbar = FALSE, zlims = c(0, 8),
#               ylocs = c(0, 6, 12, 24, 48, 96, 144),
#               ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
#               xlocs = seq(0, 360, by = 60),
#               xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
# # title(expression(paste(bolditalic("Q"), bold(""["BP"]))), font.main = 2)
# # legend("topleft", expression(paste(bold("h)"), bolditalic(" Q"), bold(""["BP"]))), text.font = 2, bty = "n", cex = 1.15)
# text(50, 7, expression(paste(bold("i)"), bolditalic(" Q"), bold(""["BP"]))), cex = 1.1)
# # p7 <- recordPlot()
# # 
# plotmag_wt_dr(wt_cat, xaxs = "r", yaxs = "r", colorbar = TRUE, zlims = c(0, 8),
#               ylocs = c(0, 6, 12, 24, 48, 96, 144),
#               ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
#               xlocs = seq(0, 360, by = 60),
#               xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
# # title(expression(paste(bolditalic("Q"), bold(""["LC"]))), font.main = 2)
# # legend("topleft", expression(paste(bold("j)"), bolditalic(" Q"), bold(""["LC"]))), text.font = 2, bty = "n", cex = 1.15)
# text(50, 7, expression(paste(bold("j)"), bolditalic(" Q"), bold(""["LC"]))), cex = 1.1)
# 
# mtext("Year", side = 1, outer = T, line = 1.2)
# mtext("Timescale (years)", 2, outer = T, line = 1)
# 
# dev.off()

# p9 <- recordPlot()
# 
# 
# plot_grid(p1, p2, p3,
#           p4, p5, p6, 
#           p7, p8, p9, p10,
#           ncol = 4, 
#           labels = NULL,
#           label_size = 12,
#           scale = 0.8,
#           axis = "b")


# Coherence: DOC and drivers ----------------------------------------------

# SO4   --   m  l  a
# TP    --   m     a
# SRP   --   m
# Chl a -- s       a
# NO3   -- xxxxxxxxx
# NH4   --   m     a
# QLD   --   m
# QBP   --   m     a
# QLC   -- xxxxxxxxx

cohp <- cohl
band1 <- c(2, 6)
band2 <- c(6, 48)
band3 <- c(48, 120)
band4 <- c(2, 120)
cohp["coh_doc_so4"] <- lapply(FUN = function(x){bandtest(object = x, band = band3)}, X = cohp["coh_doc_so4"])
cohp[c("coh_doc_so4","coh_doc_tp","coh_doc_srp",
       "coh_doc_nh3","coh_doc_dief","coh_doc_bp")] <- lapply(FUN = function(x){bandtest(object = x, band = band2)},
                                                                                                      X = cohp[c("coh_doc_so4","coh_doc_tp","coh_doc_srp",
                                                                                                                 "coh_doc_nh3","coh_doc_dief","coh_doc_bp")])
cohp["coh_doc_chla"] <- lapply(FUN = function(x){bandtest(object = x, band = band1)}, X = cohp["coh_doc_chla"])
cohp[c("coh_doc_so4","coh_doc_tp",
       "coh_doc_nh3","coh_doc_bp")] <- lapply(FUN = function(x){bandtest(object = x, band = band4)}, X = cohp[c("coh_doc_so4","coh_doc_tp",
                                                                                                                "coh_doc_nh3","coh_doc_bp")])
# tiff("R_wavelet/outputs/figures/p_coh.tif", units = "in", res = 150, width = 9, height = 7)

laymat <- matrix(1, nrow = 3, ncol = 3)
laymat[1, ] <- 1:3
laymat[2, ] <- 4:6
laymat[3, ] <- 7:9
layout(laymat)
par(mar = c(3.5, 1.5, 2, 1.5), mgp = c(3, 1, 0), oma = c(3, 5, 1, 2))

plotmag_coh_dr(cohp$coh_doc_so4, sigthresh = 0.95); mtext(expression(bold("a) SO"["4"]^" 2–")), side = 3, adj = 0, padj = -0.2)
plotmag_coh_dr(cohp$coh_doc_tp, sigthresh = 0.95); mtext(expression(bold("b) TP")), side = 3, adj = 0, padj = -0.2)
plotmag_coh_dr(cohp$coh_doc_srp, sigthresh = 0.95); mtext(expression(bold("c) SRP")), side = 3, adj = 0, padj = -0.2)
plotmag_coh_dr(cohp$coh_doc_chla, sigthresh = 0.95); mtext(expression(paste(bold("d) Chl"), bolditalic(" a"))), side = 3, adj = 0, padj = -0.2)
plotmag_coh_dr(cohp$coh_doc_no3, sigthresh = 0.95); mtext(expression(bold("e) NO"["3"]^" –")), side = 3, adj = 0, padj = -0.2)
plotmag_coh_dr(cohp$coh_doc_nh3, sigthresh = 0.95); mtext(expression(bold("f) NH"["4"]^" +")), side = 3, adj = 0, padj = -0.2)
plotmag_coh_dr(cohp$coh_doc_dief, sigthresh = 0.95); mtext(expression(paste(bold("g)"), bolditalic(" Q"), bold(""["LD"]))), side = 3, adj = 0, padj = -0.2)
plotmag_coh_dr(cohp$coh_doc_bp, sigthresh = 0.95); mtext(expression(paste(bold("h)"), bolditalic(" Q"), bold(""["BP"]))), side = 3, adj = 0, padj = -0.2)
plotmag_coh_dr(cohp$coh_doc_cat, sigthresh = 0.95); mtext(expression(paste(bold("i)"), bolditalic(" Q"), bold(""["LC"]))), side = 3, adj = 0, padj = -0.2)

mtext("Timescale (months)", side = 1, outer = T, line = 0.1)
mtext("Timescale (months)", side = 1, outer = T, line = 0.1, adj = 0.1)
mtext("Timescale (months)", side = 1, outer = T, line = 0.1, adj = 0.9)
mtext("Coherence", 2, outer = T, line = 2, adj = 0.52)
mtext("Coherence", 2, outer = T, line = 2, adj = 0.14)
mtext("Coherence", 2, outer = T, line = 2, adj = 0.9)

# dev.off()



# Plotrank ----------------------------------------------------------------
# plotrank()
# The vertical axis label Fract surr gt stands for the fraction of surrogate
# coherences that the coherence of the data is greater than at the given
# timescale, so values are between 0 and 1 and large values indicate
# significance
# 
# tiff("R_wavelet/outputs/figures/p_plotrank.tif", units = "in", res = 150, width = 9, height = 7)

laymat <- matrix(1, nrow = 3, ncol = 3)
laymat[1, ] <- 1:3
laymat[2, ] <- 4:6
laymat[3, ] <- 7:9
layout(laymat)
par(mar = c(3.0, 1.5, 2, 1.5), mgp = c(3, 1, 0), oma = c(3, 5, 1, 2))

plotrank(cohp$coh_doc_so4, sigthresh = 0.95); mtext(expression(bold("a) SO"["4"]^" 2–")), side = 3, adj = 0, padj = -0.2)
plotrank(cohp$coh_doc_tp, sigthresh = 0.95); mtext(expression(bold("b) TP")), side = 3, adj = 0, padj = -0.2)
plotrank(cohp$coh_doc_srp, sigthresh = 0.95); mtext(expression(bold("c) SRP")), side = 3, adj = 0, padj = -0.2)
plotrank(cohp$coh_doc_chla, sigthresh = 0.95); mtext(expression(paste(bold("d) Chl"), bolditalic(" a"))), side = 3, adj = 0, padj = -0.2)
plotrank(cohp$coh_doc_no3, sigthresh = 0.95); mtext(expression(bold("e) NO"["3"]^" –")), side = 3, adj = 0, padj = -0.2)
plotrank(cohp$coh_doc_nh3, sigthresh = 0.95); mtext(expression(bold("f) NH"["4"]^" +")), side = 3, adj = 0, padj = -0.2)
plotrank(cohp$coh_doc_dief, sigthresh = 0.95); mtext(expression(paste(bold("g)"), bolditalic(" Q"), bold(""["LD"]))), side = 3, adj = 0, padj = -0.2)
plotrank(cohp$coh_doc_bp, sigthresh = 0.95); mtext(expression(paste(bold("h)"), bolditalic(" Q"), bold(""["BP"]))), side = 3, adj = 0, padj = -0.2)
plotrank(cohp$coh_doc_cat, sigthresh = 0.95); mtext(expression(paste(bold("i)"), bolditalic(" Q"), bold(""["LC"]))), side = 3, adj = 0, padj = -0.2)

mtext("Timescale (months)", side = 1, outer = T, line = 0.1)
mtext("Timescale (months)", side = 1, outer = T, line = 0.1, adj = 0.1)
mtext("Timescale (months)", side = 1, outer = T, line = 0.1, adj = 0.9)
mtext("Fract surr gt", 2, outer = T, line = 2, adj = 0.52)
mtext("Significance", 2, outer = T, line = 2, adj = 0.14)
mtext("Significance", 2, outer = T, line = 2, adj = 0.9)

# dev.off()

# 0—6 months --- TP, Chl a
# 0.5—4 years --- SO4, TP, SRP, NH4, QLD, QBP
# 4—10 years --- SO4
# all timescales --- SO4, TP, Chl a, NH4, QBP

# cohl$coh_doc_so4$bandp <- NA
# cohl$coh_doc_tp$bandp <- NA
# cohl$coh_doc_srp$bandp <- NA
# cohl$coh_doc_chla$bandp <- NA
# cohl$coh_doc_no3$bandp <- NA
# cohl$coh_doc_nh3$bandp <- NA
# cohl$coh_doc_dief$bandp <- NA
# cohl$coh_doc_cat$bandp <- NA
# cohl$coh_doc_bp$bandp <- NA


# Bandtest ----------------------------------------------------------------

# // 2–6 months -----------------------------------------------------------
band1 <- c(2, 6)
band2 <- c(6, 48)
band3 <- c(48, 120)
band4 <- c(2, 120)

# short timescale denoted with 's'
svars <- c("DOC_SO4", "DOC_TP", "DOC_SRP", "DOC_Chla", 
           "DOC_NO3", "DOC_NH4", "DOC_QLD", "DOC_QLC", "DOC_QBP")

sso4 <- NULL
stp <- NULL
ssrp <- NULL
schla <- NULL
sno3 <- NULL
snh4 <- NULL
sqld <- NULL
sqlc <- NULL
sqbp <- NULL

so41 <- bandtest_coh2(object = cohl$coh_doc_so4, band = band1)
tp1 <- bandtest_coh2(object = cohl$coh_doc_tp, band = band1)
srp1 <- bandtest_coh2(object = cohl$coh_doc_srp, band = band1)
chla1 <- bandtest_coh2(object = cohl$coh_doc_chla, band = band1)
no31 <- bandtest_coh2(object = cohl$coh_doc_no3, band = band1)
nh41 <- bandtest_coh2(object = cohl$coh_doc_nh3, band = band1)
qld1 <- bandtest_coh2(object = cohl$coh_doc_dief, band = band1)
qlc1 <- bandtest_coh2(object = cohl$coh_doc_cat, band = band1)
qbp1 <- bandtest_coh2(object = cohl$coh_doc_bp, band = band1)

sso4 <- rbind(sso4, c(t(as.matrix(so41$bandp[, 3:5]))))
stp <- rbind(stp, c(t(as.matrix(tp1$bandp[, 3:5]))))
ssrp <- rbind(ssrp, c(t(as.matrix(srp1$bandp[, 3:5]))))
schla <- rbind(schla, c(t(as.matrix(chla1$bandp[, 3:5]))))
sno3 <- rbind(sno3, c(t(as.matrix(no31$bandp[, 3:5]))))
snh4 <- rbind(snh4, c(t(as.matrix(nh41$bandp[, 3:5]))))
sqld <- rbind(sqld, c(t(as.matrix(qld1$bandp[, 3:5]))))
sqlc <- rbind(sqlc, c(t(as.matrix(qlc1$bandp[, 3:5]))))
sqbp <- rbind(sqbp, c(t(as.matrix(qbp1$bandp[, 3:5]))))

scoh <- rbind(sso4, stp, ssrp, schla, sno3, snh4, sqld, sqlc, sqbp)
scoh_df <- as.data.frame(scoh)
colnames(scoh_df) <- c("pval", "phi", "coh")
scoh_df <- cbind(scoh_df, svars)
scoh_df$sin <- sin(scoh_df$phi)
scoh_df$cos <- cos(scoh_df$phi)
scoh_df$timescale <- "2- to 6-month timescales"

# significant (p < 0.05)
scoh_sig <- subset(scoh_df, pval < 0.05)
# Chl a

### Sin- and cos-transformations to analyse phase difference between DOC and drivers 
## Cosine -- focuses on how close the relationship b/w DOC and driver is to
## being in phase
# 1. in-phase (phi = 0) == 1
# 2. anti-phase (phi = ± pi) == —1
# 3. quarter-phase (phi = ± pi/2) == 0
# 
## Sine -- focuses on whether the time-lagged relationship b/w DOC and driver 
## tends to be positive or negative
# 1. in-phase (phi = 0) and anti-phase (± pi) == 0
# 2. time-lagged positive (DOC 1/4 cycle behind driver) == —1
# 3. time-lagged negative (DOC 1/4 cycle ahead of driver) == 1
# scoh_df$sin <- sin(scoh_df$phi) 
# scoh_df$cos <- cos(scoh_df$phi)
# TP almost exactly in-phase with DOC (cos(phi) = ~1) and lags slightly 
# behind TP (sin(phi) = -0.00439)
# Chl a highly in-phase with doc (cos(phi) = 0.875) and lags ahead of DOC 
# (sin(phi) = 0.483)

par(mfrow = c(1, 1), mar = c(1.5, 1, 4, 1))
rose(scoh_df$phi,
     unit = "radian", main = "2–6 month timescales", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive (behind)", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -3.5)
mtext("lagged negative (ahead)", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -3.5)


# // 0.5–4 years ----------------------------------------------------------
# medium timescales denoted with 'm'
mvars <- c("DOC_SO4", "DOC_TP", "DOC_SRP", "DOC_Chla", 
           "DOC_NO3", "DOC_NH4", "DOC_QLD", "DOC_QLC", "DOC_QBP")

mso4 <- NULL
mtp <- NULL
msrp <- NULL
mchla <- NULL
mno3 <- NULL
mnh4 <- NULL
mqld <- NULL
mqlc <- NULL
mqbp <- NULL

so42 <- bandtest_coh2(object = cohl$coh_doc_so4, band = band2)
tp2 <- bandtest_coh2(object = cohl$coh_doc_tp, band = band2)
srp2 <- bandtest_coh2(object = cohl$coh_doc_srp, band = band2)
chla2 <- bandtest_coh2(object = cohl$coh_doc_chla, band = band2)
no32 <- bandtest_coh2(object = cohl$coh_doc_no3, band = band2)
nh42 <- bandtest_coh2(object = cohl$coh_doc_nh3, band = band2)
qld2 <- bandtest_coh2(object = cohl$coh_doc_dief, band = band2)
qlc2 <- bandtest_coh2(object = cohl$coh_doc_cat, band = band2)
qbp2 <- bandtest_coh2(object = cohl$coh_doc_bp, band = band2)

mso4 <- rbind(mso4, c(t(as.matrix(so42$bandp[, 3:5]))))
mtp <- rbind(mtp, c(t(as.matrix(tp2$bandp[, 3:5]))))
msrp <- rbind(msrp, c(t(as.matrix(srp2$bandp[, 3:5]))))
mchla <- rbind(mchla, c(t(as.matrix(chla2$bandp[, 3:5]))))
mno3 <- rbind(mno3, c(t(as.matrix(no32$bandp[, 3:5]))))
mnh4 <- rbind(mnh4, c(t(as.matrix(nh42$bandp[, 3:5]))))
mqld <- rbind(mqld, c(t(as.matrix(qld2$bandp[, 3:5]))))
mqlc <- rbind(mqlc, c(t(as.matrix(qlc2$bandp[, 3:5]))))
mqbp <- rbind(mqbp, c(t(as.matrix(qbp2$bandp[, 3:5]))))

mcoh <- rbind(mso4, mtp, msrp, mchla, mno3, mnh4, mqld, mqlc, mqbp)
mcoh_df <- as.data.frame(mcoh)
colnames(mcoh_df) <- c("pval", "phi", "coh")
mcoh_df <- cbind(mcoh_df, mvars)
mcoh_df$sin <- sin(mcoh_df$phi)
mcoh_df$cos <- cos(mcoh_df$phi)
mcoh_df$timescale <- "6-month to 4-year timescales"

# significant (p < 0.05)
mcoh_sig <- subset(mcoh_df, pval < 0.05)
# SO4, TP, SRP, NH4, QLD, QBP 

## Cosine -- focuses on how close the relationship b/w DOC and driver is to
## being in phase
# 1. in-phase (phi = 0) == 1
# 2. anti-phase (phi = ± pi) == —1
# 3. quarter-phase (phi = ± pi/2) == 0
# 
## Sine -- focuses on whether the time-lagged relationship b/w DOC and driver 
## tends to be positive or negative
# 1. in-phase (phi = 0) and anti-phase (± pi) == 0
# 2. time-lagged positive (DOC 1/4 cycle behind driver) == —1
# 3. time-lagged negative (DOC 1/4 cycle ahead of driver) == 1

# 1. SO4 -- cos = 0.6 (nearing-phase to in-phase), sin = -0.8 (DOC lags behind) // coh = 0.47
# 2. TP  -- cos = 0.7 (nearing in-phase), sin = 0.7 (DOC leads ahead) // coh = 0.56
# 3. SRP -- cos = 0.7 (nearing to in-phase), sin = 0.7 (DOC leads ahead) // coh = 0.33
# 4. NH4 -- cos = 0.8 (nearing in-phase), sin = 0.6 (DOC leads ahead) // coh = 0.26
# 5. QLD -- cos = -0.4 (quarter-phase to anti-phase), sin = -0.9 (DOC lags behind) // coh = 0.38
# 6. QBP -- cos = -0.7 (nearing anti-phase), sin = -0.7 (DOC lags behind) // coh = 0.32

par(mfrow = c(1, 1), mar = c(1.5, 1, 4, 1))
rose(mcoh_df$phi,
     unit = "radian", main = "0.5–4 year timescales", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive (behind)", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -3.5)
mtext("lagged negative (ahead)", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -3.5)

# // > 4 years ------------------------------------------------------------
# long timescale denoted with 'l'
lso4 <- NULL
ltp <- NULL
lsrp <- NULL
lchla <- NULL
lno3 <- NULL
lnh4 <- NULL
lqld <- NULL
lqlc <- NULL
lqbp <- NULL

so43 <- bandtest_coh2(object = cohl$coh_doc_so4, band = band3)
tp3 <- bandtest_coh2(object = cohl$coh_doc_tp, band = band3)
srp3 <- bandtest_coh2(object = cohl$coh_doc_srp, band = band3)
chla3 <- bandtest_coh2(object = cohl$coh_doc_chla, band = band3)
no33 <- bandtest_coh2(object = cohl$coh_doc_no3, band = band3)
nh43 <- bandtest_coh2(object = cohl$coh_doc_nh3, band = band3)
qld3 <- bandtest_coh2(object = cohl$coh_doc_dief, band = band3)
qlc3 <- bandtest_coh2(object = cohl$coh_doc_cat, band = band3)
qbp3 <- bandtest_coh2(object = cohl$coh_doc_bp, band = band3)

lso4 <- rbind(lso4, c(t(as.matrix(so43$bandp[, 3:5]))))
ltp <- rbind(ltp, c(t(as.matrix(tp3$bandp[, 3:5]))))
lsrp <- rbind(lsrp, c(t(as.matrix(srp3$bandp[, 3:5]))))
lchla <- rbind(lchla, c(t(as.matrix(chla3$bandp[, 3:5]))))
lno3 <- rbind(lno3, c(t(as.matrix(no33$bandp[, 3:5]))))
lnh4 <- rbind(lnh4, c(t(as.matrix(nh43$bandp[, 3:5]))))
lqld <- rbind(lqld, c(t(as.matrix(qld3$bandp[, 3:5]))))
lqlc <- rbind(lqlc, c(t(as.matrix(qlc3$bandp[, 3:5]))))
lqbp <- rbind(lqbp, c(t(as.matrix(qbp3$bandp[, 3:5]))))

lcoh <- rbind(lso4, ltp, lsrp, lchla, lno3, lnh4, lqld, lqlc, lqbp)
lcoh_df <- as.data.frame(lcoh)
colnames(lcoh_df) <- c("pval", "phi", "coh")
lcoh_df <- cbind(lcoh_df, lvars)
lcoh_df$sin <- sin(lcoh_df$phi)
lcoh_df$cos <- cos(lcoh_df$phi)
lcoh_df$timescale <- "4- to 10-year timescales"

# significant (p < 0.05)
lcoh_sig <- subset(lcoh_df, pval < 0.05)
# SO4

## Cosine -- focuses on how close the relationship b/w DOC and driver is to
## being in phase
# 1. in-phase (phi = 0) == 1
# 2. anti-phase (phi = ± pi) == —1
# 3. quarter-phase (phi = ± pi/2) == 0
# 
## Sine -- focuses on whether the time-lagged relationship b/w DOC and driver 
## tends to be positive or negative
# 1. in-phase (phi = 0) and anti-phase (± pi) == 0
# 2. time-lagged positive (DOC 1/4 cycle behind driver) == —1
# 3. time-lagged negative (DOC 1/4 cycle ahead of driver) == 1

# SO4 -- cos = ~1 (in-phase), sin = ~0 (in-phase) // coh = 0.74

par(mfrow = c(1, 1), mar = c(1.5, 1, 4, 1))
rose(lcoh_df$phi,
     unit = "radian", main = "> 4 year timescales", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive (behind)", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -3.5)
mtext("lagged negative (ahead)", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -3.5)





# // all timescales -------------------------------------------------------
# all timescales denoted with 'a'
aso4 <- NULL
atp <- NULL
asrp <- NULL
achla <- NULL
ano3 <- NULL
anh4 <- NULL
aqld <- NULL
aqlc <- NULL
aqbp <- NULL

so44 <- bandtest_coh2(object = cohl$coh_doc_so4, band = band4)
tp4 <- bandtest_coh2(object = cohl$coh_doc_tp, band = band4)
srp4 <- bandtest_coh2(object = cohl$coh_doc_srp, band = band4)
chla4 <- bandtest_coh2(object = cohl$coh_doc_chla, band = band4)
no34 <- bandtest_coh2(object = cohl$coh_doc_no3, band = band4)
nh44 <- bandtest_coh2(object = cohl$coh_doc_nh3, band = band4)
qld4 <- bandtest_coh2(object = cohl$coh_doc_dief, band = band4)
qlc4 <- bandtest_coh2(object = cohl$coh_doc_cat, band = band4)
qbp4 <- bandtest_coh2(object = cohl$coh_doc_bp, band = band4)

aso4 <- rbind(aso4, c(t(as.matrix(so44$bandp[, 3:5]))))
atp <- rbind(atp, c(t(as.matrix(tp4$bandp[, 3:5]))))
asrp <- rbind(asrp, c(t(as.matrix(srp4$bandp[, 3:5]))))
achla <- rbind(achla, c(t(as.matrix(chla4$bandp[, 3:5]))))
ano3 <- rbind(ano3, c(t(as.matrix(no34$bandp[, 3:5]))))
anh4 <- rbind(anh4, c(t(as.matrix(nh44$bandp[, 3:5]))))
aqld <- rbind(aqld, c(t(as.matrix(qld4$bandp[, 3:5]))))
aqlc <- rbind(aqlc, c(t(as.matrix(qlc4$bandp[, 3:5]))))
aqbp <- rbind(aqbp, c(t(as.matrix(qbp4$bandp[, 3:5]))))

acoh <- rbind(aso4, atp, asrp, achla, ano3, anh4, aqld, aqlc, aqbp)
acoh_df <- as.data.frame(acoh)
colnames(acoh_df) <- c("pval", "phi", "coh")
acoh_df <- cbind(acoh_df, avars)
acoh_df$sin <- sin(acoh_df$phi)
acoh_df$cos <- cos(acoh_df$phi)
acoh_df$timescale <- "2-month to 10-year timescales"

# significant (p < 0.05)
acoh_sig <- subset(acoh_df, pval < 0.05)

## Cosine -- focuses on how close the relationship b/w DOC and driver is to
## being in phase
# 1. in-phase (phi = 0) == 1
# 2. anti-phase (phi = ± pi) == —1
# 3. quarter-phase (phi = ± pi/2) == 0
# 
## Sine -- focuses on whether the time-lagged relationship b/w DOC and driver 
## tends to be positive or negative
# 1. in-phase (phi = 0) and anti-phase (± pi) == 0
# 2. time-lagged positive (DOC 1/4 cycle behind driver) == —1
# 3. time-lagged negative (DOC 1/4 cycle ahead of driver) == 1

# 1. SO4 -- cos = 0.7 (in-phase), sin = -0.7 (DOC lags behind) // coh = 0.33
# 2. TP  -- cos = 0.9 (in-phase), sin = 0.4 (DOC leads ahead) // coh = 0.50
# 3. Chl a -- cos = 0.9 (in-phase), sin = 0.5 (DOC leads ahead) // coh = 0.42
# 4. NH4 -- cos = 0.9 (in-phase), sin = 0.4 (DOC leads ahead) // coh = 0.16
# 5. QBP -- cos = -0.8 (anti-phase), sin = -0.6 (DOC lags behind) // coh = 0.32

par(mfrow = c(1, 1), mar = c(1.5, 1, 4, 1))
rose(acoh_df$phi,
     unit = "radian", main = "All timescales\n(2 months to 10 years)", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive (behind)", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -3.5)
mtext("lagged negative (ahead)", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -3.5)




# // Rose plots -----------------------------------------------------------

# //// Significant --------------------------------------------------------
tiff("R_wavelet/outputs/figures/p_rose.tif", units = "in", res = 150, width = 8, height = 7)

laymat <- matrix(1, nrow = 2, ncol = 2)
laymat[1, ] <- 1:2
laymat[2, ] <- 3:4
layout(laymat)
par(mar = c(1.5, 0, 4, 0), mgp = c(3, 1, 0), oma = c(1, 0, 0, 0))

# a)
rose(scoh_sig$phi,
     unit = "radian", main = "2- to 6-month timescales", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive (behind)", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -4.5)
mtext("lagged negative (ahead)", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -4.5)
mtext(expression(bold("a")), side = 3, adj = 0.1, padj = -4)


# b)
rose(mcoh_sig$phi,
     unit = "radian", main = "6-month to 4-year timescales", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive (behind)", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -4.5)
mtext("lagged negative (ahead)", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -4.5)
mtext(expression(bold("b")), side = 3, adj = 0.1, padj = -3)

# c)
rose(lcoh_sig$phi,
     unit = "radian", main = "4- to 10-year timescales", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive (behind)", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -4.5)
mtext("lagged negative (ahead)", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -4.5)
mtext(expression(bold("c")), side = 3, adj = 0.1, padj = -4)

# d)
rose(acoh_sig$phi,
     unit = "radian", main = "2-month to 10-year timescales", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive (behind)", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -4.5)
mtext("lagged negative (ahead)", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -4.5)
mtext(expression(bold("d")), side = 3, adj = 0.1, padj = -3)

dev.off()


# //// All ----------------------------------------------------------------


# rbind(scoh_df, mcoh_df, lcoh_df, acoh_df)

# tiff("R_wavelet/outputs/figures/p_rose_all.tif",
#      units = "in", res = 150, width = 8, height = 7)

laymat <- matrix(1, nrow = 2, ncol = 2)
laymat[1, ] <- 1:2
laymat[2, ] <- 3:4
layout(laymat)
par(mar = c(1.5, 0, 4, 0), mgp = c(3, 1, 0), oma = c(1, 0, 0, 0))

# a)
rose(scoh_df$phi,
     unit = "radian", main = "2- to 6-month timescales", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive (behind)", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -4.5)
mtext("lagged negative (ahead)", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -4.5)
mtext(expression(bold("a")), side = 3, adj = 0.1, padj = -4)


# b)
rose(mcoh_df$phi,
     unit = "radian", main = "6-month to 4-year timescales", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive (behind)", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -4.5)
mtext("lagged negative (ahead)", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -4.5)
mtext(expression(bold("b")), side = 3, adj = 0.1, padj = -3)

# c)
rose(lcoh_df$phi,
     unit = "radian", main = "4- to 10-year timescales", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive (behind)", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -4.5)
mtext("lagged negative (ahead)", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -4.5)
mtext(expression(bold("c")), side = 3, adj = 0.1, padj = -4)

# d)
rose(acoh_df$phi,
     unit = "radian", main = "2-month to 10-year timescales", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive (behind)", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -4.5)
mtext("lagged negative (ahead)", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -4.5)
mtext(expression(bold("d")), side = 3, adj = 0.1, padj = -3)

# dev.off()


# // Average coherence plots ----------------------------------------------

names(scoh_df)[names(scoh_df) == "svars"] <- "var"
names(mcoh_df)[names(mcoh_df) == "mvars"] <- "var"
names(lcoh_df)[names(lcoh_df) == "lvars"] <- "var"
names(acoh_df)[names(acoh_df) == "avars"] <- "var"
coh_df <- rbind(scoh_df, mcoh_df, lcoh_df, acoh_df)
coh_sig <- subset(coh_df, pval < 0.05)
# write.csv(coh_df, "./R_wavelet/outputs/data/coherence.csv")

# tiff("R_wavelet/outputs/figures/p_coherence_histogram.tif", 
#      units = "in", res = 150, width = 7, height = 6.5)

par(mfrow = c(2, 2))
hist(coh_df$coh[coh_df$timescale == "2- to 6-month timescales"], 
     main = "2- to 6-month timescales", xlab = "Coherence")
mtext(expression(bold("a")), side = 3, adj = -0.15, padj = -4)
hist(coh_df$coh[coh_df$timescale == "6-month to 4-year timescales"], 
     main = "6-month to 4-year timescales", xlab = "Coherence")
mtext(expression(bold("b")), side = 3, adj = -0.15, padj = -3)
hist(coh_df$coh[coh_df$timescale == "4- to 10-year timescales"], 
     main = "4- to 10-year timescales", xlab = "Coherence")
mtext(expression(bold("c")), side = 3, adj = -0.15, padj = -4)
hist(coh_df$coh[coh_df$timescale == "2-month to 10-year timescales"], 
     main = "2-month to 10-year timescales", xlab = "Coherence")
mtext(expression(bold("d")), side = 3, adj = -0.15, padj = -3)

# dev.off()

hist(coh_sig$coh)

coh_df %>% 
  ggplot(aes(var, pval)) + 
  facet_wrap(~ timescale, ncol = 2) +
  geom_col()

coh_sig %>% 
  ggplot(aes(var, pval)) + 
  facet_wrap(~ timescale, ncol = 2) +
  geom_col() + 
  ylim(c(NA, 0.05))


# Correlation between coherences at different timescales ------------------

cor(scoh_df$coh, mcoh_df$coh, method = "pearson") # 0.17
cor(scoh_df$coh, lcoh_df$coh, method = "pearson") # —0.017
cor(scoh_df$coh, acoh_df$coh, method = "pearson") # 0.38
cor(mcoh_df$coh, lcoh_df$coh, method = "pearson") # 0.76
cor(mcoh_df$coh, acoh_df$coh, method = "pearson") # 0.84
cor(lcoh_df$coh, acoh_df$coh, method = "pearson") # 0.84



# Coherence at <= 1.5 and > 1.5 years -----------------------------------------

# Wavelet transform
tiff("R_wavelet/outputs/figures/p_wt.tif", units = "in", res = 150, width = 11, height = 5)

laymat <- matrix(1, nrow = 2, ncol = 5)
laymat[1, ] <- 1:5
laymat[2, ] <- 6:10
layout(laymat)
par(mar = c(1.5, 1.5, 2, 1.5), mgp = c(3, 1, 0), oma = c(3, 3, 0, 1))

plotmag_wt_dr(wtl$wt_doc, xaxs = "r", yaxs = "r", colorbar = FALSE, zlims = c(0, 8),
              ylocs = c(0, 6, 12, 24, 48, 96, 144),
              ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
              xlocs = seq(0, 360, by = 60),
              xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
text(50, 7, substitute(paste(bold("a) DOC"))), cex = 1.1)

plotmag_wt_dr(wtl$wt_so4, xaxs = "r", yaxs = "r", colorbar = FALSE, zlims = c(0, 8),
              ylocs = c(0, 6, 12, 24, 48, 96, 144),
              ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
              xlocs = seq(0, 360, by = 60),
              xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
text(50, 7, expression(bold("b) SO"["4"]^" 2–")), cex = 1.1)

plotmag_wt_dr(wtl$wt_tp, xaxs = "r", yaxs = "r", colorbar = FALSE, zlims = c(0, 8),
              ylocs = c(0, 6, 12, 24, 48, 96, 144),
              ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
              xlocs = seq(0, 360, by = 60),
              xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
text(50, 7, substitute(paste(bold("c) TP"))), cex = 1.1)

plotmag_wt_dr(wtl$wt_srp, xaxs = "r", yaxs = "r", colorbar = FALSE, zlims = c(0, 8),
              ylocs = c(0, 6, 12, 24, 48, 96, 144),
              ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
              xlocs = seq(0, 360, by = 60),
              xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
text(50, 7, substitute(paste(bold("d) SRP"))), cex = 1.1)

plotmag_wt_dr(wtl$wt_chla, xaxs = "r", yaxs = "r", colorbar = TRUE, zlims = c(0, 8),
              ylocs = c(0, 6, 12, 24, 48, 96, 144),
              ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
              xlocs = seq(0, 360, by = 60),
              xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
text(50, 7, expression(paste(bold("e) Chl"), bolditalic(" a"))), cex = 1.1)

plotmag_wt_dr(wtl$wt_no3, xaxs = "r", yaxs = "r", colorbar = FALSE, zlims = c(0, 8),
              ylocs = c(0, 6, 12, 24, 48, 96, 144),
              ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
              xlocs = seq(0, 360, by = 60),
              xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
text(50, 7, expression(bold("f) NO"[3]^" –")), cex = 1.1)

plotmag_wt_dr(wtl$wt_nh3, xaxs = "r", yaxs = "r", colorbar = FALSE, zlims = c(0, 8),
              ylocs = c(0, 6, 12, 24, 48, 96, 144),
              ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
              xlocs = seq(0, 360, by = 60),
              xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
text(50, 7, expression(bold("g) NH"[4]^" +")), cex = 1.1)

plotmag_wt_dr(wtl$wt_dief, xaxs = "r", yaxs = "r", colorbar = FALSE, zlims = c(0, 8),
              ylocs = c(0, 6, 12, 24, 48, 96, 144),
              ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
              xlocs = seq(0, 360, by = 60),
              xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
text(50, 7, expression(paste(bold("h)"), bolditalic(" Q"), bold(""["LD"]))), cex = 1.1)

plotmag_wt_dr(wtl$wt_bp, xaxs = "r", yaxs = "r", colorbar = FALSE, zlims = c(0, 8),
              ylocs = c(0, 6, 12, 24, 48, 96, 144),
              ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
              xlocs = seq(0, 360, by = 60),
              xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
text(50, 7, expression(paste(bold("i)"), bolditalic(" Q"), bold(""["BP"]))), cex = 1.1)

plotmag_wt_dr(wtl$wt_cat, xaxs = "r", yaxs = "r", colorbar = TRUE, zlims = c(0, 8),
              ylocs = c(0, 6, 12, 24, 48, 96, 144),
              ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
              xlocs = seq(0, 360, by = 60),
              xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
text(50, 7, expression(paste(bold("j)"), bolditalic(" Q"), bold(""["LC"]))), cex = 1.1)

mtext("Year", side = 1, outer = T, line = 1.2)
mtext("Timescale (years)", 2, outer = T, line = 1)

dev.off()


cohp <- cohl
band5 <- c(2, 18)
band6 <- c(18, Inf) 
cohp <- lapply(FUN = function(x){bandtest(object = x, band = band6)}, X = cohp)
cohp <- lapply(FUN = function(x){bandtest(object = x, band = band5)}, X = cohp)

# tiff("R_wavelet/outputs/figures/p_coh.tif", units = "in", res = 150, width = 9, height = 7)

laymat <- matrix(1, nrow = 3, ncol = 3)
laymat[1, ] <- 1:3
laymat[2, ] <- 4:6
laymat[3, ] <- 7:9
layout(laymat)
par(mar = c(3.5, 1.5, 2, 2), mgp = c(3, 1, 0), oma = c(3, 5, 1, 2))
# par(mar = c(5, 5, 3, 1), mgp = c(3, 1, 0), oma = c(0, 0, 0, 0))

plotmag_coh_dr(cohp$coh_doc_so4, sigthresh = 0.95); mtext(expression(bold("a) SO"["4"]^" 2–")), side = 3, adj = 0, padj = -0.2)
plotmag_coh_dr(cohp$coh_doc_tp, sigthresh = 0.95); mtext(expression(bold("b) TP")), side = 3, adj = 0, padj = -0.2)
plotmag_coh_dr(cohp$coh_doc_srp, sigthresh = 0.95); mtext(expression(bold("c) SRP")), side = 3, adj = 0, padj = -0.2)
plotmag_coh_dr(cohp$coh_doc_chla, sigthresh = 0.95); mtext(expression(paste(bold("d) Chl"), bolditalic(" a"))), side = 3, adj = 0, padj = -0.2)
plotmag_coh_dr(cohp$coh_doc_no3, sigthresh = 0.95); mtext(expression(bold("e) NO"["3"]^" –")), side = 3, adj = 0, padj = -0.2)
plotmag_coh_dr(cohp$coh_doc_nh3, sigthresh = 0.95); mtext(expression(bold("f) NH"["4"]^" +")), side = 3, adj = 0, padj = -0.2)
plotmag_coh_dr(cohp$coh_doc_dief, sigthresh = 0.95); mtext(expression(paste(bold("g)"), bolditalic(" Q"), bold(""["LD"]))), side = 3, adj = 0, padj = -0.2)
plotmag_coh_dr(cohp$coh_doc_bp, sigthresh = 0.95); mtext(expression(paste(bold("h)"), bolditalic(" Q"), bold(""["BP"]))), side = 3, adj = 0, padj = -0.2)
plotmag_coh_dr(cohp$coh_doc_cat, sigthresh = 0.95); mtext(expression(paste(bold("i)"), bolditalic(" Q"), bold(""["LC"]))), side = 3, adj = 0, padj = -0.2)

mtext("Timescale (months)", side = 1, outer = T, line = 0.1)
mtext("Timescale (months)", side = 1, outer = T, line = 0.1, adj = 0.1)
mtext("Timescale (months)", side = 1, outer = T, line = 0.1, adj = 0.9)
mtext("Coherence", 2, outer = T, line = 2, adj = 0.52)
mtext("Coherence", 2, outer = T, line = 2, adj = 0.14)
mtext("Coherence", 2, outer = T, line = 2, adj = 0.9)

# dev.off()

# plotrank()
# The vertical axis label Fract surr gt stands for the fraction of surrogate
# coherences that the coherence of the data is greater than at the given
# timescale, so values are between 0 and 1 and large values indicate
# significance
# 
tiff("R_wavelet/outputs/figures/p_plotrank.tif", units = "in", res = 150, width = 9, height = 7)

laymat <- matrix(1, nrow = 3, ncol = 3)
laymat[1, ] <- 1:3
laymat[2, ] <- 4:6
laymat[3, ] <- 7:9
layout(laymat)
par(mar = c(3.0, 1.5, 2, 1.5), mgp = c(3, 1, 0), oma = c(3, 5, 1, 2))

plotrank(cohp$coh_doc_so4, sigthresh = 0.95); mtext(expression(bold("a) SO"["4"]^" 2–")), side = 3, adj = 0, padj = -0.2)
plotrank(cohp$coh_doc_tp, sigthresh = 0.95); mtext(expression(bold("b) TP")), side = 3, adj = 0, padj = -0.2)
plotrank(cohp$coh_doc_srp, sigthresh = 0.95); mtext(expression(bold("c) SRP")), side = 3, adj = 0, padj = -0.2)
plotrank(cohp$coh_doc_chla, sigthresh = 0.95); mtext(expression(paste(bold("d) Chl"), bolditalic(" a"))), side = 3, adj = 0, padj = -0.2)
plotrank(cohp$coh_doc_no3, sigthresh = 0.95); mtext(expression(bold("e) NO"["3"]^" –")), side = 3, adj = 0, padj = -0.2)
plotrank(cohp$coh_doc_nh3, sigthresh = 0.95); mtext(expression(bold("f) NH"["4"]^" +")), side = 3, adj = 0, padj = -0.2)
plotrank(cohp$coh_doc_dief, sigthresh = 0.95); mtext(expression(paste(bold("g)"), bolditalic(" Q"), bold(""["LD"]))), side = 3, adj = 0, padj = -0.2)
plotrank(cohp$coh_doc_bp, sigthresh = 0.95); mtext(expression(paste(bold("h)"), bolditalic(" Q"), bold(""["BP"]))), side = 3, adj = 0, padj = -0.2)
plotrank(cohp$coh_doc_cat, sigthresh = 0.95); mtext(expression(paste(bold("i)"), bolditalic(" Q"), bold(""["LC"]))), side = 3, adj = 0, padj = -0.2)

mtext("Timescale (months)", side = 1, outer = T, line = 0.1)
mtext("Timescale (months)", side = 1, outer = T, line = 0.1, adj = 0.1)
mtext("Timescale (months)", side = 1, outer = T, line = 0.1, adj = 0.9)
mtext("Fract surr gt", 2, outer = T, line = 2, adj = 0.52)
mtext("Fract surr gt", 2, outer = T, line = 2, adj = 0.14)
mtext("Fract surr gt", 2, outer = T, line = 2, adj = 0.9)

dev.off()

cohp <- cohl
band5 <- c(2, 18)
band6 <- c(18, 120)
vars <- c("DOC_SO4", "DOC_TP", "DOC_SRP", "DOC_Chla", 
          "DOC_NO3", "DOC_NH4", "DOC_QLD", "DOC_QLC", "DOC_QBP")

shortso4 <- NULL
shorttp <- NULL
shortsrp <- NULL
shortchla <- NULL
shortno3 <- NULL
shortnh4 <- NULL
shortqld <- NULL
shortqlc <- NULL
shortqbp <- NULL

so45 <- bandtest_coh2(object = cohp$coh_doc_so4, band = band5)
tp5 <- bandtest_coh2(object = cohp$coh_doc_tp, band = band5)
srp5 <- bandtest_coh2(object = cohp$coh_doc_srp, band = band5)
chla5 <- bandtest_coh2(object = cohp$coh_doc_chla, band = band5)
no35 <- bandtest_coh2(object = cohp$coh_doc_no3, band = band5)
nh45 <- bandtest_coh2(object = cohp$coh_doc_nh3, band = band5)
qld5 <- bandtest_coh2(object = cohp$coh_doc_dief, band = band5)
qlc5 <- bandtest_coh2(object = cohp$coh_doc_cat, band = band5)
qbp5 <- bandtest_coh2(object = cohp$coh_doc_bp, band = band5)

shortso4 <- rbind(shortso4, c(t(as.matrix(so45$bandp[, 3:5]))))
shorttp <- rbind(shorttp, c(t(as.matrix(tp5$bandp[, 3:5]))))
shortsrp <- rbind(shortsrp, c(t(as.matrix(srp5$bandp[, 3:5]))))
shortchla <- rbind(shortchla, c(t(as.matrix(chla5$bandp[, 3:5]))))
shortno3 <- rbind(shortno3, c(t(as.matrix(no35$bandp[, 3:5]))))
shortnh4 <- rbind(shortnh4, c(t(as.matrix(nh45$bandp[, 3:5]))))
shortqld <- rbind(shortqld, c(t(as.matrix(qld5$bandp[, 3:5]))))
shortqlc <- rbind(shortqlc, c(t(as.matrix(qlc5$bandp[, 3:5]))))
shortqbp <- rbind(shortqbp, c(t(as.matrix(qbp5$bandp[, 3:5]))))

shortcoh <- rbind(shortso4, shorttp, shortsrp, shortchla, shortno3, shortnh4, shortqld, shortqlc, shortqbp)
shortcoh_df <- as.data.frame(shortcoh)
colnames(shortcoh_df) <- c("pval", "phi", "coh")
shortcoh_df <- cbind(shortcoh_df, vars)
shortcoh_df$sin <- sin(shortcoh_df$phi)
shortcoh_df$cos <- cos(shortcoh_df$phi)
shortcoh_df$timescale <- "Short timescales"

# significant (p < 0.05)
shortcoh_sig <- subset(shortcoh_df, pval < 0.05)

longso4 <- NULL
longtp <- NULL
longsrp <- NULL
longchla <- NULL
longno3 <- NULL
longnh4 <- NULL
longqld <- NULL
longqlc <- NULL
longqbp <- NULL

so46 <- bandtest_coh2(object = cohp$coh_doc_so4, band = band6)
tp6 <- bandtest_coh2(object = cohp$coh_doc_tp, band = band6)
srp6 <- bandtest_coh2(object = cohp$coh_doc_srp, band = band6)
chla6 <- bandtest_coh2(object = cohp$coh_doc_chla, band = band6)
no36 <- bandtest_coh2(object = cohp$coh_doc_no3, band = band6)
nh46 <- bandtest_coh2(object = cohp$coh_doc_nh3, band = band6)
qld6 <- bandtest_coh2(object = cohp$coh_doc_dief, band = band6)
qlc6 <- bandtest_coh2(object = cohp$coh_doc_cat, band = band6)
qbp6 <- bandtest_coh2(object = cohp$coh_doc_bp, band = band6)

longso4 <- rbind(longso4, c(t(as.matrix(so46$bandp[, 3:5]))))
longtp <- rbind(longtp, c(t(as.matrix(tp6$bandp[, 3:5]))))
longsrp <- rbind(longsrp, c(t(as.matrix(srp6$bandp[, 3:5]))))
longchla <- rbind(longchla, c(t(as.matrix(chla6$bandp[, 3:5]))))
longno3 <- rbind(longno3, c(t(as.matrix(no36$bandp[, 3:5]))))
longnh4 <- rbind(longnh4, c(t(as.matrix(nh46$bandp[, 3:5]))))
longqld <- rbind(longqld, c(t(as.matrix(qld6$bandp[, 3:5]))))
longqlc <- rbind(longqlc, c(t(as.matrix(qlc6$bandp[, 3:5]))))
longqbp <- rbind(longqbp, c(t(as.matrix(qbp6$bandp[, 3:5]))))

longcoh <- rbind(longso4, longtp, longsrp, longchla, longno3, longnh4, longqld, longqlc, longqbp)
longcoh_df <- as.data.frame(longcoh)
colnames(longcoh_df) <- c("pval", "phi", "coh")
longcoh_df <- cbind(longcoh_df, vars)
longcoh_df$sin <- sin(longcoh_df$phi)
longcoh_df$cos <- cos(longcoh_df$phi)
longcoh_df$timescale <- "Long timescales"

# significant (p < 0.05)
longcoh_sig <- subset(longcoh_df, pval < 0.05)

allcoh <- rbind(shortcoh_df, longcoh_df)
# cor(shortcoh_df$coh, longcoh_df$coh, method = "pearson") # —0.319
# plot(shortcoh_df$coh, longcoh_df$coh) # no relationship 
allcoh_sig <- rbind(shortcoh_sig, longcoh_sig)

min(allcoh$coh) # 0.051
max(allcoh$coh) # 0.74
mean(allcoh$coh) # 0.34
median(allcoh$coh) # 0.31

min(allcoh_sig$coh) # 0.21
max(allcoh_sig$coh) # 0.74
mean(allcoh_sig$coh) # 0.47
median(allcoh_sig$coh) # 0.50

cor.test(shortcoh_df$coh, longcoh_df$coh) # —0.324
plot(shortcoh_df$coh, longcoh_df$coh) # no obvious relationship 
cor.test(shortcoh_df$pval, longcoh_df$pval) # 0.144
plot(shortcoh_df$pval, longcoh_df$pval) # no obvious relationship 

tiff("R_wavelet/outputs/figures/p_coherence_histogram.tif",
     units = "in", res = 150, width = 9, height = 4)

par(mfrow = c(1, 2))
hist(allcoh$coh[allcoh$timescale == "Short timescales"], 
     main = "Short timescales", xlab = "Coherence")
mtext(expression(bold("a")), side = 3, adj = -0.15, padj = -4)
hist(allcoh$coh[allcoh$timescale == "Long timescales"], 
     main = "Long timescales", xlab = "Coherence")
mtext(expression(bold("b")), side = 3, adj = -0.15, padj = -3)

dev.off()

# Significant coherences

# tiff("R_wavelet/outputs/figures/p_rose.tif", units = "in", res = 150, width = 9, height = 4)

laymat <- matrix(1, nrow = 1, ncol = 2)
laymat[1, ] <- 1:2
layout(laymat)
par(mar = c(1.5, 0, 4, 0), mgp = c(3, 1, 0), oma = c(0, 0, 0, 0))

# a)
rose(shortcoh_sig$phi,
     unit = "radian", main = "Short timescales", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive (behind)", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -3.5)
mtext("lagged negative (ahead)", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -3.5)
mtext(expression(bold("a")), side = 3, adj = 0.1, padj = -4)


# b)
rose(longcoh_sig$phi,
     unit = "radian", main = "Long timescales", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive (behind)", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -3.5)
mtext("lagged negative (ahead)", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -3.5)
mtext(expression(bold("b")), side = 3, adj = 0.1, padj = -3)

# dev.off()

# All coherences

# tiff("R_wavelet/outputs/figures/p_rose_all.tif", units = "in", res = 150, width = 9, height = 4)

laymat <- matrix(1, nrow = 1, ncol = 2)
laymat[1, ] <- 1:2
layout(laymat)
par(mar = c(1.5, 0, 4, 0), mgp = c(3, 1, 0), oma = c(0, 0, 0, 0))

# a)
rose(shortcoh_df$phi,
     unit = "radian", main = "Short timescales", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive (behind)", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -3.5)
mtext("lagged negative (ahead)", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -3.5)
mtext(expression(bold("a")), side = 3, adj = 0.1, padj = -4)


# b)
rose(longcoh_df$phi,
     unit = "radian", main = "Long timescales", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive (behind)", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -3.5)
mtext("lagged negative (ahead)", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -3.5)
mtext(expression(bold("b")), side = 3, adj = 0.1, padj = -3)

# dev.off()

# Coherence and phase (trial run) -----------------------------------------

# try timescales >= 1 year

coh_vars <- data.frame(
  pair = c("doc_tp", "doc_srp", "doc_so4",
           "doc_don", "doc_no3", "doc_nh3",
           "doc_bp", "doc_dief", "doc_cat") 
)

coh2_tp <- NULL
coh2_srp <- NULL
coh2_so4 <- NULL
coh2_don <- NULL
coh2_no3 <- NULL
coh2_nh3 <- NULL
coh2_bp <- NULL
coh2_dief <- NULL
coh2_cat <- NULL

tband <- c(12, Inf)
bt_tp <- bandtest_coh2(object = cohl$coh_doc_tp, band = tband)
bt_srp <- bandtest_coh2(object = cohl$coh_doc_srp, band = tband)
bt_so4 <- bandtest_coh2(object = cohl$coh_doc_so4, band = tband)
bt_don <- bandtest_coh2(object = cohl$coh_doc_don, band = tband)
bt_no3 <- bandtest_coh2(object = cohl$coh_doc_no3, band = tband)
bt_nh3 <- bandtest_coh2(object = cohl$coh_doc_nh3, band = tband)
bt_bp <- bandtest_coh2(object = cohl$coh_doc_bp, band = tband)
bt_dief <- bandtest_coh2(object = cohl$coh_doc_dief, band = tband)
bt_cat <- bandtest_coh2(object = cohl$coh_doc_cat, band = tband)

coh2_tp <- rbind(coh2_tp, c(t(as.matrix(bt_tp$bandp[, 3:5]))))
coh2_srp <- rbind(coh2_srp, c(t(as.matrix(bt_srp$bandp[, 3:5]))))
coh2_so4 <- rbind(coh2_so4, c(t(as.matrix(bt_so4$bandp[, 3:5]))))
coh2_don <- rbind(coh2_don, c(t(as.matrix(bt_don$bandp[, 3:5]))))
coh2_no3 <- rbind(coh2_no3, c(t(as.matrix(bt_no3$bandp[, 3:5]))))
coh2_nh3 <- rbind(coh2_nh3, c(t(as.matrix(bt_nh3$bandp[, 3:5]))))
coh2_bp <- rbind(coh2_bp, c(t(as.matrix(bt_bp$bandp[, 3:5]))))
coh2_dief <- rbind(coh2_dief, c(t(as.matrix(bt_dief$bandp[, 3:5]))))
coh2_cat <- rbind(coh2_cat, c(t(as.matrix(bt_cat$bandp[, 3:5]))))

coh2_m <- rbind(coh2_tp, coh2_srp, coh2_so4,
                coh2_don, coh2_no3, coh2_nh3,
                coh2_bp, coh2_dief, coh2_cat)
coh2_df <- as.data.frame(coh2_m)

colnames(coh2_df) <- c("pval", "phi", "coh")

coh2_res <- cbind(coh2_df, coh_vars)

# subset significant coh pairs
coh2_sig <- subset(coh2_res, pval <= 0.05)

# check distribution of coh pairs
quantile(coh2_res$coh)
hist(coh2_res$coh)
hist(coh2_sig$coh) # all significant mean coherences >= 0.55

# check proportion of significant coh
alpha <- 0.05
sum(coh2_res$pval < 0.05) / nrow(coh2_res) # 55.6%

# all
par(mfrow = c(1, 1))

rose(coh2_res$phi,
     unit = "radian", main = "Long timescale phase (>= 10 years)", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -3.5)
mtext("lagged negative", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -3.5)

# significant coherence
rose(coh2_sig$phi,
     unit = "radian", main = "Long timescale phase (>= 1 year)", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -3.5)
mtext("lagged negative", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -3.5)


coh2_sig
#         pval        phi       coh     pair
# 1 0.00629937  0.5386218 0.5787444   doc_tp
# 3 0.00069993 -0.4184438 0.5567860  doc_so4
# 4 0.00019998  0.2313394 0.6736080  doc_don
# 7 0.03899610 -2.1453982 0.5934492   doc_bp
# 8 0.00059994 -1.8886211 0.6620338 doc_dief

# TP = in-phase, lagged negative (b/w 0 and pi/4) -- DOC and TP oscillate together; DOC peaks ahead of TP
# SO4 = in-phase, lagged positive (b/w 0 and —pi/4) -- DOC and SO4 oscillate together; SO4 peaks ahead of DOC
# DON = in-phase, lagged negative (b/w 0 and pi/4) -- DOC and DON oscillate together; peaks ahead of DON (but lag is very small)
# BP inflow = anti-phase, lagged positive (b/w —pi/2 and —3pi/2) -- DOC and BP inflow DO NOT oscillate together; BP inflow peaks ahead of DOC
# Dief. outflow = anti-phase, lagged positive (b/w —pi/2 and —3pi/2) -- DOC and Dief. outflow DO NOT oscillate together; Dief. outflow peaks ahead of DOC  

### Sin- and cos-transformations to analyse phase difference between DOC and drivers 
#
## Cosine -- focuses on how close the relationship b/w DOC and driver is to
## being in phase
# 1. in-phase (phi = 0) == 1
# 2. anti-phase (phi = ± pi) == —1
# 3. quarter-phase (phi = ± pi/2) == 0
# 
## Sine -- focuses on whether the time-lagged relationship b/w DOC and driver 
## tends to be positive or negative
# 1. in-phase (phi = 0) and anti-phase (± pi) == 0
# 2. time-lagged positive (DOC 1/2 cycle behind driver) == —1
# 3. time-lagged negative (DOC 1/2 cycle ahead of driver) == 1

coh2_sig$sin_phi <- sin(coh2_sig$phi)
coh2_sig$cos_phi <- cos(coh2_sig$phi)
#         pval        phi       coh     pair    sin_phi    cos_phi
# 1 0.00629937  0.5386218 0.5787444   doc_tp  0.5129534  0.8584164
# 3 0.00069993 -0.4184438 0.5567860  doc_so4 -0.4063390  0.9137224
# 4 0.00019998  0.2313394 0.6736080  doc_don  0.2292815  0.9733602
# 7 0.03899610 -2.1453982 0.5934492   doc_bp -0.8394087 -0.5435007
# 8 0.00059994 -1.8886211 0.6620338 doc_dief -0.9499174 -0.3125010

# TP -- cos = 0.86 (close to in-phase); sin = 0.51 (closer to time-lagged negative)
# SO4 -- cos = 0.91 (close to in-phase); sin = —0.41 (closer to time-lagged positive)
# DON -- cos = 0.97 (in-phase, as expected); sin = 0.23 (close to in-phase with some negative lag)
# BP inflow -- cos = —0.54 (closer to anti-phase); sin = —0.84 (time-lagged positive)
# Dief outflow -- cos = —0.31 (closer to quarter-phase); sin = —0.95 (time-lagged positive)



# Test additional timescales ----------------------------------------------
pband1 <- c(48, Inf)
pband2 <- c(6, 48)
pband3 <- c(2, 6)
# pband4 <- c(120, Inf)

## Chl a
coh2_chla1 <- NULL
coh2_chla2 <- NULL
coh2_chla3 <- NULL
# coh2_chla4 <- NULL
chla1 <- bandtest_coh2(object = cohl$coh_doc_chla, band = pband1)
chla2 <- bandtest_coh2(object = cohl$coh_doc_chla, band = pband2)
chla3 <- bandtest_coh2(object = cohl$coh_doc_chla, band = pband3)
# chla4 <- bandtest_coh2(object = cohl$coh_doc_chla, band = pband4)

## TP
coh2_tp1 <- NULL
coh2_tp2 <- NULL
coh2_tp3 <- NULL
# coh2_tp4 <- NULL
tp1 <- bandtest_coh2(object = cohl$coh_doc_tp, band = pband1)
tp2 <- bandtest_coh2(object = cohl$coh_doc_tp, band = pband2)
tp3 <- bandtest_coh2(object = cohl$coh_doc_tp, band = pband3)
# tp4 <- bandtest_coh2(object = cohl$coh_doc_tp, band = pband4)

## SRP 
coh2_srp1 <- NULL
coh2_srp2 <- NULL
coh2_srp3 <- NULL
# coh2_srp4 <- NULL
srp1 <- bandtest_coh2(object = cohl$coh_doc_srp, band = pband1)
srp2 <- bandtest_coh2(object = cohl$coh_doc_srp, band = pband2)
srp3 <- bandtest_coh2(object = cohl$coh_doc_srp, band = pband3)
# srp4 <- bandtest_coh2(object = cohl$coh_doc_srp, band = pband4)

## SO4 
coh2_so41 <- NULL
coh2_so42 <- NULL
coh2_so43 <- NULL
# coh2_so44 <- NULL
so41 <- bandtest_coh2(object = cohl$coh_doc_so4, band = pband1)
so42 <- bandtest_coh2(object = cohl$coh_doc_so4, band = pband2)
so43 <- bandtest_coh2(object = cohl$coh_doc_so4, band = pband3)
# so44 <- bandtest_coh2(object = cohl$coh_doc_so4, band = pband4)

## DON
coh2_don1 <- NULL
coh2_don2 <- NULL
coh2_don3 <- NULL
# coh2_don4 <- NULL
don1 <- bandtest_coh2(object = cohl$coh_doc_don, band = pband1)
don2 <- bandtest_coh2(object = cohl$coh_doc_don, band = pband2)
don3 <- bandtest_coh2(object = cohl$coh_doc_don, band = pband3)
# don4 <- bandtest_coh2(object = cohl$coh_doc_don, band = pband4)

## NO3
coh2_no31 <- NULL
coh2_no32 <- NULL
coh2_no33 <- NULL
# coh2_no34 <- NULL
no31 <- bandtest_coh2(object = cohl$coh_doc_no3, band = pband1)
no32 <- bandtest_coh2(object = cohl$coh_doc_no3, band = pband2)
no33 <- bandtest_coh2(object = cohl$coh_doc_no3, band = pband3)
# no34 <- bandtest_coh2(object = cohl$coh_doc_no3, band = pband4)

## NH3
coh2_nh31 <- NULL
coh2_nh32 <- NULL
coh2_nh33 <- NULL
# coh2_nh34 <- NULL
nh31 <- bandtest_coh2(object = cohl$coh_doc_nh3, band = pband1)
nh32 <- bandtest_coh2(object = cohl$coh_doc_nh3, band = pband2)
nh33 <- bandtest_coh2(object = cohl$coh_doc_nh3, band = pband3)
# nh34 <- bandtest_coh2(object = cohl$coh_doc_nh3, band = pband4)

## BP inflow
coh2_bp1 <- NULL
coh2_bp2 <- NULL
coh2_bp3 <- NULL
# coh2_bp4 <- NULL
bp1 <- bandtest_coh2(object = cohl$coh_doc_bp, band = pband1)
bp2 <- bandtest_coh2(object = cohl$coh_doc_bp, band = pband2)
bp3 <- bandtest_coh2(object = cohl$coh_doc_bp, band = pband3)
# bp4 <- bandtest_coh2(object = cohl$coh_doc_bp, band = pband4)

## Dief outflow
coh2_dief1 <- NULL
coh2_dief2 <- NULL
coh2_dief3 <- NULL
# coh2_dief4 <- NULL
dief1 <- bandtest_coh2(object = cohl$coh_doc_dief, band = pband1)
dief2 <- bandtest_coh2(object = cohl$coh_doc_dief, band = pband2)
dief3 <- bandtest_coh2(object = cohl$coh_doc_dief, band = pband3)
# dief4 <- bandtest_coh2(object = cohl$coh_doc_dief, band = pband4)

## Catchment flow
coh2_cat1 <- NULL
coh2_cat2 <- NULL
coh2_cat3 <- NULL
# coh2_cat4 <- NULL
cat1 <- bandtest_coh2(object = cohl$coh_doc_cat, band = pband1)
cat2 <- bandtest_coh2(object = cohl$coh_doc_cat, band = pband2)
cat3 <- bandtest_coh2(object = cohl$coh_doc_cat, band = pband3)
# cat4 <- bandtest_coh2(object = cohl$coh_doc_cat, band = pband4)

coh_vars <- data.frame(
  pair = c("doc_chla", "doc_tp", "doc_srp", "doc_so4", "doc_don", "doc_no3", 
           "doc_nh3", "doc_bp", "doc_dief", "doc_cat") 
)

## 1.5—3.5 years
coh2_chla1 <- rbind(coh2_chla1, c(t(as.matrix(chla1$bandp[, 3:5]))))
coh2_tp1 <- rbind(coh2_tp1, c(t(as.matrix(tp1$bandp[, 3:5]))))
coh2_srp1 <- rbind(coh2_srp1, c(t(as.matrix(srp1$bandp[, 3:5]))))
coh2_so41 <- rbind(coh2_so41, c(t(as.matrix(so41$bandp[, 3:5]))))
coh2_don1 <- rbind(coh2_don1, c(t(as.matrix(don1$bandp[, 3:5]))))
coh2_no31 <- rbind(coh2_no31, c(t(as.matrix(no31$bandp[, 3:5]))))
coh2_nh31 <- rbind(coh2_nh31, c(t(as.matrix(nh31$bandp[, 3:5]))))
coh2_bp1 <- rbind(coh2_bp1, c(t(as.matrix(bp1$bandp[, 3:5]))))
coh2_dief1 <- rbind(coh2_dief1, c(t(as.matrix(dief1$bandp[, 3:5]))))
coh2_cat1 <- rbind(coh2_cat1, c(t(as.matrix(cat1$bandp[, 3:5]))))

coh2_m1 <- rbind(coh2_chla1, coh2_tp1, coh2_srp1, coh2_so41, coh2_don1, 
                 coh2_no31, coh2_nh31, coh2_bp1, coh2_dief1, coh2_cat1)
coh2_df1 <- as.data.frame(coh2_m1)

colnames(coh2_df1) <- c("pval", "phi", "coh")

coh2_res1 <- cbind(coh2_df1, coh_vars)


## 3.5—6 years
coh2_chla2 <- rbind(coh2_chla2, c(t(as.matrix(chla2$bandp[, 3:5]))))
coh2_tp2 <- rbind(coh2_tp2, c(t(as.matrix(tp2$bandp[, 3:5]))))
coh2_srp2 <- rbind(coh2_srp2, c(t(as.matrix(srp2$bandp[, 3:5]))))
coh2_so42 <- rbind(coh2_so42, c(t(as.matrix(so42$bandp[, 3:5]))))
coh2_don2 <- rbind(coh2_don2, c(t(as.matrix(don2$bandp[, 3:5]))))
coh2_no32 <- rbind(coh2_no32, c(t(as.matrix(no32$bandp[, 3:5]))))
coh2_nh32 <- rbind(coh2_nh32, c(t(as.matrix(nh32$bandp[, 3:5]))))
coh2_bp2 <- rbind(coh2_bp2, c(t(as.matrix(bp2$bandp[, 3:5]))))
coh2_dief2 <- rbind(coh2_dief2, c(t(as.matrix(dief2$bandp[, 3:5]))))
coh2_cat2 <- rbind(coh2_cat2, c(t(as.matrix(cat2$bandp[, 3:5]))))

coh2_m2 <- rbind(coh2_chla2, coh2_tp2, coh2_srp2, coh2_so42, coh2_don2, 
                 coh2_no32, coh2_nh32, coh2_bp2, coh2_dief2, coh2_cat2)
coh2_df2 <- as.data.frame(coh2_m2)

colnames(coh2_df2) <- c("pval", "phi", "coh")

coh2_res2 <- cbind(coh2_df2, coh_vars)


## 7—10 years
coh2_chla3 <- rbind(coh2_chla3, c(t(as.matrix(chla3$bandp[, 3:5]))))
coh2_tp3 <- rbind(coh2_tp3, c(t(as.matrix(tp3$bandp[, 3:5]))))
coh2_srp3 <- rbind(coh2_srp3, c(t(as.matrix(srp3$bandp[, 3:5]))))
coh2_so43 <- rbind(coh2_so43, c(t(as.matrix(so43$bandp[, 3:5]))))
coh2_don3 <- rbind(coh2_don3, c(t(as.matrix(don3$bandp[, 3:5]))))
coh2_no33 <- rbind(coh2_no33, c(t(as.matrix(no33$bandp[, 3:5]))))
coh2_nh33 <- rbind(coh2_nh33, c(t(as.matrix(nh33$bandp[, 3:5]))))
coh2_bp3 <- rbind(coh2_bp3, c(t(as.matrix(bp3$bandp[, 3:5]))))
coh2_dief3 <- rbind(coh2_dief3, c(t(as.matrix(dief3$bandp[, 3:5]))))
coh2_cat3 <- rbind(coh2_cat3, c(t(as.matrix(cat3$bandp[, 3:5]))))

coh2_m3 <- rbind(coh2_chla3, coh2_tp3, coh2_srp3, coh2_so43, coh2_don3, 
                 coh2_no33, coh2_nh33, coh2_bp3, coh2_dief3, coh2_cat3)
coh2_df3 <- as.data.frame(coh2_m3)

colnames(coh2_df3) <- c("pval", "phi", "coh")

coh2_res3 <- cbind(coh2_df3, coh_vars)



coh2_res1$band <- "1.5 to 3.5 years"
coh2_res2$band <- "3.5 to 6 years"
coh2_res3$band <- "7—10 years"
# coh2_res4$band <- "> 10 years"

coh2_res_all <- rbind(coh2_res1, coh2_res2, coh2_res3)

coh2_res_sig <- subset(coh2_res_all, pval <= 0.05)
length(coh2_res_sig$pval)/length(coh2_res_all$pval) # 23.3%

coh2_res_sig$cos <- cos(coh2_res_sig$phi)
coh2_res_sig$sin <- sin(coh2_res_sig$phi)

sig_chla <- subset(coh2_res_sig, pair == "doc_chla") # 0
sig_tp <- subset(coh2_res_sig, pair == "doc_tp") # 1 (medium)
sig_srp <- subset(coh2_res_sig, pair == "doc_srp") # 1 (short)
sig_so4 <- subset(coh2_res_sig, pair == "doc_so4") # 3 (all)
sig_don <- subset(coh2_res_sig, pair == "doc_don") # 3 (all)
sig_no3 <- subset(coh2_res_sig, pair == "doc_no3") # 0
sig_nh3 <- subset(coh2_res_sig, pair == "doc_nh3") # 0
sig_bp <- subset(coh2_res_sig, pair == "doc_bp") # 0
sig_dief <- subset(coh2_res_sig, pair == "doc_dief") # 1 (short)
sig_cat <- subset(coh2_res_sig, pair == "doc_cat") # 0

# TP significant at 1—4 and 4—6 years
# SRP significant at 1—4 years
# SO4 significant at 1—4, 4—6, and > 6 years
# DON significant at 1—4, 4—6, and > 6 years
# Dief outflow at 1—4 years

# significant coherence plots
rose(coh2_res_sig$phi[coh2_res_sig$band == "1 to 4 years"],
     unit = "radian", main = "1 to 4 year timescale phase", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -3.5)
mtext("lagged negative", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -3.5)

rose(coh2_res_sig$phi[coh2_res_sig$band == "4 to 6 years"],
     unit = "radian", main = "4 to 6 year timescale phase", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -3.5)
mtext("lagged negative", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -3.5)

rose(coh2_res_sig$phi[coh2_res_sig$band == "8—10 years"],
     unit = "radian", main = "8 to 10 year timescale phase", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -3.5)
mtext("lagged negative", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -3.5)

rose(coh2_res_sig$phi[coh2_res_sig$band == "> 6 years"],
     unit = "radian", main = "> 6 year timescale phase", col = "lightgrey",
     breaks = c(0, pi/4, pi/2, 3*pi/4, pi, 5*pi/4, 3*pi/2, 7*pi/4, 2*pi),
     at = c(0, pi/4, pi/2, 3*pi/4, pi, -3*pi/4, -pi/2, -pi/4))
mtext("lagged positive", 1, col = "steelblue3", cex = 1)
mtext("negative (anti-phase)", 2, col = "steelblue3", cex = 1, line = -3.5)
mtext("lagged negative", 3, col = "steelblue3", cex = 1)
mtext("positive (in-phase)", 4, col = "steelblue3", cex = 1, line = -3.5)























