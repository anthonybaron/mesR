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

theme_set(theme_bw(base_size = 12) + theme(panel.background = element_blank())) 


# Source data -------------------------------------------------------------

source("./R_data-cleaning/bpwtp/code/clean-wavelet.R")

bp_drivers <- wavelet_data()


# Check autocorrelation in time series ------------------------------------

acf(bp_drivers$DOC_mg.L)
acf(bp_drivers$chla_ug.L)
acf(bp_drivers$TP_ug.L)
acf(bp_drivers$SRP_ug.L)
acf(bp_drivers$SO4_mg.L)
acf(bp_drivers$DON_mg.L)
acf(bp_drivers$NO3_mg.L)
acf(bp_drivers$NH3_mg.L)
acf(bp_drivers$SK05JG004_combined_cms)
acf(bp_drivers$SK05JG006_cms)
acf(bp_drivers$RC_IC_cms)


# Wavelet analyses --------------------------------------------------------

# Create vectors of each time series
ts_times <- rep(1:length(bp_drivers$date_ymd)) # length of time series (dates)
ts_doc <- bp_drivers$DOC_mg.L
ts_chla <- bp_drivers$chla_ug.L
ts_tp <- bp_drivers$TP_ug.L
ts_srp <- bp_drivers$SRP_ug.L
ts_so4 <- bp_drivers$SO4_mg.L
ts_don <- bp_drivers$DON_mg.L
ts_no3 <- bp_drivers$NO3_mg.L
ts_nh3 <- bp_drivers$NH3_mg.L
ts_bp <- bp_drivers$SK05JG004_combined_cms
ts_dief <- bp_drivers$SK05JG006_cms
ts_cat <- bp_drivers$RC_IC_cms

# Use wsyn::cleandat to apply an optimal Box-Cox normalization procedure 
# (transformation) to each time series, and linearly detrend, de-mean, and 
# standardize variances to 1.
ts_doc_boxcox <- cleandat(dat = ts_doc, times = ts_times, clev = 5)
ts_chla_boxcox <- cleandat(dat = ts_chla, times = ts_times, clev = 5)
ts_tp_boxcox <- cleandat(dat = ts_tp, times = ts_times, clev = 5)
ts_srp_boxcox <- cleandat(dat = ts_srp, times = ts_times, clev = 5)
ts_so4_boxcox <- cleandat(dat = ts_so4, times = ts_times, clev = 5)
ts_don_boxcox <- cleandat(dat = ts_don, times = ts_times, clev = 5)
ts_no3_boxcox <- cleandat(dat = ts_no3, times = ts_times, clev = 5)
ts_nh3_boxcox <- cleandat(dat = ts_nh3, times = ts_times, clev = 5)
ts_bp_boxcox <- cleandat(dat = ts_bp, times = ts_times, clev = 5)
ts_dief_boxcox <- cleandat(dat = ts_dief, times = ts_times, clev = 5)
ts_cat_boxcox <- cleandat(dat = ts_cat, times = ts_times, clev = 5)

# Apply wavelet transform using wsyn::wt and plot the magnitude of the transform
# against time and timescale.
wt_doc <- wt(ts_doc_boxcox$cdat, ts_times)
wt_chla <- wt(ts_chla_boxcox$cdat, ts_times)
wt_tp <- wt(ts_tp_boxcox$cdat, ts_times)
wt_srp <- wt(ts_srp_boxcox$cdat, ts_times)
wt_so4 <- wt(ts_so4_boxcox$cdat, ts_times)
wt_don <- wt(ts_don_boxcox$cdat, ts_times)
wt_no3 <- wt(ts_no3_boxcox$cdat, ts_times)
wt_nh3 <- wt(ts_nh3_boxcox$cdat, ts_times)
wt_bp <- wt(ts_bp_boxcox$cdat, ts_times)
wt_dief <- wt(ts_dief_boxcox$cdat, ts_times)
wt_cat <- wt(ts_cat_boxcox$cdat, ts_times)

plotmag_wt_dr <- function(object, zlims = NULL, neat = TRUE, colorfill = NULL,
                          colorbar = TRUE, title = NULL, filename = NA,
                          xlocs = NULL, ylocs = NULL, xlabs = NULL, ylabs = NULL, ...) {
  
  wav <- Mod(get_values(object))
  times <- get_times(object)
  timescales <- get_timescales(object)
  
  if (is.null(zlims)) {
    zlims <- range(wav, na.rm = TRUE)
  } else {
    rg <- range(wav, na.rm = TRUE)
    if (rg[1] < zlims[1] || rg[2] > zlims[2]) {
      stop("Error in plotmag.tts: zlims must encompass the z-axis range of what is being plotted")
    }
  }
  
  if (neat) {
    inds <- which(!is.na(colMeans(wav, na.rm = TRUE)))
    wav <- wav[, inds]
    timescales <- timescales[inds]
  }
  
  if (is.null(colorfill)) {
    jetcolors <- c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
    colorfill <- grDevices::colorRampPalette(jetcolors)
  }
  
  if (is.null(xlocs)) {
    xlocs <- pretty(times, n = 8)
  }
  
  if (is.null(ylocs)) {
    ylocs <- pretty(timescales, n = 8)
  }
  
  if (is.null(xlabs)) {
    xlabs <- xlocs
  }
  
  if (is.null(ylabs)) {
    ylabs <- ylocs
  }
  
  if (!is.na(filename)) {
    grDevices::pdf(paste0(filename, ".pdf"))
  }
  
  if (!colorbar) {
    graphics::image(x = times, y = log2(timescales), z = wav,
                    xlab= "", zlim = zlims, ylab = "Timescale (years)",
                    axes = FALSE, col = colorfill(100), main = title, ...)
    graphics::axis(1, at = xlocs, labels = xlabs)
    graphics::axis(2, at = log2(ylocs), labels = ylabs)
  } else {
    fields::image.plot(x = times, y = log2(timescales), z = wav,
                       xlab = "", zlim = zlims, ylab = "Timescale (years)",
                       axes = FALSE, col = colorfill(100), main = title, ...)
    graphics::axis(1, at = xlocs, labels = xlabs)
    graphics::axis(2, at = log2(ylocs), labels = ylabs)
  }
  
  if (!is.na(filename)) {
    grDevices::dev.off()
  }
}


# par(mfrow = c(3, 3))
# 
# par(mfrow = c(3, 3), 
#     mar = c(1.1, 3.5, 1.1, 1.1), 
#     oma = c(1.1, 0, 0, 0), 
#     mgp = c(2.2, 0.8, 0))

# par(mfrow = c(1, 1),
#     mar = c(1.6, 3.5, 2.1, 1.1), 
#     oma = c(1.6, 0, 1.1, 0), 
#     mgp = c(2.2, 0.8, 0))

plotmag_wt_dr(wt_doc, xaxs = "r", yaxs = "r", colorbar = TRUE, zlims = c(0, 8), 
              ylocs = c(0, 6, 12, 24, 48, 96, 144), 
              ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
              xlocs = seq(0, 360, by = 60),
              xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
title("DOC", font.main = 2)
# legend("topleft", "a", text.font = 2, bty = "n", cex = 1.15)
p1 <- recordPlot()

plotmag_wt_dr(wt_tp, xaxs = "r", yaxs = "r", colorbar = TRUE, zlims = c(0, 8), 
              ylocs = c(0, 6, 12, 24, 48, 96, 144), 
              ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
              xlocs = seq(0, 360, by = 60),
              xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
title("TP", font.main = 2)
# legend("topleft", "b", text.font = 2, bty = "n", cex = 1.15)
p2 <- recordPlot()

plotmag_wt_dr(wt_srp, xaxs = "r", yaxs = "r", colorbar = TRUE, zlims = c(0, 8), 
              ylocs = c(0, 6, 12, 24, 48, 96, 144), 
              ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
              xlocs = seq(0, 360, by = 60),
              xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
title("SRP", font.main = 2)
# legend("topleft", "c", text.font = 2, bty = "n", cex = 1.15)
p3 <- recordPlot()

plotmag_wt_dr(wt_so4, xaxs = "r", yaxs = "r", colorbar = TRUE, zlims = c(0, 8), 
              ylocs = c(0, 6, 12, 24, 48, 96, 144), 
              ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
              xlocs = seq(0, 360, by = 60),
              xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
title(expression(bold("SO"["4"]^" 2–")))
# legend("topleft", "d", text.font = 2, bty = "n", cex = 1.15)
p4 <- recordPlot()

plotmag_wt_dr(wt_no3, xaxs = "r", yaxs = "r", colorbar = TRUE, zlims = c(0, 8), 
              ylocs = c(0, 6, 12, 24, 48, 96, 144), 
              ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
              xlocs = seq(0, 360, by = 60),
              xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
title(expression(bold("NO"[3]*"–N")))
# legend("topleft", "e", text.font = 2, bty = "n", cex = 1.15)
p5 <- recordPlot()

plotmag_wt_dr(wt_nh3, xaxs = "r", yaxs = "r", colorbar = TRUE, zlims = c(0, 8), 
              ylocs = c(0, 6, 12, 24, 48, 96, 144), 
              ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
              xlocs = seq(0, 360, by = 60),
              xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
title(expression(bold("NH"[3]*"–N")))
# legend("topleft", "f", text.font = 2, bty = "n", cex = 1.15)
p6 <- recordPlot()

plotmag_wt_dr(wt_bp, xaxs = "r", yaxs = "r", colorbar = TRUE, zlims = c(0, 8), 
              ylocs = c(0, 6, 12, 24, 48, 96, 144), 
              ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
              xlocs = seq(0, 360, by = 60),
              xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
title("BP inflow", font.main = 2)
# legend("topleft", "g", text.font = 2, bty = "n", cex = 1.15)
p7 <- recordPlot()

plotmag_wt_dr(wt_dief, xaxs = "r", yaxs = "r", colorbar = TRUE, zlims = c(0, 8), 
              ylocs = c(0, 6, 12, 24, 48, 96, 144), 
              ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
              xlocs = seq(0, 360, by = 60),
              xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
title("Lake Diefenbaker outflow", font.main = 2)
# legend("topleft", "h", text.font = 2, bty = "n", cex = 1.15)
p8 <- recordPlot()

plotmag_wt_dr(wt_cat, xaxs = "r", yaxs = "r", colorbar = TRUE, zlims = c(0, 8), 
              ylocs = c(0, 6, 12, 24, 48, 96, 144), 
              ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
              xlocs = seq(0, 360, by = 60),
              xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
title("Catchment flows", font.main = 2)
# legend("topleft", "i", text.font = 2, bty = "n", cex = 1.15)
p9 <- recordPlot()

plotmag_wt_dr(wt_chla, xaxs = "r", yaxs = "r", colorbar = TRUE, zlims = c(0, 8), 
              ylocs = c(0, 6, 12, 24, 48, 96, 144), 
              ylabs = c(0, 0.5, 1, 2, 4, 8, 12),
              xlocs = seq(0, 360, by = 60),
              xlabs = c(1990, 1995, 2000, 2005, 2010, 2015, 2019))
title("Chlorophyll a", font.main = 2)
# legend("topleft", "a", text.font = 2, bty = "n", cex = 1.15)
p10 <- recordPlot()

plot_grid(p1, p2, p3,
          p4, p5, p6, 
          p7, p8, p9, p10,
          ncol = 4, 
          labels = NULL,
          label_size = 12,
          scale = 0.8,
          axis = "b")



par(mfrow = c(3, 3))
plotmag(wt_doc); title("DOC") # 1
plotmag(wt_tp); title("TP") # 2
plotmag(wt_srp); title("SRP") # 3
plotmag(wt_so4); title("SO4") # 4
plotmag(wt_don); title("DON") # 5 
plotmag(wt_nh3); title("NH3") # 6
plotmag(wt_bp); title("BP inflow") # 7
plotmag(wt_dief); title("L. Dief. outflow") # 8
plotmag(wt_cat); title("Catchment flows") # 9


# Then extract the numeric vectors for each cleaned time series to use with
# wavelet coherence analysis.
ts_doc_cleandat <- ts_doc_boxcox$cdat
ts_chla_cleandat <- ts_chla_boxcox$cdat
ts_tp_cleandat <- ts_tp_boxcox$cdat
ts_srp_cleandat <- ts_srp_boxcox$cdat
ts_so4_cleandat <- ts_so4_boxcox$cdat
ts_don_cleandat <- ts_don_boxcox$cdat
ts_no3_cleandat <- ts_no3_boxcox$cdat
ts_nh3_cleandat <- ts_nh3_boxcox$cdat
ts_bp_cleandat <- ts_bp_boxcox$cdat
ts_dief_cleandat <- ts_dief_boxcox$cdat
ts_cat_cleandat <- ts_cat_boxcox$cdat


# Coherence: DOC and drivers ----------------------------------------------

coh_dr <- function(dat1 = NULL, dat2 = NULL, times = NULL, norm = "powall",
                   sigmethod = "fast", nrand = 10000, sigma = 1.01) {
  
  coh_ob <- coh(dat1 = dat1, dat2 = dat2, times = times, norm = norm,
                sigmethod = sigmethod, nrand = nrand, sigma = sigma)
  
  return(coh_ob)
  
}

coh_doc_tp <- coh_dr(dat1 = ts_tp_cleandat, dat2 = ts_doc_cleandat, times = ts_times)
coh_doc_chla <- coh_dr(dat1 = ts_chla_cleandat, dat2 = ts_doc_cleandat, times = ts_times)
coh_doc_srp <- coh_dr(dat1 = ts_srp_cleandat, dat2 = ts_doc_cleandat, times = ts_times)
coh_doc_so4 <- coh_dr(dat1 = ts_so4_cleandat, dat2 = ts_doc_cleandat, times = ts_times)
coh_doc_don <- coh_dr(dat1 = ts_don_cleandat, dat2 = ts_doc_cleandat, times = ts_times)
coh_doc_no3 <- coh_dr(dat1 = ts_no3_cleandat, dat2 = ts_doc_cleandat, times = ts_times)
coh_doc_nh3 <- coh_dr(dat1 = ts_nh3_cleandat, dat2 = ts_doc_cleandat, times = ts_times)
coh_doc_bp <- coh_dr(dat1 = ts_bp_cleandat, dat2 = ts_doc_cleandat, times = ts_times)
coh_doc_dief <- coh_dr(dat1 = ts_dief_cleandat, dat2 = ts_doc_cleandat, times = ts_times)
coh_doc_cat <- coh_dr(dat1 = ts_cat_cleandat, dat2 = ts_doc_cleandat, times = ts_times)

# modify plotting function
plotmag_coh_dr <- function(object, sigthresh = c(0.95, 0.99), bandprows = "all", filename = NA, ...) {
  
  # extract the needed slots
  timescales <- get_timescales(object)
  coher <- get_coher(object)
  signif <- get_signif(object)
  bandp <- get_bandp(object)
  
  # error catch
  if (any(sigthresh >= 1 | sigthresh <= 0)) {
    stop("Error in plotmag.coh: inappropriate value for sigthresh")
  }
  
  if (!identical(bandprows, "all") && !any(is.na(bandp))) {
    if (!is.numeric(bandprows)) {
      stop("Error in plotmag.coh: non-numeric value for bandprows")
    }
    if (!all(bandprows %in% 1:dim(bandp)[1])) {
      stop("Error in plotmag.coh: bandprows must contain row numbers for bandp")
    }
  }
  
  if (!is.na(filename)) {
    grDevices::pdf(paste0(filename,".pdf"))
  }
  
  # if signif is absent, then just plot coher v timescales
  if (any(is.na(signif))) { 
    plot(log(1/timescales), Mod(coher),
         type = "l", lty = "solid", xaxt = "n", col = "transparent",
         xlab = "Timescale (months)", ylab = "Coherence")
    xlocs <- c(min(timescales), pretty(timescales, n = 12))
    graphics::axis(side = 1, at = log(1/xlocs), labels = xlocs) 
    
    if (!is.na(filename)) {
      grDevices::dev.off()
    }
    return(NULL)
  } 
  
  # from here on is if signif is present
  
  # get quantiles for surrogate coherences
  qs <- apply(X = Mod(signif$scoher), FUN = stats::quantile, MARGIN = 2, prob = sigthresh)
  if (length(sigthresh) == 1 ) {
    qs <- matrix(qs, 1, length(qs))
  }
  
  # if bandp is absent, just plot the lines, no p-values
  if (any(is.na(bandp))) { 
    rg <- range(Mod(coher), Mod(signif$coher), qs, na.rm = TRUE) 
    plot(log(1/timescales), Mod(coher), 
         type = "l", lty = "solid", xaxt = "n", col = "transparent", ylim = rg, 
         xlab = "Timescale (months)", ylab = "Coherence")
    xlocs <- c(min(timescales), pretty(timescales, n = 12))
    graphics::axis(side = 1, at = log(1/xlocs), labels = xlocs) 
    lines(log(1/timescales), Mod(signif$coher), type = "l", lty = "dashed", col = "red", lwd = 1.5)
    
    for (counter in 1:dim(qs)[1]) {
      lines(log(1/timescales), qs[counter, ], col = "black", lwd = 1.5)
    }
    
    if (!is.na(filename)) {
      grDevices::dev.off()
    }
    
    return(NULL)
  } 
  
  # from here on is if signif and bandp are both present
  
  rg <- range(Mod(coher), Mod(signif$coher), qs, na.rm = TRUE)
  prc <- 0.15
  drg <- diff(rg)
  rg[2] <- rg[2] + dim(bandp)[1]*prc*drg
  
  plot(log(1/timescales), Mod(coher), 
       type = "l", lty = "solid", xaxt = "n", col = "transparent", ylim = rg, 
       xlab = "Timescale (months)", ylab = "Coherence")
  xlocs <- c(min(timescales), pretty(timescales, n = 12))
  graphics::axis(side = 1, at = log(1/xlocs), labels = xlocs) 
  lines(log(1/timescales), Mod(signif$coher), type = "l", lty = "dashed", col = "red", lwd = 1.5)
  for (counter in 1:dim(qs)[1]) {
    lines(log(1/timescales), qs[counter, ], col = "black", lwd = 1.5)
  }
  if (bandprows != "all") {
    bandp <- bandp[bandprows, ]
  }
  for (counter in 1:dim(bandp)[1]) {
    b1 <- unname(bandp[counter, 1])
    if (b1 < min(timescales)) {b1 <- min(timescales)}
    b2 <- unname(bandp[counter, 2])
    if (b2 > max(timescales)) {b2 <- max(timescales)}
    p <- unname(bandp[counter, 3])
    htl <- rg[2] - (counter - 1/4 - 0.1)*prc*drg
    wwd <- 0.07*prc*drg
    lines(log(1/c(b1, b2)), c(htl, htl))
    lines(log(1/c(b1, b1)), c(htl- wwd, htl + wwd))
    lines(log(1/c(b2, b2)), c(htl- wwd, htl + wwd))
    htt <- rg[2] - (counter - 1.2/2 - 0.1)*prc*drg
    text(mean(log(1/c(b1, b2))), htt, paste0("p = ", round(p, 4)), cex = 0.66)
  }
  
  if (!is.na(filename)) {
    grDevices::dev.off()
  }
}

par(mfrow = c(3, 3), mai = c(0.6, 0.6, 0.3, 0.1))
plotmag_coh_dr(coh_doc_tp, sigthresh = 0.95); title(expression("DOC and TP"))
plotmag_coh_dr(coh_doc_srp, sigthresh = 0.95); title(expression("DOC and SRP"))
plotmag_coh_dr(coh_doc_so4, sigthresh = 0.95); title(expression(bold("DOC and SO"["4"]^" 2–")))
# plotmag_coh_dr(coh_doc_don, sigthresh = 0.95); title(expression("DOC and DON"))
# plotmag_coh_dr(coh_doc_no3, sigthresh = 0.95); title(expression("DOC and NO"[3]*""))
plotmag_coh_dr(coh_doc_nh3, sigthresh = 0.95); title(expression("DOC and NH"[3]*""))
plotmag_coh_dr(coh_doc_bp, sigthresh = 0.95); title(expression("DOC and BP inflow"))
plotmag_coh_dr(coh_doc_dief, sigthresh = 0.95); title(expression(bold("DOC and Lake Diefenbaker outflow")))
plotmag_coh_dr(coh_doc_cat, sigthresh = 0.95); title(expression("DOC and catchment flow"))

# Coherence and phase (trial run) -----------------------------------------

bandtest_coh2 <- function(object, band) {
  
  # error checking
  if (any(is.na(object$signif))) {
    stop("Error in bandtest.coh: signif cannot be NA")
  }
  if (!is.numeric(band)) {
    stop("Error in bandtest.coh: band must be numeric")
  }
  if (!is.vector(band)) {
    stop("Error in bandtest.coh: band must be a length-two numeric vector")
  }
  if (length(band) != 2) {
    stop("Error in bandtest.coh: band must be a length-two numeric vector")
  }
  
  band <- sort(band)
  timescales <- get_timescales(object)
  
  if (band[1] > max(timescales) || band[2] < min(timescales)) {
    stop("Error in bandtest.coh: band must include some of the timescales")
  }
  
  # add ranks if necessary
  if (any(is.na(object$ranks))) {
    object <- wsyn:::addranks(object)
  }
  
  # get the p-value
  x <- mean(object$ranks$coher[timescales >= band[1] & timescales <= band[2]]) # mean rank across timescales of interest, data
  sx <- apply(FUN = mean,
              X = object$ranks$scoher[, timescales >= band[1] & timescales <= band[2], drop = FALSE],
              MARGIN = 1) # mean ranks, surrogates
  pval <- (sum(sx >= x) + 1)/(length(sx) + 1)
  
  # get the average phase
  x <- object$coher[timescales >= band[1] & timescales <= band[2]]
  mnphs <- wsyn:::mnphase(x)
  mncoh <- Mod(mean(x))
  
  # form the result and return it
  if (any(is.na(object$bandp))) {
    bandp <- data.frame(ts_low_bd = band[1],
                        ts_hi_bd = band[2],
                        p_val = pval,
                        mn_phs = mnphs,
                        mn_coh = mncoh)
    object$bandp <- bandp
    return(object)
  } else {
    object$bandp[dim(object$bandp)[1] + 1, ] <- c(band, pval, mnphs, mncoh)
    return(object)
  }
}

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
bt_tp <- bandtest_coh2(object = coh_doc_tp, band = tband)
bt_srp <- bandtest_coh2(object = coh_doc_srp, band = tband)
bt_so4 <- bandtest_coh2(object = coh_doc_so4, band = tband)
bt_don <- bandtest_coh2(object = coh_doc_don, band = tband)
bt_no3 <- bandtest_coh2(object = coh_doc_no3, band = tband)
bt_nh3 <- bandtest_coh2(object = coh_doc_nh3, band = tband)
bt_bp <- bandtest_coh2(object = coh_doc_bp, band = tband)
bt_dief <- bandtest_coh2(object = coh_doc_dief, band = tband)
bt_cat <- bandtest_coh2(object = coh_doc_cat, band = tband)

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
pband1 <- c(12, 48) # 1—4 years
pband2 <- c(48, 72) # 4—6 years
pband3 <- c(96, 120) # > 8—10 years
# pband4 <- c(120, Inf)

## Chl a
coh2_chla1 <- NULL
coh2_chla2 <- NULL
coh2_chla3 <- NULL
# coh2_chla4 <- NULL
chla1 <- bandtest_coh2(object = coh_doc_chla, band = pband1)
chla2 <- bandtest_coh2(object = coh_doc_chla, band = pband2)
chla3 <- bandtest_coh2(object = coh_doc_chla, band = pband3)
# chla4 <- bandtest_coh2(object = coh_doc_chla, band = pband4)

## TP
coh2_tp1 <- NULL
coh2_tp2 <- NULL
coh2_tp3 <- NULL
# coh2_tp4 <- NULL
tp1 <- bandtest_coh2(object = coh_doc_tp, band = pband1)
tp2 <- bandtest_coh2(object = coh_doc_tp, band = pband2)
tp3 <- bandtest_coh2(object = coh_doc_tp, band = pband3)
# tp4 <- bandtest_coh2(object = coh_doc_tp, band = pband4)

## SRP 
coh2_srp1 <- NULL
coh2_srp2 <- NULL
coh2_srp3 <- NULL
# coh2_srp4 <- NULL
srp1 <- bandtest_coh2(object = coh_doc_srp, band = pband1)
srp2 <- bandtest_coh2(object = coh_doc_srp, band = pband2)
srp3 <- bandtest_coh2(object = coh_doc_srp, band = pband3)
# srp4 <- bandtest_coh2(object = coh_doc_srp, band = pband4)

## SO4 
coh2_so41 <- NULL
coh2_so42 <- NULL
coh2_so43 <- NULL
# coh2_so44 <- NULL
so41 <- bandtest_coh2(object = coh_doc_so4, band = pband1)
so42 <- bandtest_coh2(object = coh_doc_so4, band = pband2)
so43 <- bandtest_coh2(object = coh_doc_so4, band = pband3)
# so44 <- bandtest_coh2(object = coh_doc_so4, band = pband4)

## DON
coh2_don1 <- NULL
coh2_don2 <- NULL
coh2_don3 <- NULL
# coh2_don4 <- NULL
don1 <- bandtest_coh2(object = coh_doc_don, band = pband1)
don2 <- bandtest_coh2(object = coh_doc_don, band = pband2)
don3 <- bandtest_coh2(object = coh_doc_don, band = pband3)
# don4 <- bandtest_coh2(object = coh_doc_don, band = pband4)

## NO3
coh2_no31 <- NULL
coh2_no32 <- NULL
coh2_no33 <- NULL
# coh2_no34 <- NULL
no31 <- bandtest_coh2(object = coh_doc_no3, band = pband1)
no32 <- bandtest_coh2(object = coh_doc_no3, band = pband2)
no33 <- bandtest_coh2(object = coh_doc_no3, band = pband3)
# no34 <- bandtest_coh2(object = coh_doc_no3, band = pband4)

## NH3
coh2_nh31 <- NULL
coh2_nh32 <- NULL
coh2_nh33 <- NULL
# coh2_nh34 <- NULL
nh31 <- bandtest_coh2(object = coh_doc_nh3, band = pband1)
nh32 <- bandtest_coh2(object = coh_doc_nh3, band = pband2)
nh33 <- bandtest_coh2(object = coh_doc_nh3, band = pband3)
# nh34 <- bandtest_coh2(object = coh_doc_nh3, band = pband4)

## BP inflow
coh2_bp1 <- NULL
coh2_bp2 <- NULL
coh2_bp3 <- NULL
# coh2_bp4 <- NULL
bp1 <- bandtest_coh2(object = coh_doc_bp, band = pband1)
bp2 <- bandtest_coh2(object = coh_doc_bp, band = pband2)
bp3 <- bandtest_coh2(object = coh_doc_bp, band = pband3)
# bp4 <- bandtest_coh2(object = coh_doc_bp, band = pband4)

## Dief outflow
coh2_dief1 <- NULL
coh2_dief2 <- NULL
coh2_dief3 <- NULL
# coh2_dief4 <- NULL
dief1 <- bandtest_coh2(object = coh_doc_dief, band = pband1)
dief2 <- bandtest_coh2(object = coh_doc_dief, band = pband2)
dief3 <- bandtest_coh2(object = coh_doc_dief, band = pband3)
# dief4 <- bandtest_coh2(object = coh_doc_dief, band = pband4)

## Catchment flow
coh2_cat1 <- NULL
coh2_cat2 <- NULL
coh2_cat3 <- NULL
# coh2_cat4 <- NULL
cat1 <- bandtest_coh2(object = coh_doc_cat, band = pband1)
cat2 <- bandtest_coh2(object = coh_doc_cat, band = pband2)
cat3 <- bandtest_coh2(object = coh_doc_cat, band = pband3)
# cat4 <- bandtest_coh2(object = coh_doc_cat, band = pband4)

coh_vars <- data.frame(
  pair = c("doc_chla", "doc_tp", "doc_srp", "doc_so4", "doc_don", "doc_no3", 
           "doc_nh3", "doc_bp", "doc_dief", "doc_cat") 
)

## 1—4 years
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


## 4—6 years
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


## 8—10 years
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



coh2_res1$band <- "1 to 4 years"
coh2_res2$band <- "4 to 6 years"
coh2_res3$band <- "8—10 years"
# coh2_res4$band <- "> 10 years"

coh2_res_all <- rbind(coh2_res1, coh2_res2, coh2_res3)

coh2_res_sig <- subset(coh2_res_all, pval <= 0.05)
length(coh2_res_sig$pval)/length(coh2_res_all$pval) # 27.5%

coh2_res_sig$cos <- cos(coh2_res_sig$phi)
coh2_res_sig$sin <- sin(coh2_res_sig$phi)

sig_chla <- subset(coh2_res_sig, pair == "doc_chla") # 0
sig_tp <- subset(coh2_res_sig, pair == "doc_tp") # 2 (short, medium)
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






















# Coherence: Analytes and flow --------------------------------------------

# Evaluate coherence between analytes and flow using wsyn::coh.
coh_tp_bp_inflow <- coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_tp_cleandat, times = ts_times, 
                        norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                        scale.max.input = 120)
coh_srp_bp_inflow <- coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_srp_cleandat, times = ts_times, 
                         norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                         scale.max.input = 120)
coh_so4_bp_inflow <- coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_so4_cleandat, times = ts_times, 
                         norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                         scale.max.input = 120)
coh_don_bp_inflow <- coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_don_cleandat, times = ts_times, 
                         norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                         scale.max.input = 120)
coh_no3_bp_inflow <- coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_no3_cleandat, times = ts_times, 
                         norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                         scale.max.input = 120)
coh_nh3_bp_inflow <- coh(dat1 = ts_bp_inflow_cleandat, dat2 = ts_nh3_cleandat, times = ts_times, 
                         norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                         scale.max.input = 120)


coh_tp_dief <- coh(dat1 = ts_dief_cleandat, dat2 = ts_tp_cleandat, times = ts_times, 
                   norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                   scale.max.input = 120)
coh_srp_dief <- coh(dat1 = ts_dief_cleandat, dat2 = ts_srp_cleandat, times = ts_times, 
                    norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                    scale.max.input = 120)
coh_so4_dief <- coh(dat1 = ts_dief_cleandat, dat2 = ts_so4_cleandat, times = ts_times, 
                    norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                    scale.max.input = 120)
coh_don_dief <- coh(dat1 = ts_dief_cleandat, dat2 = ts_don_cleandat, times = ts_times, 
                     norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                     scale.max.input = 120)
coh_no3_dief <- coh(dat1 = ts_dief_cleandat, dat2 = ts_no3_cleandat, times = ts_times, 
                    norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                    scale.max.input = 120)
coh_nh3_dief <- coh(dat1 = ts_dief_cleandat, dat2 = ts_nh3_cleandat, times = ts_times, 
                    norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                    scale.max.input = 120)

coh_tp_rc_ic <- coh(dat1 = ts_rc_ic_cleandat, dat2 = ts_tp_cleandat, times = ts_times, 
                    norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                    scale.max.input = 120)
coh_srp_rc_ic <- coh(dat1 = ts_rc_ic_cleandat, dat2 = ts_srp_cleandat, times = ts_times, 
                     norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                     scale.max.input = 120)
coh_so4_rc_ic <- coh(dat1 = ts_rc_ic_cleandat, dat2 = ts_so4_cleandat, times = ts_times, 
                     norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                     scale.max.input = 120)
coh_don_rc_ic <- coh(dat1 = ts_rc_ic_cleandat, dat2 = ts_don_cleandat, times = ts_times, 
                      norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                      scale.max.input = 120)
coh_no3_rc_ic <- coh(dat1 = ts_rc_ic_cleandat, dat2 = ts_no3_cleandat, times = ts_times, 
                     norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                     scale.max.input = 120)
coh_nh3_rc_ic <- coh(dat1 = ts_rc_ic_cleandat, dat2 = ts_nh3_cleandat, times = ts_times, 
                     norm = "powall", sigmethod = "fast", nrand = 10000, f0 = 0.5,
                     scale.max.input = 120)


# TP and flow
par(mfrow = c(1, 3))
plotmag(coh_tp_dief, sigthresh = 0.95); title("TP and Diefenbaker outflow")
plotmag(coh_tp_rc_ic, sigthresh = 0.95); title("TP and catchment flow")
# plotmag(coh_tp_bp_inflow, sigthresh = 0.95); title("TP and BP inflow")

# SRP and flow
par(mfrow = c(1, 3))
plotmag(coh_srp_dief, sigthresh = 0.95); title("SRP and Diefenbaker outflow")
# plotmag(coh_srp_rc_ic, sigthresh = 0.95); title("SRP and catchment flow")
plotmag(coh_srp_bp_inflow, sigthresh = 0.95); title("SRP and BP inflow")

# SO4 and flow
par(mfrow = c(1, 3))
plotmag(coh_so4_dief, sigthresh = 0.95); title("SO4 and Diefenbaker outflow")
plotmag(coh_so4_rc_ic, sigthresh = 0.95); title("SO4 and catchment flow")
plotmag(coh_so4_bp_inflow, sigthresh = 0.95); title("SO4 and BP inflow")

# Organic N and flow
par(mfrow = c(1, 3))
plotmag(coh_don_dief, sigthresh = 0.95); title("Organic N and Diefenbaker outflow")
plotmag(coh_don_rc_ic, sigthresh = 0.95); title("Organic N and catchment flow")
plotmag(coh_don_bp_inflow, sigthresh = 0.95); title("Organic N and BP inflow")

# NO3 and flow
par(mfrow = c(1, 3))
plotmag(coh_no3_dief, sigthresh = 0.95); title("NO3 and Diefenbaker outflow")
plotmag(coh_no3_rc_ic, sigthresh = 0.95); title("NO3 and catchment flow")
# plotmag(coh_no3_bp_inflow, sigthresh = 0.95); title("NO3 and BP inflow")

# NH3 and flow
par(mfrow = c(1, 3))
plotmag(coh_nh3_dief, sigthresh = 0.95); title("NH3 and Diefenbaker outflow")
plotmag(coh_nh3_rc_ic, sigthresh = 0.95); title("NH3 and catchment flow")
plotmag(coh_nh3_bp_inflow, sigthresh = 0.95); title("NH3 and BP inflow")


# Significance testing ----------------------------------------------------

# 1
plotmag(coh_doc_so4, sigthresh = 0.95); title("DOC and sulphate")
coh_doc_so4 <- bandtest(coh_doc_so4, c(8, 120))
coh_doc_so4 <- bandtest(coh_doc_so4, c(2, 3))
plotmag(coh_doc_so4, sigthresh = 0.95)
title("DOC and sulphate\n 2–3 and 8–120 months")

coh_doc_so4$bandp <- NA

# 2
plotmag(coh_doc_tp, sigthresh = 0.95); title("DOC and TP")
coh_doc_tp <- bandtest(coh_doc_tp, c(2, 60))
plotmag(coh_doc_tp, sigthresh = 0.95)
title("DOC and total phosphorus\n 2–60 months")

coh_doc_tp$bandp <- NA


# 3
plotmag(coh_doc_srp, sigthresh = 0.95); title("DOC and soluble reactive phosphorus")
coh_doc_srp <- bandtest(coh_doc_srp, c(27, 50))
plotmag(coh_doc_srp, sigthresh = 0.95)
title("DOC and soluble reactive phosphorus\n 27–50 months")

coh_doc_srp$bandp <- NA

# 4
plotmag(coh_doc_don, sigthresh = 0.95); title("DOC and organic nitrogen")
coh_doc_don <- bandtest(coh_doc_don, c(2, 120))
plotmag(coh_doc_don, sigthresh = 0.95)
title("DOC and organic nitrogen\n 2–120 months")

coh_doc_don$bandp <- NA

# 5
plotmag(coh_doc_nh3, sigthresh = 0.95); title("DOC and ammonia")
coh_doc_nh3 <- bandtest(coh_doc_nh3, c(3, 20))
plotmag(coh_doc_nh3, sigthresh = 0.95)
title("DOC and ammonia\n 3–20 months")

coh_doc_nh3$bandp <- NA

# 6 
plotmag(coh_doc_no3, sigthresh = 0.95); title("DOC and nitrate")

coh_doc_no3$bandp <- NA





plotmag(coh_doc_bp_inflow, sigthresh = 0.95); title("DOC and BP inflow")
plotmag(coh_doc_dief, sigthresh = 0.95); title("DOC and Diefenbaker outflow")
plotmag(coh_doc_rc_ic, sigthresh = 0.95); title("DOC and catchment flow")

plotmag(coh_tp_dief, sigthresh = 0.95); title("TP and Diefenbaker outflow")
plotmag(coh_tp_rc_ic, sigthresh = 0.95); title("TP and catchment flow")
# plotmag(coh_tp_bp_inflow, sigthresh = 0.95); title("TP and BP inflow")

plotmag(coh_srp_dief, sigthresh = 0.95); title("SRP and Diefenbaker outflow")
# plotmag(coh_srp_rc_ic, sigthresh = 0.95); title("SRP and catchment flow")
plotmag(coh_srp_bp_inflow, sigthresh = 0.95); title("SRP and BP inflow")

plotmag(coh_so4_dief, sigthresh = 0.95); title("SO4 and Diefenbaker outflow")
plotmag(coh_so4_rc_ic, sigthresh = 0.95); title("SO4 and catchment flow")
plotmag(coh_so4_bp_inflow, sigthresh = 0.95); title("SO4 and BP inflow")

plotmag(coh_don_dief, sigthresh = 0.95); title("Organic N and Diefenbaker outflow")
plotmag(coh_don_rc_ic, sigthresh = 0.95); title("Organic N and catchment flow")
plotmag(coh_don_bp_inflow, sigthresh = 0.95); title("Organic N and BP inflow")

plotmag(coh_no3_dief, sigthresh = 0.95); title("NO3 and Diefenbaker outflow")
plotmag(coh_no3_rc_ic, sigthresh = 0.95); title("NO3 and catchment flow")
# plotmag(coh_no3_bp_inflow, sigthresh = 0.95); title("NO3 and BP inflow")

plotmag(coh_nh3_dief, sigthresh = 0.95); title("NH3 and Diefenbaker outflow")
plotmag(coh_nh3_rc_ic, sigthresh = 0.95); title("NH3 and catchment flow")
plotmag(coh_nh3_bp_inflow, sigthresh = 0.95); title("NH3 and BP inflow")

par(mfrow = c(9, 3))


library(mgcv)
library(gratia)

gam()

# hist(bp_drivers$DOC_mg.L)
hist(log(bp_drivers$DOC_mg.L))
# hist(sqrt(bp_drivers$DOC_mg.L))

# hist(bp_drivers$TP_ug.L)
# hist(log(bp_drivers$TP_ug.L))
hist(sqrt(bp_drivers$TP_ug.L))

# hist(bp_drivers$SO4_mg.L)
hist(log(bp_drivers$SO4_mg.L))
# hist(sqrt(bp_drivers$SO4_mg.L)


bpdocw <- DOC_complete_1990_2019()

bpdocw %>% 
  summarise(median = median(DOC_mg.L),
            min = min(DOC_mg.L),
            max = max(DOC_mg.L))
