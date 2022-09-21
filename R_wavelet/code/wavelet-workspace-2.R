library(wsyn)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(patchwork)
library(cowplot)
library(gridGraphics)

theme_set(theme_bw(base_size = 12) + theme(panel.background = element_blank())) 
options(digits = 3)

# Source data -------------------------------------------------------------

source("./R_data-cleaning/bpwtp/code/clean-wavelet.R")

bp_drivers1 <- wavelet_data()

bb <- bp_drivers1 %>% 
  group_by(year) %>% 
  summarise(DOC_mg.L = mean(DOC_mg.L),
            SO4_mg.L = mean(SO4_mg.L),
            dief = mean(SK05JG006_cms),
            bp = mean(SK05JG004_combined_cms),
            tp = mean(TP_ug.L),
            cat = mean(RC_IC_cms))
  
tt <- rep(1:length(bb$year)) # length of time series (dates)
dd <- bb$DOC_mg.L
ss <- bb$SO4_mg.L
ii <- bb$bp
uu <- bb$dief
pp <- bb$tp
cc <- bb$cat
  
ddc <- cleandat(dat = dd, times = tt, clev = 5)
ssc <- cleandat(dat = ss, times = tt, clev = 5)
iic <- cleandat(dat = ii, times = tt, clev = 5)
uuc <- cleandat(dat = uu, times = tt, clev = 5)
ppc <- cleandat(dat = pp, times = tt, clev = 5)
ccc <- cleandat(dat = cc, times = tt, clev = 5)

ddw <- wt(ddc$cdat, tt)
ssw <- wt(ssc$cdat, tt)
iiw <- wt(iic$cdat, tt)
uuw <- wt(uuc$cdat, tt)
ppw <- wt(ppc$cdat, tt)
ccw <- wt(ccc$cdat, tt)

par(mfrow = c(2, 3))
plotmag(ddw); title("DOC")
plotmag(ssw); title("SO4")
plotmag(ppw); title("TP")
plotmag(uuw); title("Dief")
plotmag(ccw); title("Catchment")
plotmag(iiw); title("BP")

ddss <- coh(ddc$cdat, ssc$cdat, tt, norm = "powall", sigmethod = "fast", nrand = 10000)
ddpp <- coh(ddc$cdat, ppc$cdat, tt, norm = "powall", sigmethod = "fast", nrand = 10000)
dduu <- coh(ddc$cdat, uuc$cdat, tt, norm = "powall", sigmethod = "fast", nrand = 10000)
ddcc <- coh(ddc$cdat, ccc$cdat, tt, norm = "powall", sigmethod = "fast", nrand = 10000)
ddii <- coh(ddc$cdat, iic$cdat, tt, norm = "powall", sigmethod = "fast", nrand = 10000)

par(mfrow = c(2, 3))
plotmag(ddss, sigthresh = 0.95); title("DOC-SO4")
plotmag(ddpp, sigthresh = 0.95); title("DOC-TP")
plotmag(dduu, sigthresh = 0.95); title("DOC-Dief")
plotmag(ddcc, sigthresh = 0.95); title("DOC-Catchment")
plotmag(ddii, sigthresh = 0.95); title("DOC-BP")

plotrank(ddss, sigthresh = 0.95); title("DOC-SO4")
plotrank(ddpp, sigthresh = 0.95); title("DOC-TP")
plotrank(dduu, sigthresh = 0.95); title("DOC-Dief")
plotrank(ddcc, sigthresh = 0.95); title("DOC-Catchment")
plotrank(ddii, sigthresh = 0.95); title("DOC-BP")

plotphase(ddss, sigthresh = 0.95); title("DOC-SO4")
plotphase(ddpp, sigthresh = 0.95); title("DOC-TP")
plotphase(dduu, sigthresh = 0.95); title("DOC-Dief")
plotphase(ddcc, sigthresh = 0.95); title("DOC-Catchment")
plotphase(ddii, sigthresh = 0.95); title("DOC-BP")

# Timescales --------------------------------------------------------------

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

coh_vars <- data.frame(pair = c("doc_tp", "doc_so4", "doc_dief", "doc_cat","doc_bp"))


# // Long -----------------------------------------------------------------

lcoh_tp <- NULL
lcoh_so4 <- NULL
lcoh_dief <- NULL
lcoh_cat <- NULL
lcoh_bp <- NULL

lband <- c(4, Inf)
ltp <- bandtest_coh2(object = ddpp, band = lband)
lso4 <- bandtest_coh2(object = ddss, band = lband)
ldief <- bandtest_coh2(object = dduu, band = lband)
lcat <- bandtest_coh2(object = ddcc, band = lband)
lbp <- bandtest_coh2(object = ddii, band = lband)

lcoh_tp <- rbind(lcoh_tp, c(t(as.matrix(ltp$bandp[, 3:5]))))
lcoh_so4 <- rbind(lcoh_so4, c(t(as.matrix(lso4$bandp[, 3:5]))))
lcoh_dief <- rbind(lcoh_dief, c(t(as.matrix(ldief$bandp[, 3:5]))))
lcoh_cat <- rbind(lcoh_cat, c(t(as.matrix(lcat$bandp[, 3:5]))))
lcoh_bp <- rbind(lcoh_bp, c(t(as.matrix(lbp$bandp[, 3:5]))))

lcoh <- rbind(lcoh_tp, lcoh_so4, lcoh_dief, lcoh_cat, lcoh_bp)
lcoh_df <- as.data.frame(lcoh)

colnames(lcoh_df) <- c("pval", "phi", "coh")

lcoh_res <- cbind(lcoh_df, coh_vars)
lcoh_res$cos <- cos(lcoh_res$phi)
lcoh_res$sin <- sin(lcoh_res$phi)
lcoh_res$timescale <- "long (> 4 years)"

lcoh_sig <- subset(lcoh_res, pval <= 0.05)
# #  pval    phi   coh    pair
# # 0.001 0.0453 0.627 doc_so4

plotmag(lcoh)


# // Short ----------------------------------------------------------------

scoh_tp <- NULL
scoh_so4 <- NULL
scoh_dief <- NULL
scoh_cat <- NULL
scoh_bp <- NULL

sband <- c(2, 4)
stp <- bandtest_coh2(object = ddpp, band = sband)
sso4 <- bandtest_coh2(object = ddss, band = sband)
sdief <- bandtest_coh2(object = dduu, band = sband)
scat <- bandtest_coh2(object = ddcc, band = sband)
sbp <- bandtest_coh2(object = ddii, band = sband)

scoh_tp <- rbind(scoh_tp, c(t(as.matrix(stp$bandp[, 3:5]))))
scoh_so4 <- rbind(scoh_so4, c(t(as.matrix(sso4$bandp[, 3:5]))))
scoh_dief <- rbind(scoh_dief, c(t(as.matrix(sdief$bandp[, 3:5]))))
scoh_cat <- rbind(scoh_cat, c(t(as.matrix(scat$bandp[, 3:5]))))
scoh_bp <- rbind(scoh_bp, c(t(as.matrix(sbp$bandp[, 3:5]))))

scoh <- rbind(scoh_tp, scoh_so4, scoh_dief, scoh_cat, scoh_bp)
scoh_df <- as.data.frame(scoh)

colnames(scoh_df) <- c("pval", "phi", "coh")

scoh_res <- cbind(scoh_df, coh_vars)
scoh_res$cos <- cos(scoh_res$phi)
scoh_res$sin <- sin(scoh_res$phi)
scoh_res$timescale <- "short (< 4 years)"

scoh_sig <- subset(scoh_res, pval <= 0.05)
#     pval    phi   coh    pair
# # 0.0001 -0.790 0.625  doc_tp
# # 0.0042  0.422 0.587 doc_so4


sl_res <- rbind(scoh_res, lcoh_res)
sl_res <- sl_res %>% select(pval, phi, coh, cos, sin, pair, timescale)



# Synchrony testing -------------------------------------------------------

tt <- rep(1:length(bb$year)) # length of time series (dates)
dd <- bb$DOC_mg.L
ss <- bb$SO4_mg.L
ii <- bb$bp
uu <- bb$dief
pp <- bb$tp
cc <- bb$cat

bdat <- list(dd = dd, pp = pp, ss = ss, uu = uu)
bdat <- lapply(FUN = function(x){cleandat(dat = x, times = tt, clev = 5)$cdat}, X = bdat)
bdat <- lapply(FUN = function(x){matrix(data = x, nrow = 1, ncol = 30)}, X = bdat)

# Start by fitting a wlm model with all predictors (TP, SO4, Dief) using the 
# "powall" option for norm.
wlmb <- wlm(dat = bdat, times = tt, resp = 1, pred = 2:4, 
            norm = "powall", scale.max.input = 8)

# Carry out anlyses at long (> 4 years) and short (< 4 years) timescales
# simultaneously. First test whether we can drop each variable. 
wlm_full_drop_pp <- wlmtest(wlmobj = wlm_full, drop = "pp", sigmethod = "fft", nrand = 100)
wlm_full_drop_ss <- wlmtest(wlmobj = wlm_full, drop = "ss", sigmethod = "fft", nrand = 100)
wlm_full_drop_uu <- wlmtest(wlmobj = wlm_full, drop = "uu", sigmethod = "fft", nrand = 100)

longband <- c(4, Inf)
shortband <- c(2, 4)

seb <- syncexpl(wlmb)

seb_short <- seb[seb$timescales >= shortband[1] & seb$timescales <= shortband[2], ]
seb_short_res <- round(100 * colMeans(seb_short[, c(3:12)]) / mean(seb_short$sync), 4)
# syncexpl = 73.257%
# crossterms = 0.000
# resids = 26.743
# pp = 17.905%
# ss = 16.345%
# uu = 60.758%
# interactions = —21.750%
# pp_ss = —9.027%
# pp_uu = —11.958%
# ss_uu = —0.765%

seb_long <- seb[seb$timescales >= longband[1] & seb$timescales <= longband[2], ]
seb_long_res <- round(100 * colMeans(seb_long[, c(3:12)]) / mean(seb_long$sync), 4)
# syncexpl =  98.67%
# crossterms = 0.00
# resids = 1.33
# pp = 17.905%
# ss = 16.345%
# uu = 60.758%
# interactions = —21.750%
# pp_ss = —9.027%
# pp_uu = —11.958%
# ss_uu = —0.765%

bres <- predsync(wlmb)
plotmag(bres)



# wlm with significant coherent vars --------------------------------------

tt <- rep(1:length(bb$year)) # length of time series (dates)
dd <- bb$DOC_mg.L
ss <- bb$SO4_mg.L
pp <- bb$tp

cohdat <- list(dd = dd, ss = ss, pp = pp)
cohdat <- lapply(FUN = function(x){cleandat(dat = x, times = tt, clev = 5)$cdat}, X = cohdat)
cohdat <- lapply(FUN = function(x){matrix(data = x, nrow = 1, ncol = 30)}, X = cohdat)

wlmcoh <- wlm(cohdat, tt, resp = 1, pred = 2:3, norm = "powall", scale.max.input = 8)

longband <- c(4, Inf)
shortband <- c(2, 4)

secoh <- syncexpl(wlmcoh)

secoh_short <- secoh[secoh$timescales >= shortband[1] & secoh$timescales <= shortband[2], ]
secoh_short_res <- round(100 * colMeans(secoh_short[, c(3:7)]) / mean(secoh_short$sync), 4)

secoh_long <- secoh[secoh$timescales >= longband[1] & secoh$timescales <= longband[2], ]
secoh_long_res <- round(100 * colMeans(secoh_long[, c(3:7)]) / mean(secoh_long$sync), 4)

secoh_short_res
# syncexpl = 64.1%
# crossterms = 0.0
# resids = 35.9
# ss = 22.5%
# pp = 50.7%

secoh_long_res
# syncexpl = 89.8%
# crossterms = 0.0
# resids = 10.2
# ss = 8.08%
# uu = 58.81%