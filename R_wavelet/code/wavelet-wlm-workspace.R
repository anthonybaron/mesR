library(wsyn)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(patchwork)
library(cowplot)
library(gridGraphics)
library(mgcv)

theme_set(theme_bw(base_size = 12) + theme(panel.background = element_blank())) 
options(digits = 3)

# Source data -------------------------------------------------------------

source("./R_data-cleaning/bpwtp/code/clean-wavelet.R")
source("./R_wavelet/code/wavelet-functions.R")

bp_drivers1 <- wavelet_data()

bb <- bp_drivers1 %>% 
  rename(dief = SK05JG006_cms,
         bp = SK05JG004_combined_cms,
         cat = RC_IC_cms)

tt <- rep(1:length(bb$year)) # length of time series (dates)
dd <- bb$DOC_mg.L
ss <- bb$SO4_mg.L
ii <- bb$bp
uu <- bb$dief
cc <- bb$cat
pp <- bb$TP_ug.L
rr <- bb$SRP_ug.L

ddc <- cleandat(dat = dd, times = tt, clev = 5)
ssc <- cleandat(dat = ss, times = tt, clev = 5)
iic <- cleandat(dat = ii, times = tt, clev = 5)
uuc <- cleandat(dat = uu, times = tt, clev = 5)
ccc <- cleandat(dat = cc, times = tt, clev = 5)
ppc <- cleandat(dat = pp, times = tt, clev = 5)
rrc <- cleandat(dat = rr, times = tt, clev = 5)

ddw <- wt(ddc$cdat, tt)
ssw <- wt(ssc$cdat, tt)
iiw <- wt(iic$cdat, tt)
uuw <- wt(uuc$cdat, tt)
ccw <- wt(ccc$cdat, tt)
ppw <- wt(ppc$cdat, tt)
rrw <- wt(rrc$cdat, tt)

par(mfrow = c(3, 3))
plotmag(ddw); title("DOC")
plotmag(ssw); title("SO4")
plotmag(ppw); title("TP")
plotmag(rrw); title("SRP")
plotmag(uuw); title("Dief")
plotmag(ccw); title("Catchment")
plotmag(iiw); title("BP")

ddss <- coh_dr(ddc$cdat, ssc$cdat, tt)
ddii <- coh_dr(ddc$cdat, iic$cdat, tt)
dduu <- coh_dr(ddc$cdat, uuc$cdat, tt)
ddcc <- coh_dr(ddc$cdat, ccc$cdat, tt)
ddpp <- coh_dr(ddc$cdat, ppc$cdat, tt)
ddrr <- coh_dr(ddc$cdat, rrc$cdat, tt)

par(mfrow = c(3, 2))
plotmag_coh_dr(ddss, sigthresh = 0.95); title("DOC-SO4")
plotmag_coh_dr(ddpp, sigthresh = 0.95); title("DOC-TP")
plotmag_coh_dr(ddrr, sigthresh = 0.95); title("DOC-SRP")
plotmag_coh_dr(dduu, sigthresh = 0.95); title("DOC-Dief")
plotmag_coh_dr(ddcc, sigthresh = 0.95); title("DOC-Catchment")
plotmag_coh_dr(ddii, sigthresh = 0.95); title("DOC-BP")

plotrank(ddss, sigthresh = 0.95); title("DOC-SO4")
plotrank(ddpp, sigthresh = 0.95); title("DOC-TP")
plotrank(ddrr, sigthresh = 0.95); title("DOC-SRP")
plotrank(dduu, sigthresh = 0.95); title("DOC-Dief")
plotrank(ddcc, sigthresh = 0.95); title("DOC-Catchment") # no sig coh
plotrank(ddii, sigthresh = 0.95); title("DOC-BP")

plotphase(ddss, sigthresh = 0.95); title("DOC-SO4")
plotphase(ddpp, sigthresh = 0.95); title("DOC-TP")
plotphase(ddrr, sigthresh = 0.95); title("DOC-SRP")
plotphase(dduu, sigthresh = 0.95); title("DOC-Dief")
# plotphase(ddcc, sigthresh = 0.95); title("DOC-Catchment")
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

coh_vars <- data.frame(pair = c("doc_so4", "doc_tp", "doc_dief", "doc_bp"))


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
bdat <- lapply(FUN = function(x){matrix(data = x, nrow = 1, ncol = 360)}, X = bdat)

# Start by fitting a wlm model with all predictors (TP, SO4, Dief) using the 
# "powall" option for norm.
wlmb <- wlm(dat = bdat, times = tt, resp = 1, pred = 2:4, 
            norm = "powall", scale.max.input = 120)

# Carry out anlyses at long (> 4 years) and short (< 4 years) timescales
# simultaneously. First test whether we can drop each variable. 
drop_pp <- wlmtest(wlmobj = wlmb, drop = "pp", sigmethod = "fft", nrand = 100)
drop_ss <- wlmtest(wlmobj = wlm_full, drop = "ss", sigmethod = "fft", nrand = 100)
drop_uu <- wlmtest(wlmobj = wlm_full, drop = "uu", sigmethod = "fft", nrand = 100)

band1 <- c(11, 13)
band2 <- c(48, 60)
band3 <- c(96, 120)

seb <- syncexpl(wlmb)

seb1 <- seb[seb$timescales >= band1[1] & seb$timescales <= band1[2], ]
seb1_res <- round(100 * colMeans(seb1[, c(3:12)]) / mean(seb1$sync), 4)
# syncexpl = 58.097%
# crossterms = 0.00
# resids = 41.91
# pp = 79.69%
# ss = 29.85%
# uu = 9.58%
# interactions = —61.03%
# pp_ss = -36.56%
# pp_uu = -39.43%
# ss_uu = 14.96%

seb2 <- seb[seb$timescales >= band2[1] & seb$timescales <= band2[2], ]
seb2_res <- round(100 * colMeans(seb2[, c(3:12)]) / mean(seb2$sync), 4)
# syncexpl =  97.47%
# crossterms = 0.00
# resids = 2.53
# pp = 65.52%
# ss = 239.68%
# uu = 177.73%
# interactions = -385.46%
# pp_ss = -224.85%
# pp_uu = 119.18%
# ss_uu = -279.79%

seb3 <- seb[seb$timescales >= band3[1] & seb$timescales <= band3[2], ]
seb3_res <- round(100 * colMeans(seb3[, c(3:12)]) / mean(seb3$sync), 4)
# syncexpl =  99.9989%
# crossterms = 0.00
# resids = 0.0011
# pp = 14.5143%
# ss = 67.3586%
# uu = 307.5619%
# interactions = -289.4358%
# pp_ss = -23.9876%
# pp_uu = 72.9415%
# ss_uu = -240.4820%

bres <- predsync(wlmb)
plotmag(bres)

