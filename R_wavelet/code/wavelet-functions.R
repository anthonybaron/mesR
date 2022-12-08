library(wsyn)

# Coherence and phase -----------------------------------------------------
coh_dr <- function(dat1 = NULL, dat2 = NULL, times = NULL, norm = "powall",
                   sigmethod = "fast", nrand = 10000, sigma = 1.01,
                   scale.max.input = 120) {
  
  coh_ob <- coh(dat1 = dat1, dat2 = dat2, times = times, norm = norm,
                sigmethod = sigmethod, nrand = nrand, sigma = sigma,
                scale.max.input = scale.max.input)
  
  return(coh_ob)
  
}

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


# Plotting ----------------------------------------------------------------

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
                    xlab = "", zlim = zlims, ylab = "", # ylab = "Timescale (years)",
                    axes = FALSE, col = colorfill(100), main = title, ...)
    graphics::axis(1, at = xlocs, labels = xlabs)
    graphics::axis(2, at = log2(ylocs), labels = ylabs)
  } else {
    fields::image.plot(x = times, y = log2(timescales), z = wav,
                       xlab = "", zlim = zlims, ylab = "", # ylab = "Timescale (years)",
                       axes = FALSE, col = colorfill(100), main = title, ...)
    graphics::axis(1, at = xlocs, labels = xlabs)
    graphics::axis(2, at = log2(ylocs), labels = ylabs)
  }
  
  if (!is.na(filename)) {
    grDevices::dev.off()
  }
  
}

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
         type = "l", lty = "solid", xaxt = "n", col = "#31688EFF", 
         lwd = 1.5, cex = 1.2,  
         xlab = "Timescale (months)", ylab = "Coherence")
    xlocs <- c(min(timescales), pretty(timescales, n = 12))
    graphics::axis(side = 1, at = log(1/xlocs), labels = xlocs, cex = 1.2) 
    
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
         type = "l", lty = "solid", xaxt = "n", col = "#31688EFF", ylim = rg,
         lwd = 1.5, cex = 1.2,
         xlab = "", ylab = "")
    xlocs <- c(min(timescales), pretty(timescales, n = 12))
    graphics::axis(side = 1, at = log(1/xlocs), labels = xlocs, cex = 1.2) 
    lines(log(1/timescales), Mod(signif$coher), type = "l", lty = "dashed", col = "red", lwd = 1.5, cex = 1.2)
    
    for (counter in 1:dim(qs)[1]) {
      lines(log(1/timescales), qs[counter, ], col = "black", lwd = 1.5, cex = 1.2)
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
       type = "l", lty = "solid", xaxt = "n", col = "#31688EFF", ylim = rg, 
       lwd = 1.5, cex = 1.2,
       xlab = "", ylab = "")
  xlocs <- c(min(timescales), pretty(timescales, n = 12))
  graphics::axis(side = 1, at = log(1/xlocs), labels = xlocs, cex = 1.2) 
  lines(log(1/timescales), Mod(signif$coher), type = "l", lty = "dashed", col = "red", lwd = 1.5, cex = 1.2)
  for (counter in 1:dim(qs)[1]) {
    lines(log(1/timescales), qs[counter, ], col = "black", lwd = 1.5, cex = 1.2)
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
    text(mean(log(1/c(b1, b2))), htt, paste0("p = ", round(p, 4)), cex = 1.2)
  }
  
  if (!is.na(filename)) {
    grDevices::dev.off()
  }
}

plotrank_coh_dr <- function(object, sigthresh = 0.95, bandprows = "all", filename = NA, ...) {
  
  # error check
  if (any(is.na(object$signif))) {
    stop("Error in plotrank.coh: plotrank.coh needs a signif slot")
  }
  
  # extract the needed slots
  ranks <- get_ranks(object)
  if (any(is.na(ranks))) {
    object <- addranks(object)
    ranks <- get_ranks(object)
  }
  bandp <- get_bandp(object)
  timescales <- get_timescales(object)
  
  # more error check
  if (any(sigthresh >= 1 | sigthresh <= 0)) {
    stop("Error in plotrank.coh: inappropriate value for sigthresh")
  }
  if (!identical(bandprows, "all") && !any(is.na(bandp))) {
    if (!is.numeric(bandprows)) {
      stop("Error in plotrank.coh: non-numeric value for bandprows")
    }
    if (!all(bandprows %in% 1:dim(bandp)[1])) {
      stop("Error in plotrank.coh: bandprows must contain row numbers for bandp")
    }
  }
  
  if (!is.na(filename)) {
    grDevices::pdf(paste0(filename, ".pdf"))
  }
  
  if (any(is.na(bandp))) {
    # if bandp is absent, just plot the lines, no p-values
    x <- log(1/timescales)
    plot(x, ranks$coher, type = "l", lty = "solid", xaxt = "n", col = "#31688EFF", lwd = 1.5,
         xlab = "Timescales", ylab = "Fract surr gt", ylim = c(0.5, 1))
    for (counter in 1:length(sigthresh)) {
      lines(range(x), c(sigthresh[counter], sigthresh[counter]), lty = "dashed")
    }
    xlocs <- c(min(timescales), pretty(timescales, n = 8))
    graphics::axis(side = 1, at = log(1/xlocs), labels = xlocs, cex = 1) 
  } else {
    # if bandp is present plot p-values too
    # get the new vertical axis range to fit the p-vals
    rg <- c(0.5, 1)
    prc <- 0.15
    drg <- diff(rg)
    rg[2] <- rg[2] + dim(bandp)[1]*prc*drg
    
    # plot
    x <- log(1/timescales)
    plot(x, ranks$coher, type = "l", lty = "solid", xaxt = "n", col = "red", 
         xlab = "Timescales", ylab = "Fract surr gt", ylim = rg)
    lines(range(x), c(1, 1), type = 'l', lty = "solid")
    for (counter in 1:length(sigthresh)) {
      lines(range(x), c(sigthresh[counter], sigthresh[counter]), lty = "dashed")
    }
    xlocs <- c(min(timescales), pretty(timescales, n = 8))
    graphics::axis(side = 1, at = log(1 / xlocs), labels = xlocs, cex = 1) 
    
    # add the p-vals
    if (!identical(bandprows, "all")) {
      bandp <- bandp[bandprows, ]
    }
    for (counter in 1:dim(bandp)[1]) {
      b1 <- unname(bandp[counter, 1])
      if (b1<min(timescales)) {b1 <- min(timescales)}
      b2 <- unname(bandp[counter, 2])
      if (b2 > max(timescales)) {b2 <- max(timescales)}
      p <- unname(bandp[counter, 3])
      htl <- rg[2] - (counter - 1/4 - 0.1)*prc*drg
      wwd <- 0.07*prc*drg
      lines(log(1/c(b1, b2)), c(htl, htl))
      lines(log(1/c(b1, b1)), c(htl - wwd, htl + wwd))
      lines(log(1/c(b2, b2)), c(htl - wwd, htl + wwd))
      htt <- rg[2] - (counter - 1.2 / 2 - 0.1)*prc*drg
      text(mean(log(1/c(b1, b2))), htt, paste0("p =", round(p, 4)), cex = 0.66)
    }
  }
  
  if (!is.na(filename)) {
    grDevices::dev.off()
  }
  
  return(NULL)
  
}