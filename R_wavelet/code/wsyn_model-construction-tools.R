
# 6.1 Model construction tools --------------------------------------------

# First create a diver variable composed of an oscillation of period 12 years
# and an oscillation of period 3 years, and normally distributed white noise of
# mean 0 and standard deviation 1.5.
lts <- 12
sts <- 3
mats <- 3
times <- seq(from = -mats, to = 100)
ts1 <- sin(2*pi*times/lts)
ts2 <- sin(2*pi*times/sts)
numlocs <- 10

d1 <- matrix(data = NA, nrow = numlocs, ncol = length(times)) # the first driver 
for (counter in 1:numlocs) {
  d1[counter, ] <- ts1 + ts2 + rnorm(n = length(times), mean = 0, sd = 1.5)
}

d1

# Next create a second driver, again composed of an oscillation of period 12
# years and an oscillation of period 3 years, and normally distributed white
# noise of mean 0 and standard deviation 1.5.
d2 <- matrix(data = NA, nrow = numlocs, ncol = length(times)) # the second driver
for (counter in 1:numlocs) {
  d2[counter, ] <- ts1 + ts2 + rnorm(n = length(times), mean = 0, sd = 1.5)
}

# Next create an irrelevant environmental variable. With real data, of course,
# one will not necessarily know in advance whether an environmental variable is
# irrelevant to a population system. But, for the purpose of demonstrating the
# methods, we are playing the dual role of data creator and analyst.
dirrel <- matrix(NA, numlocs, length(times))
for (counter in 1:numlocs) { 
  dirrel[counter, ] <- rnorm(length(times), mean = 0, sd = 1.5)
}

# The population in each location is a combination of the two drivers, plus
# local variability. Driver 1 is averaged over 3 time steps in its influence on
# the populations, so only the period-12 variability in driver 1 influences the
# populations.
pops <- matrix(NA, numlocs, length(times)) # the populations
for (counter in (mats + 1):length(times)) {
  aff1 <- apply(FUN = mean, X = d1[, (counter - mats):(counter - 1)], MARGIN = 1)
  aff2 <- d2[, counter - 1]
  pops[, counter] <- aff1 + aff2 + rnorm(numlocs, mean = 0, sd = 3)
}

pops <- pops[, times >= 0]
d1 <- d1[, times >= 0]
d2 <- d2[, times >= 0]
dirrel <- dirrel[, times >= 0]
times <- times[times >= 0]

# If only the data were available and we were unaware of how they were
# generated, we may want to infer the causes of synchrony and its
# timescale-specific patterns in the populations. The wavelet mean fields of
# pops, d1 and d2 show some synchrony at timescales of about 3 and 12 for all
# three variables.
dat <- list(pops = pops, d1 = d1, d2 = d2, dirrel = dirrel)
dat <- lapply(FUN = function(x){cleandat(x, times, clev = 1)$cdat}, X = dat)

# populations
wmfpop <- wmf(dat$pops, times, scale.max.input = 28)
plotmag(wmfpop)

# d1
wmfd1 <- wmf(dat$d1, times, scale.max.input = 28)
plotmag(wmfd1)

# d2
wmfd2 <- wmf(dat$d2, times, scale.max.input = 28)
plotmag(wmfd2)

# Thus we cannot know for sure from the wavelet mean fields whether population
# synchrony at each timescale is due to synchrony in d1, d2, or both drivers at
# that timescale. However, we can fit wavelet linear models.
# 
# Start by fitting a model with all three predictors. Only the "powall" option
# for norm is implemented so far.
wlm_all <- wlm(dat = dat, times = times, resp = 1, pred = 2:4, norm = "powall",
               scale.max.input = 28)

# We will carry out analyses for this model at long timescales (11 to 13 years)
# and short timescales (2 to 4 years) simultaneously. First test whether we can
# drop each variable.
wlm_all_drop_dirrel <- wlmtest(wlmobj = wlm_all, drop = "dirrel",
                               sigmethod = "fft", nrand = 100)
wlm_all_drop_d1 <- wlmtest(wlm_all, drop = "d1", sigmethod = "fft", nrand = 100)
wlm_all_drop_d2 <- wlmtest(wlm_all, drop = "d2", sigmethod = "fft", nrand = 100)

# Examine results for dropping dirrel, long and short timescales. We find that
# dirrel does not need to be retained in either long or short-timescale models,
# as expected given how data were constructed:
blong <- c(4, Inf)
bshort <- c(2, 4)

wlm_all_drop_dirrel <- bandtest(wlm_all_drop_dirrel, band = blong)
wlm_all_drop_dirrel <- bandtest(wlm_all_drop_dirrel, band = bshort)

plotmag(wlm_all_drop_dirrel) 
plotrank(wlm_all_drop_dirrel) # drop long, short

# Examine results for dropping d1, long and short timescales. We find that d1
# should be retained in a long-timescale model but need not be retained in a
# short-timescale model, again as expected:
wlm_all_drop_d1 <- bandtest(wlm_all_drop_d1, band = blong)
wlm_all_drop_d1 <- bandtest(wlm_all_drop_d1, band = bshort)

plotmag(wlm_all_drop_d1)  
plotrank(wlm_all_drop_d1) # keep long, drop short

# Examine results for dropping d2, long and short timescales. We find that d2
# should be retained in both a short-timescale model and in a long-timescale
# model, again as expected:
wlm_all_drop_d2 <- bandtest(wlm_all_drop_d2, band = blong)
wlm_all_drop_d2 <- bandtest(wlm_all_drop_d2, band = bshort)

plotmag(wlm_all_drop_d2)  
plotrank(wlm_all_drop_d2) # keep long and short

# Note that only 100 randomizations were used in this example. This is for speed
# - in a real analysis, at least 1000 randomizations should typically be
# performed, and preferably at least 10000.


# 6.2 Amounts of synchrony explained --------------------------------------

# Now we have constructed models for short timescales (2—4 years) and long
# timescales (11—13 years) for the example, finding, as expected, that d1 is a
# driver at long timescales only and d2 is a driver at short and long
# timescales. 
# 
# How much of the synchrony in the response variable is explained by
# these drivers for each timescale band?

# For short timescales, almost all the synchrony that can be explained is
# explained by Moran effects of d2:
se <- syncexpl(wlm_all)

se_short <- se[se$timescales >= bshort[1] & se$timescales <= bshort[2], ]
round(100 * colMeans(se_short[, c(3:12)]) / mean(se_short$sync), 4)
# syncexpl   crossterms       resids           d1           d2       dirrel 
#  55.7398       0.6923      43.5678       1.2563      47.3174       0.2686 
# interactions        d1_d2    d1_dirrel    d2_dirrel 
#       6.8975       6.6787       0.0543       0.1645 

# These are percentages of sychrony explained by various factors: 
# 
# syncexpl is total synchrony explained by the predictors for which we have
# data; 
# 
# crossterms must be small enough for the rest of the results to be 
# interpretable; 
# 
# d1, d2 and dirrel are percentages of sychrony explained by those predictors;
# 
# interactions is percentage of synchrony explained by interactions between
# predictors (see Sheppard et al. (2019));
# 
# and the remaining terms are percentages of synchrony explained by individual 
# interactions.

# For long timescales, Moran effects of both drivers are present, as are
# interactions between these Moran effects:
se_long <- se[se$timescales >= blong[1] & se$timescales <= blong[2], ]
round(100 * colMeans(se_long[, c(3:12)]) / mean(se_long$sync), 4)
# syncexpl   crossterms       resids           d1           d2       dirrel 
#  99.8803      -2.0652       2.1850      29.6611      34.4248       0.0635      
# interactions        d1_d2    d1_dirrel    d2_dirrel 
#      35.7310      35.7873       0.3073      -0.3636 

# Note that cross terms are fairly small in both these analyses compared to
# synchrony explained. Results can only be interpreted when this is the case.
# See Sheppard et al. (2019) for detailed information on cross terms and
# interacting Moran effects.

# The pattern of synchrony that would pertain if the only drivers of synchrony
# were those included in a model can also be produced, and compared to the
# actual pattern of synchrony (as represented by the wavelet mean field) to help
# evaluate the model.
pres <- predsync(wlm_all)
plotmag(pres)

par(mfrow = c(1, 2))
plotmag(pres)
plotmag(wmfpop)

# The similarity is pretty good. 

# Now make the comparison using the model with sole predictor d1.
wlm_d1 <- wlm(dat, times, resp = 1, pred = 2, norm = "powall", scale.max.input = 28)

pres_d1 <- predsync(wlm_d1)

par(mfrow = c(1, 2))
plotmag(pres_d1)
plotmag(wmfpop)

# The similarity with the wavelet mean field of the populations is pretty good
# at long timescales (where the model with sole predictor d1 was found to be a
# good model), but not at short timescales.