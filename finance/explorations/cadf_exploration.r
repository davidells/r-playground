# Example of testing two price series for cointegration, as well as for
# mean reversion in a spread between them. 

# The hedge ratio used in cadf is determined by default by fitting a linear model using lm.
# We'll also use an alternative regression using tls (instead of ols from lm) and test out
# the pair that way.


# Run cadf using ols and tls regression method, and using the ADF model II 
# (assumes an intercept constant, but does not assume a drift constant, 
# and with lag order of 1. Return best (lowest) p value found. Anything
# below 0.10 indicates 90% certainty of mean reversion, 0.05 indicates 95%, etc.
cadf.best <- function (x, y) {
  debug <- FALSE
  
  # OLS can create two different hedge ratios based on which series
  # is considered independent vs dependent.
  cadf.ols <- cadf(x, y, method="ols", model=2, k=1)
  
  # TLS is symmetric, so order of series doesn't matter. See
  # http://quanttrader.info/public/betterHedgeRatios.pdf
  cadf.tls <- cadf(x, y, method="tls", model=2, k=1)
  
  # Capture smallest p-value among our tests
  pValueMin <- min(
      cadf.ols$p.value, 
      cadf.tls$p.value)
  
  if (pValueMin == cadf.ols$p.value) {
    cadf <- cadf.ols
  } else {
    cadf <- cadf.tls
  }
  return(cadf)
}

# Plot series, adding lines indicating one and two standard deviations away
plotWithStdDev <- function (series) {
  mean <- mean(series)
  sd <- sd(series)
  plot(series)
  abline(h=mean, col="green")
  abline(h=mean + sd, col="orange")
  abline(h=mean - sd, col="orange")
  abline(h=mean + sd*2, col="red")
  abline(h=mean - sd*2, col="red")
}

# First up, let's grab some data. For now, let's just guess at some cointegrating price series.
# How about, the energy sector ETF, and a proxy for the price of oil, USO. We'll do 2008 through 2014.
getSymbols(c("XLE", "USO"), from="2008-01-01", to="2014-12-01")
print(cadf.best(Ad(XLE), Ad(USO)))

# Huh, ok, not a cointegrating pair it seems. pValue of 0.39 is not indicating mean reversion.
# Let's take a look. 
sprd <- Ad(USO) - hedgeRatio(Ad(USO), Ad(XLE), method="ols") * Ad(XLE)
plot(sprd)

# Hm, starting with 2009, it look much more stable, let's try the subset and see what we get...
XLE <- XLE["2009-01-01::"]
USO <- USO["2009-01-01::"]
print(cadf.best(Ad(XLE), Ad(USO)))

# Indeed, a p-value of 0.04 is much better. Let's see this spread, with standard deviations.
sprd <- Ad(USO) - hedgeRatio(Ad(USO), Ad(XLE), method="ols") * Ad(XLE)
plotWithStdDev(sprd)

# Finally, let's see what the half life of a reversion strategy might be with this spread...
print(halflife(sprd))

# Let's try the example from Ernie Chan's book, of EWA vs EWC (note we can't get 2006 data
# from default getSymbols provider yahoo, so ours starts at 2007)
getSymbols(c("EWA", "EWC"), from="2007-01-01", to="2012-04-09")
print(cadf.best(Ad(EWA), Ad(EWC)))

# Ah, there's a very low p-value.
# Let's plot the spread between EWA and EWC (note it's not necessarily the spread
# that gave us our cadf.pValue result).
sprd <- Ad(EWC) - hedgeRatio(Ad(EWC), Ad(EWA), method="ols") * Ad(EWA)
plotWithStdDev(sprd)

# And the half life here?
print(halflife(sprd))