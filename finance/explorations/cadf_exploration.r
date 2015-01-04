# Example of testing two price series for cointegration, as well as for
# mean reversion in a spread between them. 

library(tseries)
library(quantmod)

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
dimnames(sprd)[2] <- "USO.XLE.Spread"
plotWithStdDev(sprd)

# Finally, let's see what the half life of a reversion strategy might be with this spread...
print(halflife(sprd))

# Ah, but we've been tricked. It turns out that the spread we've created is actually
# pretty much the same as just owning USO. Surprisingly, the price of oil over this time
# period is mean reverting all on it's own. Though that's still the point if you're looking
# to trade mean reversion, we need to be aware of this fact.

# We have to systematically avoid this problem. One general way to do this is ensure
# that the resulting portfolio is not correlated too strong with any one instrument.
if (correlatedWithConstituent( sprd, data.frame(Ad(USO), Ad(XLE)) )) {
  print("Spread is correlated to an underlying")
  cor(data.frame(sprd, Ad(USO), Ad(XLE)))[1,]
}

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