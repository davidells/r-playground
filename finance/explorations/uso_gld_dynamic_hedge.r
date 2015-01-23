# Reproduction of Example 3.1 and 3.2 from Ernie Chan's Algorithmic Trading
library('quantmod')

# Example from Ernie Chan's book, of USO vs GLD (note we can't get 2006 data
# from default getSymbols provider yahoo, so ours starts at 2007)
getSymbols(c("USO", "GLD"), from="2007-01-01", to="2012-04-09")

# Bring price series together into a data frame
df <- as.xts( data.frame(Ad(USO), Ad(GLD)) )

# As in Chan's example, this is set to 20, having already been determined with
# the benefit of hindsight
lookback = 20

# Calculate a rolling hedge ratio for this pair, over window defined by the lookback
hedgeRatios <- rollapply(df, 
  width = lookback, 
  by.column = FALSE,
  FUN = function (window) { 
    hedgeRatio(window[,1], window[,2]) 
})

# Create a portfolio using the securities in our data frame, weighted according
# to the hedge ratios we just discovered.
weights <- cbind( rep(1, dim(df)[1]), -hedgeRatios )
port <- reclass(rowSums(df * weights), df)

# Plot the portfolio price
plotWithStdDev(port)

# Are we over exposed to any one security?
if (correlatedWithConstituent( port, df )) {
  print("Portfolio is overly correlated to an underlying")
  cor(data.frame(port, df))
}

# Figure out mean reversion halflife
# lookback <- round(halflife(port))

# TODO: Pull this remaining mean reversion test code into a function, use that function
# here and in mean_reversion_backtest.r

# Amount of unit portfolio to hold is determined by the negative of the z-score
# of the series.
zScore <- (port - movingAvg(port, lookback)) / movingStd(port, lookback)
units <- -zScore

# Expand our units to agree with series dataframe in terms of dimensions.
ret <- portfolio_ret(units, weights, df)
plot(cumsum(ret), main="Cumulative Returns of USO-GLD Spread w/ Dynamic Hedge Ratio")

# TODO: Finish example by using Bollinger Bands for entry / exit

# Now let's use Bollinger Bands as a way to mark entry and exit points
entryZscore <- 1
exitZscore <- 0

longsEntry <- zScore < -entryZscore
longsExit <- zScore >= -exitZscore
shortsEntry <- zScore > entryZscore
shortsExit <- zScore <= exitZscore

unitsLong <- rep(NA, dim(port)[1])
unitsShort <- rep(NA, dim(port)[1])

unitsLong[1] <- 0
unitsLong[longsEntry] <- 1
unitsLong[longsExit] <- 0
unitsLong <- na.locf(unitsLong)

unitsShort[1] <- 0
unitsShort[shortsEntry] <- -1
unitsShort[shortsExit] <- 0
unitsShort <- na.locf(unitsShort)

units <- unitsLong + unitsShort
ret <- portfolio_ret(units, weights, df)
plot(cumsum(ret), main="Cumulative Returns of USO-GLD Spread w/ Bollinger Band")
