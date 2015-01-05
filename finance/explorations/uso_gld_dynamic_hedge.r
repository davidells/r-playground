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

# *Note: Also worth exploring a dynamic set of weights, perhaps through the use
# of roll apply?

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
units <- rep.col(units, dim(df)[2])

# Determine daily positions for each of the series, according to their portfolio
# weight and the amount of the unit portfolio we are holding
positions <- as.xts(units * weights * df)

# Get profit and loss according to our positions and the daily
# percent change of each security.
pnl <- na.omit( lag(positions, 1) * (diff(df) / lag(df, 1)) )
pnl <- as.xts( apply(pnl, 1, FUN = sum) )

# Calculate gross market value (without margin, this is equal to capital invested)
# as the absolute value of the dollar amount for each of our positions
grossMktVal <- na.omit( abs(lag(positions, 1)) )
grossMktVal <- as.xts( apply(grossMktVal, 1, FUN = sum) )

# Finally, calculate daily returns as our daily profit and loss divided by the
# daily gross market value.
ret <- pnl / grossMktVal

plot(cumsum(ret), main="Cumulative Returns of USO-GLD Spread w/ Dynamic Hedge Ratio")

# TODO: Finish example by using Bollinger Bands for entry / exit