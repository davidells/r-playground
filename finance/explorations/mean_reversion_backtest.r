# Reproduction of Example 2.7 and 2.8 from Ernie Chan's book "Algorithmic Trading"
library('quantmod')

# Example from Ernie Chan's book, of EWA vs EWC (note we can't get 2006 data
# from default getSymbols provider yahoo, so ours starts at 2007)
getSymbols(c("EWC", "EWA", "IGE"), from="2007-01-01", to="2012-04-09")

# Bring price series together into a data frame
df <- as.xts( data.frame(Ad(EWC), Ad(EWA), Ad(IGE)) )

# Run the johansen test on this set of series
jo <- johansen(df)

# TODO: If this becomes a function, we have to do some checking re: johansen
# trace and eigen value stats that have been returned

# Create a portfolio using the securities in our data frame, weighted according
# to the best weighting found from the johansen test
port <- portfolio(df, jo$bestWeights)

# Plot the portfolio price
plotWithStdDev(port)

# Are we over exposed to any one security?
if (correlatedWithSecurity( port, df )) {
  print("Portfolio is overly correlated to an underlying")
  cor(data.frame(port, df))
}

# Figure out mean reversion halflife
lookback <- round(halflife(port))

# Amount of unit portfolio to hold is determined by the negative of the z-score
# of the series.
zScore <- (port - movingAvg(port, lookback)) / movingStd(port, lookback)
units <- -zScore

# Expand our units vector and create weights vector that agrees with original
# series dataframe in terms of dimensions.
units <- rep.col(units, dim(df)[2])
weights <- rep.row(jo$bestWeights, dim(df)[1])

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

plot(cumsum(ret), main="Cumulative Returns of EWC-EWA-IGE Mean Reversion")
