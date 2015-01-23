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
lookback <- round(halflife(port))

# Amount of unit portfolio to hold is determined by the negative of the z-score
# of the series.
zScore <- (port - movingAvg(port, lookback)) / movingStd(port, lookback)
units <- -zScore

# Expand our weights vector to match row size of our dataframe.
weights <- rep.row(jo$bestWeights, dim(df)[1])

# Get the returns for this portfolio
ret <- portfolio_ret(units, weights, df)

# Plot cumulative returns of the portfolio
plot(cumsum(ret), main="Cumulative Returns of EWC-EWA-IGE Mean Reversion")
