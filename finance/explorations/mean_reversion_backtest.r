# Reproduction of Example 2.7 and 2.8 from Ernie Chan's book "Algorithmic Trading"
library('quantmod')

# Example from Ernie Chan's book, of EWA vs EWC (note we can't get 2006 data
# from default getSymbols provider yahoo, so ours starts at 2007)
getSymbols(c("EWC", "EWA", "IGE"), from="2007-01-01", to="2012-04-09")

df <- data.frame(Ad(EWC), Ad(EWA), Ad(IGE))
jo <- johansen(df)
print(jo)
port <- portfolio(df, jo$bestWeights)
plotWithStdDev(port)
lookback <- round(halflife(port))
print(lookback)

zScore <- (port - movingAvg(port, lookback)) / movingStd(port, lookback)
units <- -zScore

units <- rep.col(units, dim(df)[2])
weights <- rep.row(jo$bestWeights, dim(df)[1])

positions <- as.xts(units * weights * df)

series <- as.xts(df)
pnl <- na.omit( lag(positions, 1) * (diff(series) / lag(series, 1)) )
pnl <- as.xts( apply(pnl, 1, FUN = sum) )
grossMktVal <- na.omit( abs(lag(positions, 1)) )
ret <- pnl / as.xts( apply(grossMktVal, 1, FUN = sum) )