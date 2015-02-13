library(quantmod)
library(gdata) #trim
library(XML)
library(parallel)

# Load local libraries
source("libraries/load.r", chdir=TRUE)

interactive <- TRUE

# Get tickers of top 99 ETFs by volume
url <- "http://etfdb.com/compare/volume/"
doc <- htmlTreeParse(url, useInternalNodes=TRUE)
symbols <- unlist(xpathApply(doc, "//table/tr/td[1]//a", xmlValue))

# Limit for debugging purposes
symbols <- symbols[1:10]

# Fetch some bit of data for each
getSymbols(symbols, from="2013-01-01")

# Generate a (N x 2) matrix of all possible symbols pairs
symbolPairs <- t(combn(symbols, 2))

# Create list of paired price series, for each symbol pair
pairs <- apply(symbolPairs, 1, function(pair){
  return( list(
    symbols = pair,
    y = Ad(get(pair[1])), 
    x = Ad(get(pair[2])))
  )
})

# Filter out pairs based on some initial assessments.
# In this case, use low correlation to rule out some pairs.
pairs <- Filter(function(pair){
  return( cor(pair$x, pair$y) > 0.5 )
}, pairs)

# Collect some analysis on our price series pairs, like
# the CADF test (TODO: Add other tests)
analyzed <- lapply(pairs, function(pair){
  list("pair" = pair, 
       "cadf" = cadf(pair$x, pair$y, method="tls"))
})

# Filter out pairs that don't meet some test
# result requirements
candidates <- Filter(function(result){
  return( result$cadf$p.value < 0.05 )
}, analyzed)

# Did our filter return nothing? If so, stop.
if (length(candidates) == 0) {
  stop("No qualified candidates.")
}

# Capture table of test results.
#
# TODO: This is just for visual stuff anyway,
# but would be nice to have a data frame instead
# of a matrix, with correct types for the test results.
testResults <- unlist(lapply(candidates, function(result) {
  list(result$pair$symbols, 
       result$cadf$p.value)
}))
tableCols <- length(candidates[[1]]) + 1
testResults <- matrix(testResults, ncol=tableCols, byrow=TRUE)
colnames(testResults) <- c("y", "x", "cadf.pval")
rownames(testResults) <- seq(rows(testResults))

# A good time to stop and look at testResults.
if (interactive) {
  print( testResults )
  readline( "Hit enter to continue" )
}

# Create a portfolio for each of these series, using
# some strategy for defining the portfolio like:
#   static hedge ratio
#   dynamic hedge ratio 
#   kalman filtering
#   what else?
#
# For now, just use a static hedge ratio defined via "tls" method.
portfolios <- mclapply(candidates, function(result){
  
  # Extract price series
  pair <- result$pair
  y <- pair$y
  x <- pair$x

  # Determine weights using a static "tls" hedge ratio.
  b <- hedgeRatio(y, x, method="tls")
  weights <- c(1, -b)
  
  # Create portfolio using given series and weights
  df <- as.xts( data.frame(y, x) )
  port <- portfolio(df, weights)
  
  # Use this portfolio series to discover a lookback
  # that can power a dynamic hedge ratio.
  lookback <- round(halflife(port))
  
  # The lookback should also be used here to filter out the candidate.
  # Should that be achieved earlier in processing somehow? At any rate,
  # if it's negative, we can do no more
  if (lookback < 0) {
    return(list(
      "symbols"=result$pair$symbols,
      "error"="Negative lookback"))
  }
  
  # Now create a portfolio using a dynamic hedge ratio using discovered lookback.
  # We seem to have to use OLS for rolling hedge ratios
  B <- hedgeRatios(y, x, lookback=lookback, method="ols")
  weights <- cbind( ones(rows(B)), -B )
  port <- portfolio(df, weights)

  result <- c(result)
  result$hedgeMethod = "ols.moving-average"
  result$lookback = lookback
  result$weights = weights
  result$series = na.omit(port)
  
  return (result)
}, mc.cores = 4)

# Filter out portfolios based on lookback and other factors
# TODO: Move this to mclapply block above?
portfolios <- Filter(function(p){  
  if (!is.null(p$error)) {
    return (FALSE)
  }

  if (p$lookback > 30) {
    return (FALSE)
  }
  
  if (rows(p$series) <= 0) {
    return (FALSE)
  }

  securities <- as.xts( data.frame(p$pair$y, p$pair$x) )
  securities <- truncateRows(securities, rows(p$series))
  
  falseHedge <- correlatedWithConstituent(
    p$series, securities, threshold = 0.6)
  
  return (!falseHedge)
}, portfolios)


# Review portfolios
if (interactive) {
  lapply(portfolios, function(p){
    
    symbols <- p$pair$symbols
    label <- paste(
      "Porfolio of",
      symbols[1], symbols[2], 
      ", pVal =", round(p$cadf$p.value, 2),
      ", lookback =", p$lookback,
      sep = " ")
    
    plotWithStdDev(p$series, main = label)
    readline( "Hit enter to continue" )
  })
}

# Evaluate trading strategy for portfolio
# Move this to mclapply block?
portfolios <- lapply(portfolios, function(p){
  
  # TODO: Replace with bollinger band strategy
  series <- p$series
  lookback <- p$lookback
  weights <- p$weights
  
  zScore <- (series - movingAvg(series, lookback)) / movingStd(series, lookback)
  
  entryZscore <- 1
  exitZscore <- 0
  
  longsEntry <- zScore < -entryZscore
  longsExit <- zScore >= -exitZscore
  shortsEntry <- zScore > entryZscore
  shortsExit <- zScore <= exitZscore
  
  unitsLong <- rep(NA, rows(series))
  unitsShort <- rep(NA, rows(series))
  
  unitsLong[1] <- 0
  unitsLong[longsEntry] <- 1
  unitsLong[longsExit] <- 0
  unitsLong <- na.locf(unitsLong)
  
  unitsShort[1] <- 0
  unitsShort[shortsEntry] <- -1
  unitsShort[shortsExit] <- 0
  unitsShort <- na.locf(unitsShort)
  
  units <- unitsLong + unitsShort
  
  securities <- as.xts( data.frame(p$pair$y, p$pair$x) )

  weights <- truncateRows(weights, length(units))
  securities <- truncateRows(securities, length(units))
  
  returns <- portfolio_ret(units, weights, securities)
  
  p <- c(p)
  p$returns <- returns
  p$totalReturn <- sum(returns)
  p$sharpe <- mean(returns) / sd(returns)
  return (p)
})

# Filter portfolios based on returns
portfolios <- Filter(function(p){
  p$totalReturn > 0
}, portfolios)

# Rank the portfolios + strategy
score <- sapply(portfolios, function(p){
  #sum(p$returns)  #Total return
  p$sharpe # Sharpe ratio
})

scoreOrder <- order(score, decreasing = TRUE)
portfolios <- portfolios[scoreOrder]

# Review returns
if (interactive) {
  lapply(portfolios, function(p){
    
    symbols <- p$pair$symbols
    label <- paste(
      "Cumulative returns of",
      symbols[1], symbols[2], 
      "(sharpe =", round(p$sharpe, 2), ")",
      sep = " ")
    
    plot(cumsum(p$returns), main = label)
    readline( "Hit enter to continue" )
  })
}