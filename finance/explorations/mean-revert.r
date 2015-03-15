library(gdata) #trim
library(PerformanceAnalytics)
library(parallel)
library(quantmod)
library(XML)

# Load local libraries
source("libraries/load.r", chdir=T)
source("libraries/mean-reversion/load.r", chdir=T)

# ----------------------------------------------------------------
# ----------------------------------------------------------------
# A basic pipeline for digging up pairs exhibiting mean reversion
# ----------------------------------------------------------------
# ----------------------------------------------------------------
pairs.pipeline <- function(strategy){
  
  # Constants
  CPU_CORES <- 8
  DEBUG <- FALSE
  
  # If in debug mode, use normal lapply, otherwise use the
  # parallel mclapply version
  list.compute <- if (DEBUG) lapply else function(xs, fun){
    mclapply(xs, fun, mc.cores=CPU_CORES)
  }
  
  # Get symbol pairs and price data from specific strategy
  symbolPairs <- strategy$getSymbolPairs()
  priceData <- strategy$getStockData(symbolPairs)
  
  # Create list of paired price series, for each symbol pair
  pairs <- apply(symbolPairs, 1, function(pair){
    # Note we're locking ourselves into adjusted close here
    # TODO: That's a big deal and we need to consider attaching the whole
    # OHLC structure here instead.
    return(list(
      symbols = pair,
      y = Ad( priceData[[ pair[1] ]] ),
      x = Ad( priceData[[ pair[2] ]] )))
  })
  
  # The data between the pairs should align
  pairs <- Filter(function(pair){
    rows(pair$x) == rows(pair$y)
  }, pairs)
  
  # -------------------------------------------------------------
  # Collect the strategy's analysis on our price series pairs
  # -------------------------------------------------------------
  analyzed <- list.compute(pairs, function(pair){
    c(list("pair" = pair), 
      strategy$analyzePair(pair))
  })
  
  # Filter out pairs that didn't meet some test
  # result requirements. Notice this is done by returning
  # a list that has a 'reject' field (with a string value
  # that tells the reason for rejection)
  candidates <- Filter(function(candidate){
    is.null(candidate$reject)
  }, analyzed)
  
  # -------------------------------------------------------------
  # Was everything rejected? If so, stop.
  # -------------------------------------------------------------
  if (length(candidates) == 0) {
    stop("No qualified candidates.")
  }
  
  # -------------------------------------------------------------
  # Capture table of test results.
  # -------------------------------------------------------------
  #
  # TODO: This is just for visual stuff anyway,
  # but would be nice to have a data frame instead
  # of a matrix, with correct types for the test results.
  testResults <- unlist(lapply(candidates, function(result) {
    list("y" = result$pair$symbols[[1]], 
         "x" = result$pair$symbols[[2]], 
         strategy$getTestTableValues(result))
  }))
  
  tableCols <- length(candidates[[1]]) + 1
  testResults <- matrix(testResults, ncol=tableCols, byrow=TRUE)
  colnames(testResults) <- c("y", "x", names(candidates[[1]])[-1])
  rownames(testResults) <- seq(rows(testResults))
  
  # A good time to stop and look at testResults. 
  if (DEBUG) {
    print( testResults )
    readline( "Hit enter to continue" )
  }
  
  # -------------------------------------------------------------
  # Create a portfolio for each of these series
  # -------------------------------------------------------------
  portfolios <- list.compute(candidates, function(candidate){
    strategy$createPortfolio(candidate)
  })
  
  # -------------------------------------------------------------
  # Filter out portfolios based on their attributes
  # -------------------------------------------------------------
  portfolios <- Filter(function(p) {
    rows(p$series) > 0 && strategy$filterPortfolio(p)
  }, portfolios)
  
  
  # -------------------------------------------------------------
  # Apply trading strategy to portfolios to create return series
  # -------------------------------------------------------------
  portfolios <- list.compute(portfolios, function(p){
    units <- strategy$getPosition(p)
    
    if (length(units) <= 1 || all(is.na(units))) {
      return(list(
        "symbols"=p$pair$symbols,
        "reject"="Not enough transactions in strategy"
      ))
    }
    
    weights <- truncateTo(p$weights, length(units), from.head=T)
    
    securities <- as.xts( data.frame(p$pair$y, p$pair$x) )
    securities <- truncateTo(securities, length(units), from.head=T)
    
    returns <- portfolio_ret(units, weights, securities)
    
    p <- c(p)
    p$units <- units
    p$returns <- returns
    p$return.total <- sum(returns)
    p$sharpe.annual <- PerformanceAnalytics::SharpeRatio.annualized(returns)
    p$return.annual <- PerformanceAnalytics::Return.annualized(returns)
    p$num.trades <- length(rle(units)$values) - 1
    
    return (p)
  })
  
  # -------------------------------------------------------------
  # Filter portfolios based on returns or other evaluation
  # -------------------------------------------------------------
  portfolios <- Filter(function(p){
    is.null(p$reject) && strategy$filterStrategyResult(p)
  }, portfolios)
  
  
  # -------------------------------------------------------------
  # Rank the overall portfolio and strategy
  # -------------------------------------------------------------
  score <- sapply(portfolios, strategy$scoreStrategyResult)
  
  # Sort the portfolios by their score
  portfolios <- portfolios[order(score, decreasing=TRUE)]
  
  # TODO: Add score to portfolio / result / final output
  
  return( portfolios )
}

portfolio.plots.return <- function (p) {
  symbols <- p$pair$symbols
  label <- paste(
    "Cumulative returns of ",
    symbols[1], " ", symbols[2], "\n",
    "  sharpe ratio = ", round(p$sharpe.annual, 2), ", ",
    "  annual return = ", round(p$return.annual, 2),
    sep = "")
  
  plot(cumsum(p$returns), main=label)
}

portfolio.plots.price <- function (p) {
  symbols <- p$pair$symbols
  label <- paste(
    "Porfolio of",
    symbols[1], symbols[2], 
    ", pVal =", round(p$cadf$p.value, 2),
    ", lookback =", p$lookback,
    sep = " ")
  
  plotWithStdDev(p$series, main=label)
}

portfolio.plots.position <- function (p) {
  symbols <- p$pair$symbols
  label <- paste(
    "Position in",
    symbols[1], symbols[2], 
    sep = " ")
  
  plotWithStdDev(p$units, main=label, type="l")
}

portfolio.plots <- function (p) {
  portfolio.plots.price(p)
  portfolio.plots.position(p)
  portfolio.plots.return(p)
}


# Run top100etf strategy through the pipeline
portfolios <- pairs.pipeline(top100etf.mean.reversion)

# TODO: More general way to do this?
portfolio.results <- (function(){
  symbolMatrix <- t( sapply(portfolios, function(p) p$pair$symbols) )
  data.frame( 
    "y" = symbolMatrix[,1], 
    "x" = symbolMatrix[,2],
    "cadf.pval" = sapply(portfolios, function(p) p$cadf$p.value),
    "lookback" = sapply(portfolios, function(p) p$lookback),
    "hedge.lookback" = sapply(portfolios, function(p) p$hedge.lookback),
    "sharpe.annual" = sapply(portfolios, function(p) p$sharpe.annual),
    "return.annual" = sapply(portfolios, function(p) p$return.annual),
    "num.trades" = sapply(portfolios, function(p) p$num.trades),
    "return.total" = sapply(portfolios, function(p) p$return.total))
})()

portfolio.plots(portfolios[[1]])
