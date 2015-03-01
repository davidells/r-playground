library(gdata) #trim
library(PerformanceAnalytics)
library(parallel)
library(quantmod)
library(XML)

# Load local libraries
source("libraries/load.r", chdir=TRUE)

etfs.by.volume_best.pairs <- function(){
  
  strategy <- list(
    
    # Get tickers of top 100 ETFs by volume, and provide every
    # possible pair among them.
    "getSymbolPairs"=function(){
      
      # Get tickers of top 100 ETFs by volume from barchart.com
      url <- "http://www.barchart.com/etf/vleaders.php"
      doc <- htmlTreeParse(url, useInternalNodes=TRUE)
      symbols <- unlist(xpathApply(doc, "//table/tbody/tr/td[1]//a", xmlValue))
      
      # Limit for debugging purposes
      symbols <- symbols[1:50]
      
      # Generate a (N x 2) matrix of all possible symbols pairs (ignoring order)
      symbolPairs <- t(combn(symbols, 2))
    },
    
    
    # Get daily data from Yahoo, and cache it. Note, you need
    # to delete the cache file if you change the symbols handed in
    "getStockData" = function() {
      
      # If we have the data cached, use that
      if (file.exists("/tmp/top99etf")) {
        load("/tmp/top99etf")
        
      # Otherwise, go fetch it, and cache it
      } else {
        top99etf <- new.env()
        getSymbols(symbols, from="2013-01-01", env = top99etf, auto.assign = T)
        save(top99etf, file = "/tmp/top99etf")
      }
      
      return( top99etf )
    },
    
    
    # Analyze the pair
    "analyzePair" = function(pair) {
      reject <- function(reason) {
        list("reject" = reason)
      }
      
      y <- pair$y
      x <- pair$x
      
      # Reject pairs that are not correlated
      # TODO: Make the threshold here a parameter of this strategy?
      if (cor(pair$x, pair$y) < 0.5) { 
        return(reject("Pair does exhibit initial correlation"))
      }
  
      # Collect some analysis on our price series pairs, like
      # the CADF test (TODO: Add other tests)
      cadf.results <- cadf(x, y, method="tls")
      
      # Reject pairs that fail the CADF test
      if (cadf.results$p.value > 0.05) {
        return(reject("CADF pValue too high (must be under 0.05)"))
      }
      
      # ------------------------------------------------
      # Capture lookback using "tls" static hedge ratio 
      # (warning: data snooping bias here!)
      # ------------------------------------------------
      df <- as.xts( data.frame(y, x) )
      b <- hedgeRatio(y, x, method="tls")
      weights <- rep.row(c(1, -b), rows(y))
      port <- portfolio(df, weights)
      lookback <- round(halflife(port))
      
      if (lookback < 0 || lookback > rows(port)) {
        return(reject(
          paste("Lookback (sign of mean reversion) must be above ",
                "0 and less than total size of data")))
      }
      
      list(
        "cadf" = cadf.results,
        "lookback" = lookback
      )
    },
    
    
    "getTestTableValues" = function(result) {
      list(
        "cadf.pval" = result$cadf$p.value,
        "lookback" = result$lookback)
    }
  )
  
  # Run it through the pairs pipeline
  pairs.pipeline(strategy)
}

# A basic pipeline for digging up pairs exhibiting mean reversion
pairs.pipeline <- function(strategy){
  
  # Constants
  CPU_CORES <- 8
  INTERACTIVE <- FALSE
  DEBUG <- FALSE
  SKIP_FETCH <- FALSE
  
  # If in interactive mode, use normal lapply, otherwise use the
  # parallel mclapply version
  list.compute <- if (DEBUG) lapply else function(xs, fun){
    mclapply(xs, fun, mc.cores=CPU_CORES)
  }
  
  # Get symbol pairs and price data from specific strategy
  symbolPairs <- strategy$getSymbolPairs()
  priceData <- strategy$getStockData()
  
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
  
  
  # -------------------------------------------------------------
  # Filter out pairs based on some initial assessments.
  # -------------------------------------------------------------
  
  # The data between the pairs should align
  pairs <- Filter(function(pair){
    rows(pair$x) == rows(pair$y)
  }, pairs)
  
  # Collect some analysis on our price series pairs, like
  # the CADF test (TODO: Add other tests)
  # TODO: Make this pluggable
  analyzed <- list.compute(pairs, function(pair){
    c(list("pair" = pair), 
      strategy$analyzePair(pair))
  })
  
  # Filter out pairs that don't meet some test
  # result requirements
  # TODO: Also make this pluggable
  #candidates <- Filter(strategy$filterAnalyzed, analyzed)
  candidates <- Filter(function(result){
    is.null(result$reject)
  }, analyzed)
  
  # -------------------------------------------------------------
  # Did our filter return nothing? If so, stop.
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
         strategy[['getTestTableValues']](result))
  }))
  
  tableCols <- length(candidates[[1]]) + 1
  testResults <- matrix(testResults, ncol=tableCols, byrow=TRUE)
  colnames(testResults) <- c("y", "x", names(candidates[[1]])[-1])
  rownames(testResults) <- seq(rows(testResults))
  
  # A good time to stop and look at testResults.
  if (INTERACTIVE) {
    print( testResults )
    readline( "Hit enter to continue" )
  }
  
  
  # -------------------------------------------------------------
  # Create a portfolio for each of these series
  # -------------------------------------------------------------
  
  # Using some strategy for defining the portfolio like:
  #   static hedge ratio
  #   dynamic hedge ratio 
  #   kalman filtering
  #   what else?
  #
  # For now, just use a static hedge ratio defined via "tls" method.
  portfolios <- list.compute(candidates, function(result){
    
    # Extract price series
    pair <- result$pair
    y <- pair$y
    x <- pair$x
    df <- as.xts( data.frame(y, x) )
    lookback <- result$lookback
  
    # collect portfolio data
    
    # -------------------------------------
    # Static hedge ratio using "tls" (warning: data snooping bias here!)
    # -------------------------------------
    #hedge.lookback <- 0
    #b <- hedgeRatio(y, x, method="tls")
    #weights <- rep.row(c(1, -b), rows(y))
    #port <- portfolio(df, weights)
    
    # -------------------------------------
    # Dynamic hedge ratio based on arbitrary N
    # -------------------------------------
    #hedge.lookback <- 100
    #B <- hedgeRatios(y, x, lookback=hedge.lookback, method="ols")
    #weights <- cbind( ones(rows(B)), -B )
    #port <- portfolio(df, weights)
    
    # -------------------------------------
    # Dynamic hedge ratio based on lookback
    # -------------------------------------
     hedge.lookback <- lookback
     B <- hedgeRatios(y, x, lookback=lookback, method="ols")
     weights <- cbind( ones(rows(B)), -B )
     port <- portfolio(df, weights)

    result <- c(result)
    result$hedge.lookback = hedge.lookback
    result$weights = weights
    result$series = na.omit(port)
    
    return (result)
  })
  
  # -------------------------------------------------------------
  # Filter out portfolios based on their attributes
  # -------------------------------------------------------------
  portfolios <- Filter(function(p){  
    if (rows(p$series) <= 0) {
      return (FALSE)
    }
  
    securities <- as.xts( data.frame(p$pair$y, p$pair$x) )
    securities <- chompRows(securities, rows(p$series))
    
    falseHedge <- correlatedWithConstituent(
      p$series, securities, threshold = 0.6)
    
    # Anything else?
    return (!falseHedge)
    
  }, portfolios)
  
  
  if (INTERACTIVE) {
    cat("Portfolios after initial filtering: ", length(portfolios))
  }
  
  
  # Review portfolios
  if (INTERACTIVE) {
    lapply(portfolios, function(p){
      portfolio.plots.price(p)
      readline( "Hit enter to continue" )
    })
  }
  
  
  
  # TODO: Move this function somewhere?
  # ------------------------------
  # Define some trading strategies
  # ------------------------------
  bollinger.band <- function (zScore) {
    entryZscore <- 1
    exitZscore <- 0
    
    longsEntry <- zScore < -entryZscore
    longsExit <- zScore >= -exitZscore
    shortsEntry <- zScore > entryZscore
    shortsExit <- zScore <= exitZscore
    
    unitsLong <- rep(NA, rows(zScore))
    unitsShort <- rep(NA, rows(zScore))
    
    unitsLong[1] <- 0
    unitsLong[longsEntry] <- 1
    unitsLong[longsExit] <- 0
    unitsLong <- na.locf(unitsLong)
    
    unitsShort[1] <- 0
    unitsShort[shortsEntry] <- -1
    unitsShort[shortsExit] <- 0
    unitsShort <- na.locf(unitsShort)
    
    units <- unitsLong + unitsShort
  }
  
  # -------------------------------------------------------------
  # Apply trading strategy to portfolios to create return series
  # -------------------------------------------------------------
  portfolios <- list.compute(portfolios, function(p){
    
    # TODO: Make strategy pluggable
    series <- p$series
    lookback <- p$lookback
    hedge.lookback <- p$hedge.lookback
    weights <- p$weights
    
    # -----------------------------------
    # zScore based on rolling average and standard deviation (lookback)
    # -----------------------------------
    # zScore <- (series - movingAvg(series, lookback)) / movingStd(series, lookback)
    
    # -----------------------------------
    # zScore based on rolling average and standard deviation (hedge.lookback)
    # -----------------------------------
     zScore <- (series - movingAvg(series, hedge.lookback)) / movingStd(series, hedge.lookback)
    
    # -----------------------------------
    # zScore based on static mean and standard deviation (warning: data snooping bias here!)
    # -----------------------------------
    # zScore <- (series - mean(series)) / sd(series)
    
    # -----------------------------------
    # Determine units directly as -zScore 
    # -----------------------------------
    # units <- -zScore
    
    # -----------------------------------
    # Determine units using bollinger band strategy
    # -----------------------------------
     units <- bollinger.band(zScore)
    
    if (length(units) <= 1 || all(is.na(units))) {
      return(list(
        "symbols"=p$pair$symbols,
        "error"="Not enough transactions in strategy"
      ))
    }
    
    securities <- as.xts( data.frame(p$pair$y, p$pair$x) )

    weights <- chompRows(weights, length(units))
    securities <- chompRows(securities, length(units))
    
    returns <- portfolio_ret(units, weights, securities)
    
    p <- c(p)
    p$units <- units
    p$returns <- returns
    p$return.total <- sum(returns)
    p$sharpe.annual <- PerformanceAnalytics::SharpeRatio.annualized(returns)
    p$return.annual <- PerformanceAnalytics::Return.annualized(returns)
    return (p)
  })
  
  # -------------------------------------------------------------
  # Filter portfolios based on returns or other evaluation
  # -------------------------------------------------------------
  portfolios <- Filter(function(p){
    is.null(p$error) && p$return.total > 0.1 && p$sharpe.annual > 0.5
  }, portfolios)
  
  
  # -------------------------------------------------------------
  # Rank the overall portfolio and strategy
  # -------------------------------------------------------------
  score <- sapply(portfolios, function(p){
    #sum(p$returns)  #Total return
    p$sharpe.annual # Sharpe ratio
  })
  
  # Sort the portfolios by their score
  portfolios <- 
    portfolios[order(score, decreasing=TRUE)]
  
  # TODO: Support arbitrary limit on the number of returned portfolios here?
  
  # Review returns
  if (INTERACTIVE) {
    lapply(portfolios, function(p){
      portfolio.plots.return(p)
      readline( "Hit enter to continue" )
    })
  }
  
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


portfolios <- etfs.by.volume_best.pairs()

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
    "return.total" = sapply(portfolios, function(p) p$return.total))
})()

portfolio.plots(portfolios[[1]])
