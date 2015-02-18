library(gdata) #trim
library(PerformanceAnalytics)
library(parallel)
library(quantmod)
library(XML)

# Load local libraries
source("libraries/load.r", chdir=TRUE)

# A basic pipeline for digging up pairs exhibiting mean reversion
etfs.by.volume_best.pairs <- function(){
  
  # Constants
  CPU_CORES <- 8
  INTERACTIVE <- FALSE
  SKIP_FETCH <- FALSE
  
  # If in interactive mode, use normal lapply, otherwise use the
  # parallel mclapply version
  list.apply <- if (INTERACTIVE) lapply else function(xs, fun){
    mclapply(xs, fun, mc.cores=CPU_CORES)
  }
  
  # Get tickers of top 99 ETFs by volume
  url <- "http://etfdb.com/compare/volume/"
  doc <- htmlTreeParse(url, useInternalNodes=TRUE)
  symbols <- unlist(xpathApply(doc, "//table/tr/td[1]//a", xmlValue))
  
  # Limit for debugging purposes
  symbols <- symbols[1:50]
  
  # Fetch some bit of data for each
  # TODO: How can I save the data retrieved so I don't
  # always have to fetch?
  #if (!SKIP_FETCH) {
  #  getSymbols(symbols, from="2013-01-01")
  #}
  
  # If we have the data cached, use that
  if (file.exists("/tmp/top99etf")) {
    load("/tmp/top99etf")

  # Otherwise, go fetch it, and cache it
  } else {
    top99etf <- new.env()
    getSymbols(symbols, from="2013-01-01", env = top99etf, auto.assign = T)
    save(top99etf, file = "/tmp/top99etf")
  }
  
  # Generate a (N x 2) matrix of all possible symbols pairs (ignoring order)
  symbolPairs <- t(combn(symbols, 2))
  
  # Create list of paired price series, for each symbol pair
  pairs <- apply(symbolPairs, 1, function(pair){
    
    # Note we're locking ourselves into adjusted close here
    return(list(
      symbols = pair,
      y = Ad( top99etf[[ pair[1] ]] ),
      x = Ad( top99etf[[ pair[2] ]] )))
  })
  
  
  # -------------------------------------------------------------
  # Filter out pairs based on some initial assessments.
  # -------------------------------------------------------------
  
  # In this case, use low correlation to rule out some pairs.
  pairs <- Filter(function(pair){
    rows(pair$x) == rows(pair$y) && cor(pair$x, pair$y) > 0.5
  }, pairs)
  
  # Collect some analysis on our price series pairs, like
  # the CADF test (TODO: Add other tests)
  # TODO: Make this pluggable
  analyzed <- list.apply(pairs, function(pair){
    list("pair" = pair, 
         "cadf" = cadf(pair$x, pair$y, method="tls"))
  })
  
  # Filter out pairs that don't meet some test
  # result requirements
  # TODO: Also make this pluggable
  candidates <- Filter(function(result){
    return( result$cadf$p.value < 0.05 )
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
  testResults <- unlist(list.apply(candidates, function(result) {
    list(result$pair$symbols, 
         result$cadf$p.value)
  }))
  
  tableCols <- length(candidates[[1]]) + 1
  testResults <- matrix(testResults, ncol=tableCols, byrow=TRUE)
  colnames(testResults) <- c("y", "x", "cadf.pval")
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
  portfolios <- list.apply(candidates, function(result){
    
    # Extract price series
    pair <- result$pair
    y <- pair$y
    x <- pair$x
  
    # Determine weights using a static "tls" hedge ratio.
    b <- hedgeRatio(y, x, method="tls")
    weights <- rep.row(c(1, -b), rows(y))
    
    # Create portfolio using given series and weights
    df <- as.xts( data.frame(y, x) )
    port <- portfolio(df, weights)
    
    # Use this portfolio series to discover a lookback
    # that can power a dynamic hedge ratio.
    lookback <- round(halflife(port))
    
    if (INTERACTIVE) {
      cat(
        "Pair: ", pair$symbols, ", ",
        "Lookback:", lookback, "\n",
        "Length(weights):", length(weights), "\n")
    }
    
    # The lookback should also be used here to filter out the candidate.
    # Should that be achieved earlier in processing somehow? At any rate,
    # if it's negative, we can do no more
    if (lookback <= 0 || lookback > rows(port)) {
      return(list(
        "symbols"=result$pair$symbols,
        "error"="Negative lookback"))
    }
    
    # Dynamic hedge ratio based on lookback
    # -------------------------------------
    # B <- hedgeRatios(y, x, lookback=lookback, method="ols")
    # weights <- cbind( ones(rows(B)), -B )
    # port <- portfolio(df, weights)
  
    result <- c(result)
    result$lookback = lookback
    result$weights = weights
    result$series = na.omit(port)
    
    return (result)
  })
  
  # -------------------------------------------------------------
  # Filter out portfolios based on their attributes
  # -------------------------------------------------------------
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
  
  
  # -------------------------------------------------------------
  # Apply trading strategy to portfolios to create return series
  # -------------------------------------------------------------
  
  portfolios <- lapply(portfolios, function(p){
    
    # TODO: Put bollinger band strategy in function
    # TODO: Make strategy pluggable
    series <- p$series
    lookback <- p$lookback
    weights <- p$weights
    
    # zScore based on rolling average and standard deviation
    # zScore <- (series - movingAvg(series, lookback)) / movingStd(series, lookback)
    
    # zScore based on static mean and standard deviation
     zScore <- (series - mean(series)) / sd(series)
    
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
    
    if (length(units) <= 1) {
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
    p$returns <- returns
    p$totalReturn <- sum(returns)
    p$annualSharpe <- PerformanceAnalytics::SharpeRatio.annualized(returns)
    p$annualReturn <- PerformanceAnalytics::Return.annualized(returns)
    return (p)
  })
  
  # -------------------------------------------------------------
  # Filter portfolios based on returns or other evaluation
  # -------------------------------------------------------------
  portfolios <- Filter(function(p){
    is.null(p$error) && p$totalReturn > 0.1 && p$annualSharpe > 0.5
  }, portfolios)
  
  
  # -------------------------------------------------------------
  # Rank the overall portfolio and strategy
  # -------------------------------------------------------------
  score <- sapply(portfolios, function(p){
    #sum(p$returns)  #Total return
    p$annualSharpe # Sharpe ratio
  })
  
  # Sort the portfolios by their score
  portfolios <- 
    portfolios[order(score, decreasing=TRUE)]
  
  # TODO: Support arbitrary limit on the number of returned portfolios here?
  
  # Review returns
  if (INTERACTIVE) {
    lapply(portfolios, function(p){
      
      symbols <- p$pair$symbols
      label <- paste(
        "Cumulative returns of",
        symbols[1], symbols[2], "\n",
        "  sharpe ratio =", round(p$annualSharpe, 2), "\n",
        "  annual return =", round(p$annualReturn, 2),
        sep = " ")
      
      plot(cumsum(p$returns), main = label)
      readline( "Hit enter to continue" )
    })
  }
  
  return( portfolios )
}

# What to do with this output? 
# porfolios <- etfs.by.volume_best.pairs()
# lapply
