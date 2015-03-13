top100etf.mean.reversion <- list(
  
  # -------------------------------
  # getSymbolPairs
  #
  # Get tickers of top 100 ETFs by volume, and provide every
  # possible pair among them.
  # -------------------------------
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
  
  
  # -------------------------------
  # getStockData:
  #
  # Get daily data from Yahoo, and cache it. Note, you need
  # to delete the cache file if you change the symbols handed in
  # -------------------------------
  "getStockData" = function(symbolPairs) {
    symbols <- unique(as.vector(symbolPairs))
    
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
  
  
  # -------------------------------
  # analyzePair:
  #
  # Reject pairs that are not correlated.
  # Require pairs to have a CADF p value of 95% or higher (testing cointegration).
  # Determine the "lookback" for rolling hedge ratios, moving averages, etc. In this
  #   case, use the "halflife" (time to mean reversion) of the portfolio price series.
  # -------------------------------
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
    b <- hedgeRatio(x, y, method="tls")
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
  
  # --------------------------------------------------
  # getTestTableValues
  #
  # Utility function, return specific numbers from analysis above
  # --------------------------------------------------
  "getTestTableValues" = function(result) {
    
    list(
      "cadf.pval" = result$cadf$p.value,
      "lookback" = result$lookback)
  },
  
  # --------------------------------------------------
  # createPortfolio
  #
  # Create a portfolio using a rolling hedge ratio based on 
  # the lookback given from the analysis step.
  # --------------------------------------------------
  "createPortfolio" = function(candidate) {
    
    # Create using some strategy for defining the portfolio like:
    #   static hedge ratio
    #   dynamic hedge ratio 
    #   kalman filtering
    #   what else?
    
    # Extract price series
    pair <- candidate$pair
    y <- pair$y
    x <- pair$x
    df <- as.xts( data.frame(y, x) )
    lookback <- candidate$lookback
    
    # collect portfolio data
    
    # -------------------------------------
    # Static hedge ratio using "tls" (warning: data snooping bias here!)
    # -------------------------------------
    #hedge.lookback <- 0
    #b <- hedgeRatio(x, y, method="tls")
    #weights <- rep.row(c(1, -b), rows(y))
    #port <- portfolio(df, weights)
    
    # -------------------------------------
    # Dynamic hedge ratio based on arbitrary N
    # -------------------------------------
    #hedge.lookback <- 100
    #B <- hedgeRatios(x, y, lookback=hedge.lookback, method="ols")
    #weights <- cbind( ones(rows(B)), -B )
    #port <- portfolio(df, weights)
    
    # -------------------------------------
    # Dynamic hedge ratio based on lookback
    # -------------------------------------
    hedge.lookback <- lookback
    B <- hedgeRatios(x, y, lookback=lookback, method="ols")
    weights <- cbind( ones(rows(B)), -B )
    port <- portfolio(df, weights)
    
    candidate <- c(candidate)
    candidate$hedge.lookback = hedge.lookback
    candidate$weights = weights
    candidate$series = na.omit(port)
    
    return (candidate)
  },
  
  # --------------------------------------------------
  # filterPortfolio
  #
  # Now that a portfolio has been constructed, we want
  # to make sure that it isn't overly correlated with one of
  # it's constituents. In this strategy, we want actual
  # pairs where each instrument contributes to the portfolio.
  # --------------------------------------------------
  "filterPortfolio" = function(portfolio){  
    p <- portfolio
    
    securities <- as.xts( data.frame(p$pair$y, p$pair$x) )
    securities <- truncateTo(securities, rows(p$series), from.head=T)
    
    falseHedge <- correlatedWithConstituent(
      p$series, securities, threshold = 0.6)
    
    # Anything else?
    return (!falseHedge)
  },
  
  # --------------------------------------------------
  # getPosition
  #
  # Determine daily position in the unit portfolio
  # --------------------------------------------------
  "getPosition" = function(portfolio) {
    p <- portfolio
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
  },
  
  "filterStrategyResult" = function(result) {
    result$return.annual > 0.1 && result$sharpe.annual > 0.5
  },
  
  "scoreStrategyResult" = function(p){
    #sum(p$returns)  #Total return
    p$sharpe.annual # Sharpe ratio
  }
)  