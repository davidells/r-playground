# Cointegrating Augmented Dickey Fuller test. Takes
# two series, and runs the adf test on the two spreads between
# them using hedge ratios using the given method (ols or tls)
cadf <- function (x, y, method="tls", k=1, model=2) {
  
  hedgeX <- hedgeRatio(x, y, method=method)
  
  # If method is "tls", hedge is symmetric, so return adf
  if (method == "tls") {
    adf <- adf(y - hedgeX * x, k=k, model=model)
    
  # Otherwise, we try both sides, and pick the one with
  # the best ADF statistic
  } else if (method == "ols") {
    adfX <- adf(y - hedgeX*x, k=k, model=model)
    
    hedgeY <- hedgeRatio(y, x, method=method)
    adfY <- adf(x - hedgeY*y, k=k, model=model)
    
    if (adfX$statistic < adfY$statistic) {
      adf <- adfX
    } else {
      adf <- adfY
    }
  }
  
  adf$hedgeRatioMethod <- method
  return(adf)
}

# Run cadf using ols and tls regression method, and using the ADF model "model"
# and with lag order of k. Return best ADF test found (lowest p value). Anything
# below 0.10 indicates 90% certainty of mean reversion, 0.05 indicates 95%, etc.
cadf.best <- function (x, y, model=2, k=1) {
  debug <- FALSE

  # The hedge ratio used in cadf is determined by default by fitting a linear model using lm.
  # We'll also use an alternative regression using tls and test out the pair that way.
  
  # OLS can create two different hedge ratios based on which series
  # is considered independent vs dependent.
  cadf.ols <- cadf(x, y, method="ols", model=model, k=k)
  
  # TLS is symmetric, so order of series doesn't matter. See
  # http://quanttrader.info/public/betterHedgeRatios.pdf
  cadf.tls <- cadf(x, y, method="tls", model=model, k=k)
  
  # Capture smallest p-value among our tests
  pValueMin <- min(
    cadf.ols$p.value, 
    cadf.tls$p.value)
  
  if (pValueMin == cadf.ols$p.value) {
    cadf <- cadf.ols
  } else {
    cadf <- cadf.tls
  }
  return(cadf)
}