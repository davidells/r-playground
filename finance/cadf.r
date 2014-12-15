# Cointegrating Augmented Dickey Fuller test. Takes
# two series, and runs the adf test on the two spreads between
# them using hedge ratios using the given method (ols or tls)
cadf <- function (x, y, method="ols", ...) {
  
  hedgeY <- hedgeRatio(x, y, method=method)
  
  if (method == "tls") {
    adf <- adf(x - hedgeY * y, ...)
    
  } else if (method == "ols") {
    hedgeX <- hedgeRatio(y, x, method=method)
    adfX <- adf(x - hedgeY*y, ...)
    adfY <- adf(y - hedgeX*x, ...)
    
    if (adfX$statistic < adfY$statistic) {
      adf <- adfX
    } else {
      adf <- adfY
    }
  }
  
  adf$hedgeRatioMethod <- method
  return(adf)
}