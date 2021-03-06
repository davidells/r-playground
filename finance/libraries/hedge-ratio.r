# Adapted from http://quanttrader.info/public/betterHedgeRatios.pdf
hedgeRatio <- function (x, y, method=c("ols", "tls")) {
  method <- match.arg(method)
  if (method == "ols") {
    coef(lm(y ~ x))[2]
  } else if (method == "tls") {
    r <- princomp(~ y + x)
    r$loadings[1,1] / r$loadings[2,1]
  }
}

hedgeRatios <- function(x, y, lookback = 20, ...) {
  df <- as.xts(data.frame(x, y))
  rollapply(df,
    width = lookback, 
    by.column = FALSE,
    FUN = function (window) { 
      hedgeRatio(window[,1], window[,2], ...) 
  })
}
