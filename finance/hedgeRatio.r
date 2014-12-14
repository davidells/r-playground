# Adapted from http://quanttrader.info/public/betterHedgeRatios.pdf
hedgeRatio <- function(x, y, method=c("ols", "tls")) {
  method <- match.arg(method)
  if (method == "ols") {
    coef(lm(x ~ y))[2]
  } else if (method == "tls") {
    r <- princomp(~ x + y)
    r$loadings[1,1] / r$loadings[2,1]
  }
}