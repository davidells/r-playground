# RescaledRange adapted from HurstK in FGN package
RescaledRange <- function (z) 
{
  y <- z - mean(z)
  S <- cumsum(y)
  R <- (max(S) - min(S))/sqrt(sum(y^2)/length(y))
  return(R)
}

# Implemented based on method discussed at
# http://www.bearcave.com/misl/misl_tech/wavelets/hurst/index.html
# Uncomment debug lines and run against brown72.h
HurstExponent <- function(series) {
  len <- length(series)
  upperBound <- floor(log2(len)) - 1
  divisions <- 0:upperBound
  hurstVals <- matrix(0, length(divisions), 2)
  #debug <- matrix(0, length(divisions), 4)
  for (i in divisions) {
    n <- floor(len / 2^i)
    rangeSeries <- rollapply(series, width=n, FUN="RescaledRange", by=n)
    rangeMean <- mean(rangeSeries, na.rm=TRUE)
    hurstVals[i+1,] = c(log2(n), log2(rangeMean))
    #debug[i+1,] = c(n, rangeMean, log2(n), log2(rangeMean))
  }
  #print(debug)
  hurstLm <- lm(hurstVals[,2] ~ hurstVals[,1])
  return(hurstLm$coefficients[2])
}

#HurstExponent(rnorm(2000, sd=1.0))
#bwn <- read.csv("brown72.h", header=FALSE)[,1]
#HurstExponent(bwn)