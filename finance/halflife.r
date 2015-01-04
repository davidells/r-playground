# Computes half life of mean reversion for given series.
# See Chan, Ernie - Algorithmic Trading, Ch. 2
halflife <- function (y) {
  yDelta <- diff(y)[2:length(y)]
  y <- y[2:length(y)]
  lm <- lm(yDelta ~ y)
  -(-log(2) / lm$coefficients[2])
}
