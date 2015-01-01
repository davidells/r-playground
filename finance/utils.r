# Plot series, adding lines indicating one and two standard deviations away
plotWithStdDev <- function (series, ...) {
  mean <- mean(series)
  sd <- sd(series)
  plot(series, ...)
  abline(h=mean, col="green")
  abline(h=mean + sd, col="yellow")
  abline(h=mean - sd, col="yellow")
  abline(h=mean + sd*2, col="orange")
  abline(h=mean - sd*2, col="orange")
  abline(h=mean + sd*3, col="red")
  abline(h=mean - sd*3, col="red")
}

# Plot two series and also add a line showing best fit line
scatterPlotWithBestFit <- function(x, y, ...) {
  plot(x, y, ...)
  model <- lm(y ~ x + 1)
  abline(a=coef(m)[1], b=coef(m)[2], col="red")
}