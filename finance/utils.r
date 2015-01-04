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

# Given a set of price series in dataframe securities, and a set of weights for each
# instrument given in weights, return portfolio consisting of the weighted price
# series
portfolio <- function (securities, weights) {
  as.xts(as.matrix(securities) %*% weights)
}

movingAvg <- function (x, n) {
  rollapply(data = x, width = n, FUN = mean, na.rm = T, fill = NA)
}

movingStd <- function (x, n) {
  rollapply(data = x, width = n, FUN = sd, na.rm = T, fill = NA)
}

maxCorrelation <- function (portfolio, securities) {
  cols <- dim(securities)[2] + 1
  max(cor(data.frame(portfolio, securities))[1,2:cols])
}

correlatedWithConstituent <- function (portfolio, securities) {
  abs(maxCorrelation(sprd, data.frame(Ad(USO), Ad(XLE)))) > 0.60
}
# rep.row and rep.col from 
# http://www.r-bloggers.com/a-quick-way-to-do-row-repeat-and-col-repeat-rep-row-rep-col/
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}