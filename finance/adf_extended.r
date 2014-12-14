#
# Extended from adf.test in tseries package (authored by A. Trapletti),
# this version adds a "model" parameter, that allows the user to choose which
# ADF model should be used for the test.
#
adf <- function (
  x, alternative = c("stationary", "explosive"), 
  model = 1, k = trunc((length(x) - 1)^(1/3))) {
  
  if (NCOL(x) > 1) 
    stop("x is not a vector or univariate time series")
  if (any(is.na(x))) 
    stop("NAs in x")
  if (k < 0) 
    stop("k negative")
  if (model < 1 || model > 3)
    stop("model must be one of (1, 2, 3)")
  
  alternative <- match.arg(alternative)
  DNAME <- deparse(substitute(x))
  
  k <- k + 1
  x <- as.vector(x, mode="double")
  y <- diff(x)
  n <- length(y)
  z <- embed(y, k)
  yt <- z[, 1]
  xt1 <- x[k:n]
  tt <- k:n
  
  if (k > 1) {
    yt1 <- z[, 2:k]
    if (model == 1)
      res <- lm(yt ~ 0 + xt1 + yt1)
    else if (model == 2)
      res <- lm(yt ~ 1 + xt1 + yt1)
    else if (model == 3)
      res <- lm(yt ~ 1 + xt1 + tt + yt1)
  } 
  else {
    if (model == 1)
      res <- lm(yt ~ 0 + xt1)
    else if (model == 2)
      res <- lm(yt ~ 1 + xt1)
    else if (model == 3)
      res <- lm(yt ~ 1 + xt1 + tt)
  }
  
  res.sum <- summary(res)
  STAT <- res.sum$coefficients[2, 1]/res.sum$coefficients[2, 2]
  
  if (model == 1)
    table <- cbind(c(-2.66, -2.62, -2.60, -2.58, -2.58, -2.58), 
                   c(-2.26, -2.25, -2.24, -2.23, -2.23, -2.23),
                   c(-1.95, -1.95, -1.95, -1.95, -1.95, -1.95),
                   c(-1.60, -1.61, -1.61, -1.61, -1.61, -1.61), 
                   c(0.92, 0.91, 0.90, 0.89, 0.89, 0.89), 
                   c(1.33, 1.31, 1.29, 1.29, 1.28, 1.28),
                   c(1.70, 1.66, 1.64, 1.63, 1.62, 1.62),
                   c(2.16, 2.08, 2.03, 2.01, 2.00, 2.00))
  
  else if (model == 2)
    table <- cbind(c(-3.75, -3.58, -3.51, -3.46, -3.44, -3.43),
                   c(-3.33, -3.22, -3.17, -3.14, -3.13, -3.12),
                   c(-3.00, -2.93, -2.89, -2.88, -2.87, -2.86),
                   c(-2.62, -2.60, -2.58, -2.57, -2.57, -2.57), 
                   c(-0.37, -0.40, -0.42, -0.42, -0.43, -0.44),
                   c(0.00, -0.03, -0.05, -0.06, -0.07, -0.07),
                   c(0.34, 0.29, 0.26, 0.24, 0.24, 0.23),
                   c(0.72, 0.66, 0.63, 0.62, 0.61, 0.60))
  
  else if (model == 3) 
    table <- cbind(c(-4.38, -4.15, -4.04, -3.99, -3.98, -3.96), 
                   c(-3.95, -3.8, -3.73, -3.69, -3.68, -3.66), 
                   c(-3.6, -3.5, -3.45, -3.43, -3.42, -3.41), 
                   c(-3.24, -3.18, -3.15, -3.13, -3.13, -3.12), 
                   c(-1.14, -1.19, -1.22, -1.23, -1.24, -1.25), 
                   c(-0.8, -0.87, -0.9, -0.92, -0.93, -0.94), 
                   c(-0.5, -0.58, -0.62, -0.64, -0.65, -0.66), 
                   c(-0.15, -0.24, -0.28, -0.31, -0.32, -0.33))

  tablen <- dim(table)[2]
  tableT <- c(25, 50, 100, 250, 500, 1e+05)
  tablep <- c(0.01, 0.025, 0.05, 0.1, 0.9, 0.95, 0.975, 0.99)
  tableipl <- numeric(tablen)
  
  rownames(table) <- tableT
  colnames(table) <- tablep
  
  for (i in (1:tablen))
    tableipl[i] <- approx(tableT, table[, i], n, rule = 2)$y
  
  interpol <- approx(tableipl, tablep, STAT, rule = 2)$y
  if (is.na(approx(tableipl, tablep, STAT, rule = 1)$y)) {
    if (interpol == min(tablep)) 
      warning("p-value smaller than printed p-value")
    else 
      warning("p-value greater than printed p-value")
  }
  
  if (alternative == "stationary") 
    PVAL <- interpol
  else if (alternative == "explosive") 
    PVAL <- 1 - interpol
  else 
    stop("irregular alternative")
  
  PARAMETER <- k - 1
  METHOD <- "Augmented Dickey-Fuller Test"
  names(STAT) <- "Dickey-Fuller"
  names(PARAMETER) <- "Lag order"
  
  structure(
    list(
      statistic = STAT, parameter = PARAMETER, 
      alternative = alternative, model = model,
      p.value = PVAL, method = METHOD, 
      critvals = table, fit = res, data.name = DNAME), 
    class = "htest")
}

# Cointegrating Augmented Dickey Fuller test. Takes
# two series, and runs the adf test on the two spreads between
# them using hedge ratios from linear regressions using each
# as the independent variable
cadf <- function(x, y, ...) {
  lmX <- lm(x ~ y)
  lmY <- lm(y ~ x)
  adfX <- adf(x - coef(lmX)[2]*y, ...)
  adfY <- adf(y - coef(lmY)[2]*x, ...)
  if (adfX$statistic < adfY$statistic) {
    adfX$data.model <- lmX
    adfX
  } else {
    adfY$data.model <- lmY
    adfY
  }
}