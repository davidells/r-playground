# load packages
library(PerformanceAnalytics)
library("tseries")
library("zoo")
library(pls)

# Start with a very dummy data set
# See http://web.stanford.edu/~wfsharpe/mia/fac/mia_fac2.htm

B <- matrix(c(0.1, 0.3, 0.6,
              0.2, 0.8, 0.0,
              0.0, 0.7, 0.3,
              0.0, 0.0, 1.0),
            4, 3, byrow=TRUE)

R <- matrix(c(16.0, 4.0,
              7.0, 1.0,
              8.0, 6.0,
              22.0, 7.0),
            4, 2, byrow=TRUE)

F <- matrix(c(4, 3,
              7, 2,
              20, 10),
            3, 2, byrow=TRUE)

E <- R - B %*% F

# Ok, let's go get real data and play around with it

quotes <- function(ticker) {
  return(get.hist.quote(instrument=ticker, start="2014-01-01", end="2014-10-31",
                 quote=c("Open", "High", "Low", "Close", "AdjClose", "Volume"),
                 provider="yahoo", origin="1970-01-01",
                 compression="d", retclass="zoo"))
}

SPY.df <- quotes('SPY')
XLE.df <- quotes('XLE')

SPYXLE.df <- merge(SPY.df$AdjClose, XLE.df$AdjClose)
SPYXLE.ret.df <- diff(log(SPYXLE.df))
colnames(SPYXLE.ret.df) <- c("SPY", "XLE");
plot(coredata(SPYXLE.ret.df))

# Run simple linear regression of XLE vs SPY, get XLE's "alpha" and "beta"
SPYXLE.ret.lm <- lm(XLE ~ SPY, data = SPYXLE.ret.df)
abline(SPYXLE.ret.lm)

# Intercept is "alpha" of XLE, slope is the "beta"
# Also, note quantiles of residuals, we'll check that in linear factor model
summary(SPYXLE.ret.lm)

# Ok, model this using linear factor model, one factor, the market
# (ala CAPM with Risk Free return of 0)
R <- SPYXLE.ret.df$XLE
a <- coefficients(SPYXLE.ret.lm)[1]
b <- coefficients(SPYXLE.ret.lm)[2]
F <- SPYXLE.ret.df$SPY

E <- R - (b * F + a)

# See that the residuals agree with linear model
mean(E)
median(E)
quantile(E)

# Introduce another factor: (a proxy for) returns on crude oil
USO.df <- quotes('USO')

ALL.df <- merge(SPY.df$AdjClose, XLE.df$AdjClose, USO.df$AdjClose)
ALL.ret.df <- diff(log(ALL.df))
colnames(ALL.ret.df) <- c("SPY", "XLE", "USO");
pairs(ALL.ret.df)

# Run linear regression of XLE vs SPY and USO
ALL.ret.lm <- lm(XLE ~ ., data = ALL.ret.df)

# Note quantiles of residuals, we'll check that in linear factor model
summary(ALL.ret.lm)

# Ok, model this using linear factor model, two factors
R <- ALL.ret.df$XLE
a <- coefficients(ALL.ret.lm)[1]
B <- coefficients(ALL.ret.lm)[c(2, 3)]
F <- ALL.ret.df[,c(1, 3)]

E <- R - (F %*% B + a)

# See that the residuals agree with linear model
mean(E)
median(E)
quantile(E)

# Let's test our factors against each other, calculating a variance inflation factor
# See http://en.wikipedia.org/wiki/Multicollinearity
SPYUSO.r2 <- summary(lm(SPY ~ USO, data = ALL.ret.df))$r.squared
USOSPY.r2 <- summary(lm(USO ~ SPY, data = ALL.ret.df))$r.squared
SPY.vif = 1 / (1 - SPYUSO.r2)
USO.vif = 1 / (1 - USOSPY.r2)

SPY.vif
USO.vif

# We can also spot check a correlation matrix of factors
cor(ALL.ret.df[,c(1,3)])

# Now let's check out something we figure to be correlated to USO (a commodities ETF)
DBC.df <- quotes('DBC')
ALL.ret.df$DBC <- diff(log(DBC.df$AdjClose))

# Run linear regression of XLE vs SPY, USO, and DBC
ALL.ret.lm <- lm(XLE ~ ., data = ALL.ret.df)

# Note quantiles of residuals, we'll check that in linear factor model
summary(ALL.ret.lm)

# Let's test our factors against each other, calculating a variance inflation factor
# See http://en.wikipedia.org/wiki/Multicollinearity
SPYALL.r2 <- summary(lm(SPY ~ USO + DBC, data = ALL.ret.df))$r.squared
USOALL.r2 <- summary(lm(USO ~ SPY + DBC, data = ALL.ret.df))$r.squared
DBCALL.r2 <- summary(lm(DBC ~ SPY + USO, data = ALL.ret.df))$r.squared
SPY.vif = 1 / (1 - SPYALL.r2)
USO.vif = 1 / (1 - USOALL.r2)
DBC.vif = 1 / (1 - DBCALL.r2)

SPY.vif
USO.vif
DBC.vif

# Hm, USO and DBC are showing up a bit higher than SPY
# Let's check the correlation matrix
F <- ALL.ret.df[,c(1, 3, 4)]
cor(F)
pairs(F)

# DBC and USO are certainly correlated (0.78)

# Alright, well, let's transform these three factors into less related factors.
# Since we know there is correlation in the factors, we ought to get a good result 
# from a PCA.
# See http://en.wikipedia.org/wiki/Principal_component_analysis
ALL.ret.pca <- princomp(~ SPY + USO + DBC, data = ALL.ret.df)
summary(ALL.ret.pca)

# Interesting, we could cover 94% of the variance of original three factors by
# just using the first two components.

F <- ALL.ret.pca$scores
cor(F)

# These factors are definitely uncorrelated

# Let's try a fit against it
ALL.ret.pca.lm <- lm(XLE ~ F, data = ALL.ret.df)
summary(ALL.ret.pca.lm)


# Now wait, what was the original XLE vs SPY fit?
summary(SPYXLE.ret.lm)

# So the R squared value for our PCA model is definitely better
# There seems to be enough error coming from the third component...
# Let's drop it and check things out
ALL.ret.pca.lm <- lm(XLE ~ F[,c(1,2)], data = ALL.ret.df)

# Still a pretty good fit for this data. Neat.
# One last thing, let's check our results against a PCR.
# See http://en.wikipedia.org/wiki/Principal_component_regression and http://machinelearningmastery.com/linear-regression-in-r/
ALL.ret.pcr <- pcr(XLE~., data=ALL.ret.df, validation="CV")
summary(ALL.ret.pcr)

# 94% variance of original factors covered by two factors -- that number agrees
# And we get to see how much variance of XLE is accounted for by the new factors, that's nice.
# Alright, last step, let's fit a model using our PCR scores.
ALL.ret.pcr.lm <- lm(XLE ~ ALL.ret.pcr$scores[,c(1,2)], data=ALL.ret.df)
summary(ALL.ret.pcr.lm)

# Agrees with values of model we got via princomp, good stuff. The scores must be
# coming out the same for the various components. Other interesting thing about PCR
# is that you also get these modified factor exposures, see pcr$coefficients. Is that
# useful though?
ALL.ret.pcr$coefficients
