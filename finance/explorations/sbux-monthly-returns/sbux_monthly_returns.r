# Most of the code here came from Eric Zivot's code in 
# the coursera "Computational Finance" course

# Read in a csv of two columns, date and price. But, while reading in, 
# set the row labels to the dates, and import as a one column data frame
# (one column of "Adj.Close") and row-labeled not by number, but by "Date".
sbux.df <- read.csv(file="sbux_monthly_returns.csv", header=TRUE, 
                    stringsAsFactors=FALSE, row.names="Date")

# Note: an alternative to the above is to maintain the file with no date header:
#               Adj.Close
# 2012-12-01    20.32
# ...
#
# And you'll get a data frame of one column, labeled by date

# Get "raw" vector of prices from data frame, using [row, column] indexing syntax
sbux.prices <- sbux.df[, "Adj.Close"]

# Calculate the vector of simple returns for this time series of prices
n <- length(sbux.prices)
sbux.ret <- (sbux.prices[2:n] - sbux.prices[0:(n-1)]) / sbux.prices[0:(n-1)]

# Let's put labels on the vector (we don't use rownames on the left because
# sbux.ret is a vector, not a data frame)
names(sbux.ret) <- rownames(sbux.df)[2:n]

# Calculate the vector of log returns for this time series of prices
sbux.ret.cc <- log(1 + sbux.ret)

# Take a look at simple returns and continuous returns side by side.
# cbind stands for column bind, there is also an rbind for row bind.
head(cbind(sbux.ret, sbux.ret.cc))



# Plot simple and cc returns...

# create plot with simple returns
plot(sbux.ret, type="l", col="blue", lwd=2, 
     ylab="Return", main="Monthly Returns on SBUX")

# add horizontal line at zero
abline(h=0)     

# add line for cc returns
lines(sbux.ret.cc, col="red", lwd=2)

# add a legend
legend(x="bottomright", legend=c("Simple", "CC"), 
       lty=1, lwd=2, col=c("blue","red"))




# Calculate growth of $1 (by tracking the cumulative product of gross returns)...
# Remember, gross returns are 1 + simple (i.e. simple of 0.06 -> gross of 1.06)
sbux.fv <- cumprod(1 + sbux.ret)

plot(sbux.fv, type="l", col="blue", lwd=2, ylab="Dollars", 
     main="FV of $1 invested in SBUX")


# Plot histogram of returns, grouping them using round function
hist(round(sbux.ret.cc, 2), ylim=c(0, 80), 
     main="SBUX CC Returns", xlab="cc return")