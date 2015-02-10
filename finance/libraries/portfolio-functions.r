
# Given a set of price series in dataframe securities, and a set of weights for each
# instrument given in weights, return portfolio consisting of the weighted price
# series
portfolio <- function (securities, weights) {
  
  # If weights have been sized to dimensions
  # of securities, just multiply elements
  if (!is.null(dim(weights))
        && rows(weights) == rows(securities)
        && cols(weights) == cols(securities)) {
    
    reclass( rowSums(securities * weights), securities )
    
  # Otherwise, do a matrix multiplication on
  # securities vs (static) weights.
  } else {
    as.xts( as.matrix(securities) %*% weights )
  }
}

# Given the units of the portfolio to hold (N x 1), the weights for the instruments (N x M), and the
# price series (N x M), return an (N x 1) return series.
portfolio_ret <- function (units, weights, df) {
  # Expand weights series to fit dataframe dimensions, if needed
  if (rows(weights) == 1) {
    weights <- rep.row(weights, rows(df))
  }
  
  # Expand units to fit the dimension of the dataframe
  units <- rep.col(units, dim(df)[2])
  
  # Determine daily positions for each of the series, according to their portfolio
  # weight and the amount of the unit portfolio we are holding
  positions <- as.xts(units * weights * df)
  
  # Get profit and loss according to our positions and the daily
  # percent change of each security.
  pnl <- na.omit( lag(positions, 1) * (diff(df) / lag(df, 1)) )
  pnl <- as.xts( apply(pnl, 1, sum) )
  
  # Calculate gross market value (without margin, this is equal to capital invested)
  # as the absolute value of the dollar amount for each of our positions
  grossMktVal <- na.omit( abs(lag(positions, 1)) )
  grossMktVal <- as.xts( apply(grossMktVal, 1, sum) )
  
  # Finally, calculate daily returns as our daily profit and loss divided by the
  # daily gross market value.
  ret <- pnl / grossMktVal
  ret <- na.fill( ret, 0 )
}
