portfolio_ret <- function (units, weights, df) {
  # Expand units to fit the dimension of the dataframe
  units <- rep.col(units, dim(df)[2])
  
  # Determine daily positions for each of the series, according to their portfolio
  # weight and the amount of the unit portfolio we are holding
  positions <- as.xts(units * weights * df)
  
  # Get profit and loss according to our positions and the daily
  # percent change of each security.
  pnl <- na.omit( lag(positions, 1) * (diff(df) / lag(df, 1)) )
  pnl <- as.xts( apply(pnl, 1, FUN = sum) )
  
  # Calculate gross market value (without margin, this is equal to capital invested)
  # as the absolute value of the dollar amount for each of our positions
  grossMktVal <- na.omit( abs(lag(positions, 1)) )
  grossMktVal <- as.xts( apply(grossMktVal, 1, FUN = sum) )
  
  # Finally, calculate daily returns as our daily profit and loss divided by the
  # daily gross market value.
  ret <- pnl / grossMktVal
  ret <- na.fill( ret, 0 )
}
