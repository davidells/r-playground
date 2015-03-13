# ------------------------------
# Define bollinger band trading strategy, which holds when the price
# is less than 1 std dev below the mean, is short when price is more than
# 1 std dev above the mean, and otherwise is out.
# ------------------------------
# TODO: Create bollinger band that increases size based greater
#       std dev from the mean (ex. long 2 when more than 2 std dev
#       below the mean, etc)
bollinger.band <- function (zScore) {
  entryZscore <- 1
  exitZscore <- 0
  
  longsEntry <- zScore < -entryZscore
  longsExit <- zScore >= -exitZscore
  shortsEntry <- zScore > entryZscore
  shortsExit <- zScore <= exitZscore
  
  unitsLong <- rep(NA, rows(zScore))
  unitsShort <- rep(NA, rows(zScore))
  
  unitsLong[1] <- 0
  unitsLong[longsEntry] <- 1
  unitsLong[longsExit] <- 0
  unitsLong <- na.locf(unitsLong)
  
  unitsShort[1] <- 0
  unitsShort[shortsEntry] <- -1
  unitsShort[shortsExit] <- 0
  unitsShort <- na.locf(unitsShort)
  
  units <- unitsLong + unitsShort
}