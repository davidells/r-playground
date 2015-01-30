library('urca')

johansen <- function (x, ecdet="const", ...) {
  
  p <- 0
  if (ecdet == "none") {
    p <- -1
  } else if (ecdet == "const") {
    p <- 0
  } else if (ecdet == c("const", "trend")) {
    p <- 1
  }
  
  result = list()
  for (type in c("trace", "eigen")) {
    
    nDimensions <- dim(x)[2]
    cvals <- data.frame(matrix(0, nDimensions, 4))
    colnames(cvals) <- c(paste(type, "stat"), "90%", "95%", "99%")
    rowNames <- c()
    
    jo <- ca.jo(x, type=type, ecdet=ecdet, ...)
    
    for (i in seq(nDimensions)) {
      index <- nDimensions - (i - 1)
      cvals[i,] <- c(
        jo@teststat[index],
        johansenCriticalValues(index, p, type=type))
      rowNames[i] <- paste("r <=", i-1)
    }
    rownames(cvals) <- rowNames
    
    if (type == "eigen") {
      result <- c(result, list("eigen"=cvals))
    } else {
      result <- c(result, list("trace"=cvals))
    }
  }
  result <- c(result, list("eig"=jo@lambda, "evec"=jo@Vorg, "evecNorm"=jo@V))
  
  bestEvecNorm <- result$evecNorm[1:dim(result$evecNorm)[1]-1,1]
  result <- c(result, list("bestWeights"=bestEvecNorm))
}


# Ported from http://www.mathworks.com/matlabcentral/fileexchange/45093-time-frequency-generalized-phase-synchrony-for-eeg-signal-analysis/content/TF%20Generalized%20Phase%20Synchrony/c_sja.m
# PURPOSE: find critical values for Johansen maximum eigenvalue statistic
# ------------------------------------------------------------
# USAGE:  johansenEigenCriticalValues(n,p)
# where:    n = dimension of the VAR system
#           p = order of time polynomial in the null-hypothesis
#                 p = -1, no deterministic part
#                 p =  0, for constant term
#                 p =  1, for constant plus time-trend
#                 p >  1  returns no critical values
# ------------------------------------------------------------
# RETURNS: a (3x1) vector of percentiles for the maximum eigenvalue
#          statistic for: [90% 95% 99%]               
# ------------------------------------------------------------
# NOTES: for n > 12, the function returns a (3x1) vector of zeros.
#        The values returned by the function were generated using
#        a method described in MacKinnon (1996), using his FORTRAN
#        program johdist.f                        
# ------------------------------------------------------------
# SEE ALSO: johansen()
# ------------------------------------------------------------
# References: MacKinnon, Haug, Michelis (1996) 'Numerical distribution 
# functions of likelihood ratio tests for cointegration', 
# Queen's University Institute for Economic Research Discussion paper.
# -------------------------------------------------------

# written by:
# James P. LeSage, Dept of Economics
# University of Toledo
# 2801 W. Bancroft St,
# Toledo, OH 43606
# jlesage@spatial-econometrics.com


johansenCriticalValues <- function(n, p, type="eigen") {
  e0 <- matrix( 
    c(2.9762, 4.1296, 6.9406,
      9.4748, 11.2246, 15.0923,
      15.7175, 17.7961, 22.2519,
      21.8370, 24.1592, 29.0609,
      27.9160, 30.4428, 35.7359,
      33.9271, 36.6301, 42.2333,
      39.9085, 42.7679, 48.6606,
      45.8930, 48.8795, 55.0335,
      51.8528, 54.9629, 61.3449,
      57.7954, 61.0404, 67.6415,
      63.7248, 67.0756, 73.8856,
      69.6513, 73.0946, 80.0937),
    12, 3, byrow=TRUE)
  
  e1 <- matrix(
    c(2.7055,  3.8415,  6.6349, 
      12.2971,  14.2639,  18.5200,  
      18.8928,  21.1314,  25.8650,  
      25.1236,  27.5858,  32.7172,  
      31.2379,  33.8777,  39.3693,  
      37.2786,  40.0763,  45.8662,  
      43.2947,  46.2299,  52.3069,  
      49.2855,  52.3622,  58.6634,  
      55.2412,  58.4332,  64.9960,  
      61.2041,  64.5040,  71.2525,  
      67.1307,  70.5392,  77.4877,
      73.0563,  76.5734,  83.7105),
    12, 3, byrow=TRUE)
  
  e2 <- matrix(
    c(2.7055,   3.8415,   6.6349,        
      15.0006,  17.1481,  21.7465,        
      21.8731,  24.2522,  29.2631,
      28.2398,  30.8151,  36.1930,
      34.4202,  37.1646,  42.8612,
      40.5244,  43.4183,  49.4095,
      46.5583,  49.5875,  55.8171,
      52.5858,  55.7302,  62.1741,
      58.5316,  61.8051,  68.5030,
      64.5292,  67.9040,  74.7434,
      70.4630,  73.9355,  81.0678,
      76.4081,  79.9878,  87.2395),
    12, 3, byrow=TRUE)
  
  t0 <- matrix(
    c(2.9762,   4.1296,   6.9406,
       10.4741,  12.3212,  16.3640,
       21.7781,  24.2761,  29.5147,
       37.0339,  40.1749,  46.5716,
       56.2839,  60.0627,  67.6367,
       79.5329,  83.9383,  92.7136,
       106.7351, 111.7797, 121.7375,
       137.9954, 143.6691, 154.7977,
       173.2292, 179.5199, 191.8122,
       212.4721, 219.4051, 232.8291,
       255.6732, 263.2603, 277.9962,
       302.9054, 311.1288, 326.9716),
    12, 3, byrow=TRUE)
  
  t1 <- matrix(
    c(2.7055,   3.8415,   6.6349,
        13.4294,  15.4943,  19.9349,
        27.0669,  29.7961,  35.4628,
        44.4929,  47.8545,  54.6815,
        65.8202,  69.8189,  77.8202,
        91.1090,  95.7542, 104.9637,
        120.3673, 125.6185, 135.9825,
        153.6341, 159.5290, 171.0905,
        190.8714, 197.3772, 210.0366,
        232.1030, 239.2468, 253.2526,
        277.3740, 285.1402, 300.2821,
        326.5354, 334.9795, 351.2150),
    12, 3, byrow=TRUE)
  
  t2 <- matrix(
      c(2.7055,   3.8415,   6.6349,
         16.1619,  18.3985,  23.1485,
         32.0645,  35.0116,  41.0815,
         51.6492,  55.2459,  62.5202,
         75.1027,  79.3422,  87.7748,
         102.4674, 107.3429, 116.9829,
         133.7852, 139.2780, 150.0778,
         169.0618, 175.1584, 187.1891,
         208.3582, 215.1268, 228.2226,
         251.6293, 259.0267, 273.3838,
         298.8836, 306.8988, 322.4264,
         350.1125, 358.7190, 375.3203),
      12, 3, byrow=TRUE)
  
  if (type == "eigen") {
    jcp0 <- e0
    jcp1 <- e1
    jcp2 <- e2
  } else if (type == "trace") {
    jcp0 <- t0
    jcp1 <- t1
    jcp2 <- t2
  }
  
  if ((p > 1) | (p < -1)) {
    matrix(0, 1, 3)
  }
  else if ((n > 12) | (n < 1)) {
    matrix(0, 1, 3)
  }
  else if (p == -1) {
    jcp0[n,]
  }
  else if (p == 0) {
    jcp1[n,]
  }
  else if (p == 1) {
    jcp2[n,]
  }
}
