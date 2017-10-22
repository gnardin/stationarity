#' @title Grazzini stationarity test algorithm
#'
#' @description
#' Procedure to test for time series stationarity described in
#' Granzzini, J. (2012) Analysis of the emergent properties: Stationarity
#' and ergodicity, JASSS, 15:(2).
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' @param window Size of the window to perform the test
#' 
#' @return 0: Non-Stationary, 1: Stationary
#' 
#' @importFrom stats qnorm
#' 
wwrun.test <- function(data, alpha, window) {
  rr <- list()
  
  tsLen <- length(data)
  if (tsLen >= (2 * window)) {
    sequence <- seq(1, tsLen, window)
    for(i in 1:length(sequence)) {
      rr[[i]] <- data[seq(sequence[i], sequence[i] + window - 1)]
    }
  } else if (tsLen >= window) {
    rr[[1]] <- data[seq(1, window)]
  } else {
    rr[[1]] <- data
  }
  
  sMeans <- vector()
  for (i in 1:length(rr)) {
    sMeans[i] <- mean(rr[[i]])
  }
  
  overallMean <- mean(data)
  
  runList <- vector()
  runs <- 0
  state <- 0
  above <- 0
  below <- 0
  for (sVal in sMeans) {
    if (sVal < overallMean) {
      below <- below + 1
      runList <- c(runList, -1)
    }
    
    if ((sVal < overallMean) & (state != -1)) {
      state <- -1
      runs <- runs + 1
    }
    
    if (sVal > overallMean) {
      above <- above + 1
      runList <- c(runList, 1)
    }
    
    if ((sVal > overallMean) & (state != 1)) {
      state <- 1
      runs <- runs + 1
    }
  }
  
  beta <- above * below^-1
  mu <- (2 * above * (1 + beta)^-1) + 1
  
  s <- ((mu - 1) * (mu - 2)) * (length(sMeans) - 1)^-1
  sigma <- sqrt(s)
  
  p <- (runs - mu) * sigma^-1
  
  z <- qnorm(1 - alpha)
  r <- ifelse(((p < -z) | (p > z)), NONSTATIONARY, STATIONARY)
  
  return(r)
}