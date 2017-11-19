#' @title Kolmogorov-Smirnov stability algorithm
#'
#' @description
#' Procedure to test for time series stability described using the
#' Kolmorov-Smirnov test
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' 
#' @return 0: Non-Stationary, 1: Stationary
#' 
#' @importFrom stats ks.test
#' 
kolmogorov.smirnov.test <- function(data, alpha) {
  center <- round(length(data) / 2)
  x1 <- data[1:center]
  x2 <- data[(center + 1):length(data)]
  
  test <- ks.test(x1, x2)
  
  decision <- ifelse(test$p.value > alpha, STATIONARY, NONSTATIONARY)
  
  return(decision)
}
