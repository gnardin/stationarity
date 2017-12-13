#' @title Chli and De Wilde stability algorithm
#'
#' @description
#' Procedure to test for time series stability described in
#' Chli, M. and de Wilde, P. (2009). Convergence and knowledge processing in
#' multi-agent systems. Springer.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' 
#' @return 0: Non-Stationary, 1: Stationary
#' 
#' @importFrom stats t.test
#' @importFrom stats var.test
#' 
cdw.test <- function(data, alpha) {
  center <- as.integer(length(data) / 2)
  x1 <- data[1:center]
  x2 <- data[(center + 1):length(data)]
  
  mean.test <- t.test(x1, x2)
  variance.test <- var.test(x1, x2)
  
  decision <- ifelse(mean.test$p.value > alpha,
      ifelse(variance.test$p.value > alpha, STATIONARY, NONSTATIONARY),
      NONSTATIONARY)
  
  return(decision)
}

