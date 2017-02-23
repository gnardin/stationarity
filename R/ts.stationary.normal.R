#' @title Time Series Data Generator with Normal Error Distribution
#'
#' @description
#' Generates non-stationary time series data with normal error distribution.
#' 
#' @param N Number of time series
#' @param TS Size of the time series
#' @param delta Drift constant
#' @param phi Vector of autoregressive parameters
#' @param theta Vector of moving average parameters
#' @param mu Normal error mean
#' @param sigma Normal error standard deviation
#' @param seeds Vector of the seeds
#' @param burnin Number of samples thrown away at the beginning of time series generation
#' 
#' @return N time series of size TS
#' 
#' @examples
#' ts.stationary.normal(5, 5000, 1, 0.9, 0, 0, 1, c(645,983,653,873,432), 10)
#' 
#' @export "ts.stationary.normal"
#' 
ts.stationary.normal <- function(N, TS, delta, phi, theta, mu, sigma, seeds, burnin){
  
  stopifnot(!is.null(seeds), N > 0, TS > 1, N <= length(seeds))
  
  ts <- array(0, dim=c(TS, N))
  for(i in 1:N){
    set.seed(seeds[i])
    ts[,i] <- ts.data.generator(TS, 0, delta, 0, phi, theta, mu, sigma, 0, burnin)
  }
  
  return(ts)
}
