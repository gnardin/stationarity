#' @title Time Series Data Generator with Variance Trend
#'
#' @description
#' Generates non-stationary time series data with variance trend and
#' normal error distribution.
#' 
#' @param N Number of time series
#' @param TS Size of the time series
#' @param delta Mean
#' @param phi Vector of autoregressive parameters
#' @param theta Vector of moving average parameters
#' @param error Type of error and parameters
#'        Normal      - c(ERROR_N, mean, stdv)
#'        Exponential - c(ERROR_E, mean, lambda)
#'        Triangle    - c(ERROR_T, lower, upper, mode)
#' @param omega Trend on the variance
#' @param seeds Vector of the seeds
#' @param burnin Number of samples thrown away at the beginning of time series generation
#' 
#' @return N time series of size TS
#' 
#' @examples
#' ts.trend.variance(5, 5000, 0, 0.9, 0, c(ERROR_N, 0, 1), 2, c(645,983,653,873,432), 10)
#' 
#' @export "ts.trend.variance"
#' 
ts.trend.variance <- function(N, TS, delta, phi, theta, error, omega, seeds, burnin){
  
  stopifnot(!is.null(seeds), N > 0, TS > 1, N <= length(seeds))
  
  ts <- array(0, dim=c(TS, N))
  for(i in 1:N){
    set.seed(seeds[i])
    ts[,i] <- ts.data.generator(TS, 0, delta, 0, phi, theta, error, omega, burnin)
  }
  
  return(ts)
}
