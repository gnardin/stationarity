#' @title Time Series Data Generator with Mean Trend
#'
#' @description
#' Generates non-stationary time series data with mean trend and
#' normal error distribution.
#' 
#' @param N Number of time series
#' @param TS Size of the time series
#' @param delta Mean
#' @param tau Trend on the mean
#' @param phi Vector of autoregressive parameters
#' @param theta Vector of moving average parameters
#' @param error Type of error and parameters
#'        Normal      - c(ERROR_N, mean, stdv)
#'        Exponential - c(ERROR_E, mean, lambda)
#'        Triangle    - c(ERROR_T, lower, upper, mode)
#' @param seeds Vector of the seeds
#' @param burnin Number of samples thrown away at the beginning of time series generation
#' 
#' @return N time series of size TS
#' 
#' @examples
#' ts.trend.mean(5, 5000, 0, 1, 0.9, 0, c(ERROR_N, 0, 1), c(645,983,653,873,432), 10)
#' 
#' @export "ts.trend.mean"
#' 
ts.trend.mean <- function(N, TS, delta, tau, phi, theta, error, seeds, burnin){
  
  stopifnot(!is.null(seeds), N > 0, TS > 1, N <= length(seeds))
  
  ts <- array(0, dim=c(TS, N))
  for(i in 1:N){
    set.seed(seeds[i])
    ts[,i] <- ts.data.generator(TS, 0, delta, tau, phi, theta, error, 0, burnin)
  }
  
  return(ts)
}
