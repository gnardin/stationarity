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
#' @param error Type of error and parameters
#'        Normal      - c(ERROR_N, mean, stdv)
#'        Exponential - c(ERROR_E, mean, lambda)
#'        Triangle    - c(ERROR_T, lower, upper, mode)
#' @param seeds Vector of seeds
#' @param burnin Number of samples thrown away at the beginning of time series generation
#' 
#' @return N time series of size TS
#' 
#' @examples
#' ts.stationarity(5, 5000, 1, 0.9, 0, c(ERROR_N,0,1), c(645,983,653,873,432), 10)
#' 
#' @export "ts.stationarity"
#' 
ts.stationarity <- function(N, TS, delta, phi, theta, error, seeds, burnin){
  
  stopifnot(!is.null(seeds), N > 0, TS > 1, N <= length(seeds))
  
  ts <- array(0, dim=c(TS, N))
  for(i in 1:N){
    set.seed(seeds[i])
    ts[,i] <- ts.data.generator(TS, 0, delta, 0, phi, theta, error, 0, burnin, FALSE)
  }
  
  return(ts)
}
