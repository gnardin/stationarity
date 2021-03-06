#' @title Time Series Data Generator with Autocorrelation Trend
#'
#' @description
#' Generates non-stationary time series data with autocorrelation trend and
#' normal error distribution.
#' 
#' @param N Number of time series
#' @param TS Size of the time series
#' @param delta Mean
#' @param phis Vector with the minimum and maximum autoregressive values
#' @param thetas Vector with the minimum and maximum moving average values
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
#' ts.trend.autocorrelation(5, 5000, 0, c(-0.9, 0.9), c(0, 0), c(ERROR_N, 0, 1),
#' c(645,983,653,873,432), 10)
#' 
#' @export "ts.trend.autocorrelation"
#' 
ts.trend.autocorrelation <- function(N, TS, delta, phis, thetas, error, seeds, burnin){
  
  stopifnot(!is.null(seeds), N > 0, TS > 1, N <= length(seeds),
      length(phis) == 2, length(thetas) == 2)
  
  fP <- phis[1]
  sP <- phis[2]
  phiL <- as.matrix(fP + (((sP - fP) / (TS - 1)) * 0:(TS - 1)))
  
  fT <- thetas[1]
  sT <- thetas[2]
  thetaL <- as.matrix(fT + (((sT - fT) / (TS - 1)) * 0:(TS - 1)))
  
  ts <- array(0, dim=c(TS, N))
  for(i in 1:N){
    set.seed(seeds[i])
    ts[,i] <- ts.data.generator(TS, 0, delta, 0, phiL, thetaL, error, 0, burnin, FALSE)
  }
  
  return(ts)
}
