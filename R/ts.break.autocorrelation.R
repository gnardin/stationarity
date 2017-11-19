#' @title Time Series Data Generator with Break in the Autocorrelation
#'
#' @description
#' Generates non-stationary time series data with break in the autocorrelation
#' and normal error distribution.
#' 
#' @param N Number of time series
#' @param TS Size of the time series
#' @param delta Mean
#' @param phis Vector with two autoregressive values
#' @param thetas Vector with two moving average values
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
#' ts.break.autocorrelation(5, 5000, 0, c(0.45, 0.9), c(0, 0), c(ERROR_N, 0, 1),
#' c(645,983,653,873,432), 10)
#' 
#' @export "ts.break.autocorrelation"
#' 
ts.break.autocorrelation<- function(N, TS, delta, phis, thetas, error, seeds, burnin){
  
  stopifnot(!is.null(seeds), N > 0, TS > 1, N <= length(seeds),
      length(phis) == 2, length(thetas) == 2)
  
  fHalf <- as.integer(TS / 2)
  sHalf <- TS - fHalf
  
  ts <- array(0, dim=c(TS, N))
  for(i in 1:N){
    set.seed(seeds[i])
    
    ts1 <- ts.data.generator(fHalf, 0, delta, 0,
        phis[1], thetas[1], error, 0, burnin)
    
    ts2 <- ts.data.generator(sHalf, ts1[length(ts1)], delta, 0,
        phis[2], thetas[2], error, 0, 0)
    
    ts[,i] <- c(ts1, ts2)
  }
  
  return(ts)
}
