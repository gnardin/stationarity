#' @title Time Series Data Generator with Break in the Mean
#'
#' @description
#' Generates non-stationary time series data with break in the mean and
#' normal error distribution.
#' 
#' @param N Number of time series
#' @param TS Size of the time series
#' @param deltas Vector with two mean values
#' @param phi Autoregressive parameter
#' @param theta Moving average parameter
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
#' ts.break.mean(5, 5000, c(0, 2), 0.9, 0, c(ERROR_N, 0, 1), c(645,983,653,873,432), 10)
#' 
#' @export "ts.break.mean"
#' 
ts.break.mean <- function(N, TS, deltas, phi, theta, error, seeds, burnin){
  
  stopifnot(!is.null(seeds), N > 0, TS > 1, N <= length(seeds),
      length(deltas) == 2)
  
  firstHalf <- as.integer(TS / 2)
  secondHalf <- TS - firstHalf
  
  ts <- array(0, dim=c(TS, N))
  for(i in 1:N){
    set.seed(seeds[i])
    
    ts1 <- ts.data.generator(firstHalf, 0, deltas[1], 0,
        phi, theta, error, 0, burnin, FALSE)
    
    ts2 <- ts.data.generator(secondHalf, ts1[length(ts1)], deltas[2], 0,
        phi, theta, error, 0, 0, TRUE)
    
    ts[,i] <- c(ts1, ts2)
  }
  
  return(ts)
}
