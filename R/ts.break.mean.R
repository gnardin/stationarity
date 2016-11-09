#' @title Time Series Data Generator with Break in the Mean
#'
#' @description
#' Generates non-stationary time series data with break in the mean and
#' normal error distribution.
#' 
#' @param N Number of time series
#' @param TS Size of the time series
#' @param deltas Vector with two mean values
#' @param phi Vector of autoregressive parameters
#' @param theta Vector of moving average parameters
#' @param mu Normal error mean
#' @param sigma Normal error standard deviation
#' @param seeds Vector of the seeds
#' 
#' @return N time series of size TS
#' 
#' @examples
#' ts.break.mean(5, 5000, c(0, 2), 0.9, 0, 0, 1, c(645,983,653,873,432))
#' 
#' @export "ts.break.mean"
#' 
ts.break.mean <- function(N, TS, deltas, phi, theta, mu, sigma, seeds){
  
  if(is.null(seeds)){
    stop("The seeds vector cannot be NULL.")
  } else if(N <= 0){
    stop("N must be greater than 0.")
  } else if(TS <= 0){
    stop("TS must be greater than 0.")
  } else if(N > length(seeds)){
    stop("The seeds vector size must be greater than or equal to N.")
  } else if(length(deltas) != 2){
    stop("The deltas must be a vector with 2 elements.")
  }
  
  firstHalf <- as.integer(TS / 2)
  secondHalf <- TS - firstHalf
  
  ts <- array(0, dim=c(TS, N))
  for(i in 1:N){
    set.seed(seeds[i])
    ts[,i] <- c(ts.data.generator(firstHalf, deltas[1], 0, phi, theta, mu, sigma, 0),
        ts.data.generator(secondHalf, deltas[2], 0, phi, theta, mu, sigma, 0))
  }
  
  return(ts)
}
