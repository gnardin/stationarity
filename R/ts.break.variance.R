#' @title Time Series Data Generator with Break in the Variance
#'
#' @description
#' Generates non-stationary time series data with break in the variance and
#' normal error distribution.
#' 
#' @param N Number of time series
#' @param TS Size of the time series
#' @param delta Mean
#' @param phi Vector of autoregressive parameters
#' @param theta Vector of moving average parameters
#' @param mu Normal error mean
#' @param sigmas Vector with two error standard deviation values
#' @param seeds Vector of the seeds
#' @param burnin Number of samples thrown away at the beginning of time series generation
#' 
#' @return N time series of size TS
#' 
#' @examples
#' ts.break.variance(5, 5000, 0, 0.9, 0, 0, c(1, 2), c(645,983,653,873,432), 10)
#' 
#' @export "ts.break.variance"
#' 
ts.break.variance <- function(N, TS, delta, phi, theta, mu, sigmas, seeds, burnin){
  
  if(is.null(seeds)){
    stop("The seeds vector cannot be NULL.")
  } else if(N <= 0){
    stop("N must be greater than 0.")
  } else if(TS <= 0){
    stop("TS must be greater than 0.")
  } else if(N > length(seeds)){
    stop("The seeds vector size must be greater than or equal to N.")
  }
  
  firstHalf <- as.integer(TS / 2)
  secondHalf <- TS - firstHalf
  
  ts <- array(0, dim=c(TS, N))
  for(i in 1:N){
    set.seed(seeds[i])
    
    ts1 <- ts.data.generator(firstHalf, 0, delta, 0,
        phi, theta, mu, sigmas[1], 0, burnin)
    
    ts2 <- ts.data.generator(secondHalf, ts1[length(ts1)], delta, 0,
        phi, theta, mu, sigmas[2], 0, 0)
        
    ts[,i] <- c(ts1, ts2)
  }
  
  return(ts)
}
