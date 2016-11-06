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
#' @param mu Normal error mean
#' @param sigma Normal error standard deviation
#' @param seeds Vector of the seeds
#' @param drop Number of initial samples to drop (Default: 100)
#' 
#' @return N time series of size TS
#' 
#' @examples
#' ts.break.autocorrelation(5, 5000, 0, c(0.45, 0.9), c(0, 0), 0, 1, c(645,983,653,873,432))
#' 
#' @export "ts.break.autocorrelation"
#' 
ts.break.autocorrelation<- function(N, TS, delta, phis, thetas, mu, sigma, seeds, drop=100){
  
  if(is.null(seeds)){
    stop("The seeds vector cannot be NULL.")
  } else if(N <= 0){
    stop("N must be greater than 0.")
  } else if(TS <= 0){
    stop("TS must be greater than 0.")
  } else if(N > length(seeds)){
    stop("The seeds vector size must be greater than or equal to N.")
  } else if(length(phis) != 2){
    stop("The phis must be a vector with 2 elements.")
  } else if(length(thetas) != 2){
    stop("The thetas must be a vector with 2 elements.")
  }
  
  fHalf <- as.integer(TS / 2)
  sHalf <- TS - fHalf
  
  ts <- array(0, dim=c(TS, N))
  for(i in 1:N){
    set.seed(seeds[i])
    ts[,i] <- c(ts.data.generator(fHalf, delta, 0, phis[1], thetas[1], mu, sigma, 0, drop=drop),
        ts.data.generator(sHalf, delta, 0, phis[2], thetas[2], mu, sigma, 0, drop=drop))
  }
  
  return(ts)
}
