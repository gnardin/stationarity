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
#' @param mu Normal error mean
#' @param sigma Normal error standard deviation
#' @param seeds Vector of the seeds
#' @param burnin Number of samples thrown away at the beginning of time series generation
#' 
#' @return N time series of size TS
#' 
#' @examples
#' ts.trend.autocorrelation(5, 5000, 0, c(-0.9, 0.9), c(0, 0), 0, 1, c(645,983,653,873,432), 10)
#' 
#' @export "ts.trend.autocorrelation"
#' 
ts.trend.autocorrelation <- function(N, TS, delta, phis, thetas, mu, sigma, seeds, burnin){
  
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
  
  fP <- phis[1]
  sP <- phis[2]
  phiL <- as.matrix(fP + (((sP - fP) / (TS - 1)) * 0:(TS - 1)))
  
  fT <- thetas[1]
  sT <- thetas[2]
  thetaL <- as.matrix(fT + (((sT - fT) / (TS - 1)) * 0:(TS - 1)))
  
  
  ts <- array(0, dim=c(TS, N))
  for(i in 1:N){
    set.seed(seeds[i])
    ts[,i] <- ts.data.generator(TS, 0, delta, 0, phiL, thetaL, mu, sigma, 0, burnin)
  }
  
  return(ts)
}
