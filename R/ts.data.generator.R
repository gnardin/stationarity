#' @title Time Series Data Generator
#'
#' @description
#' Generates stationary or non-stationary time series data with mean or
#' variance trend and normal error distribution.
#' 
#' @param N Number of samples
#' @param delta Mean
#' @param tau Trend on the mean
#' @param phi Matrix of autoregressive parameters over time
#' @param theta Matrix of moving average parameters over time
#' @param mu Normal error mean
#' @param sigma Normal error standard deviation
#' @param omega Trend on the variance
#' @param drop Number of initial samples to drop (Default: 0)
#' 
#' @return Time series of size N
#' 
#' @examples
#' ts.data.generator(100, 0, 0, 1, 0, 0, 1, 0)
#' 
#' @importFrom stats rnorm
#' @export "ts.data.generator"
#' 
ts.data.generator <- function(N, delta, tau, phi, theta, mu, sigma, omega, drop=0){
  
  if(N <= 1){
    stop("The N value must be greater than 1.")
  }
  
  ## Test the number of samples to drop
  if(drop >= 0){
    N <- N + drop
  } else {
    stop("The drop value cannot be negative.")
  }
  
  ## Number of autoregressive elements
  if(!is.matrix(phi)){
    p <- length(phi)
    phi <- matrix(rep(phi, each=N), nrow=N, ncol=p)
  } else {
    p <- ncol(phi)
  }
  
  ## Number of moving average elements
  if(!is.matrix(theta)){
    q <- length(theta)
    theta <- matrix(rep(theta, each=N), nrow=N, ncol=q)
  } else {
    q <- ncol(theta)
  }
  
  ## Calculate time series
  Y <- vector()
  epsilon <- vector()
  
  ## Error
  epsilon[1] <- rnorm(1, mu, (sigma + (omega / N)))
  
  ## Dependent variable
  Y[1] <- delta + (tau / N) + epsilon[1]
  
  for(t in 2:N){
    
    ## Error
    epsilon[t] <- rnorm(1, mu, (sigma + ((omega * t) / N)))
    
    ## Dependent variable
    Y[t] <- delta + ((tau * t) / N) + epsilon[t]
    
    if(p > 0){
      ## Incorporate the autoregression
      for(i in 1:min(p, (t - 1))){
        Y[t] = Y[t] + (phi[t, i] * Y[t - i])
      }
    }
    
    if(q > 0){
      ## Incorporate the moving average
      for(j in 1:min(q, (t - 1))){
        Y[t] = Y[t] + (theta[t, j] * epsilon[t - j])
      }
    }
  }
  
  Y <- Y[(drop + 1):N]
  return(Y)
}
