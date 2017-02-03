#' @title Time Series Data Generator
#'
#' @description
#' Generates stationary or non-stationary time series data with mean or
#' variance trend and normal error distribution.
#' 
#' @param TS Size of the time series
#' @param y0 Y[0] value
#' @param delta Drift constant
#' @param tau Trend on the mean
#' @param phi Matrix of autoregressive parameters over time
#' @param theta Matrix of moving average parameters over time
#' @param mu Normal error mean
#' @param sigma Normal error standard deviation
#' @param omega Trend on the variance
#' @param burnin Number of samples thrown away at the beginning of time series generation
#' 
#' @return Time series of size N
#' 
#' @examples
#' x <- 100
#' ts.data.generator(x, 0, 0, 0, 1, 0, 0, 1, 0, 10)
#' 
#' @importFrom stats rnorm
#' 
#' @export "ts.data.generator"
#' 
ts.data.generator <- function(TS, y0, delta, tau, phi, theta, mu, sigma, omega, burnin){
  
  if(TS <= 1){
    stop("The N value must be greater than 1.")
  }
  
  ## Number of autoregressive elements
  if(!is.matrix(phi)){
    p <- length(phi)
    phis <- matrix(rep(phi, each=TS), nrow=TS, ncol=p)
  } else {
    p <- ncol(phi)
    phis <- phi
  }
  
  ## Number of moving average elements
  if(!is.matrix(theta)){
    q <- length(theta)
    thetas <- matrix(rep(theta, each=TS), nrow=TS, ncol=q)
  } else {
    q <- ncol(theta)
    thetas <- theta
  }
  
  ## Burn-in
  if(burnin > 0){
    
    ## Calculate time series
    Y <- vector()
    epsilon <- vector()
    
    ## Error
    epsilon[1] <- rnorm(1, mu, sigma)
    
    ## Dependent variable
    alpha <- tau / TS
    Y[1] <- y0 + delta + alpha + epsilon[1]
    
    for(t in 2:burnin){
      
      ## Error
      epsilon[t] <- rnorm(1, mu, sigma)
      
      ## Dependent variable
      Y[t] <- delta + (alpha * t) + epsilon[t]
      
      if(p > 0){
        ## Incorporate the autoregression
        for(i in 1:min(p, (t - 1))){
          Y[t] = Y[t] + (phis[1, i] * Y[t - i])
        }
      }
      
      if(q > 0){
        ## Incorporate the moving average
        for(j in 1:min(q, (t - 1))){
          Y[t] = Y[t] + (thetas[1, j] * epsilon[t - j])
        }
      }
    }
    
    y0 = Y[length(Y)]
  }
  
  
  ## Calculate time series
  Y <- vector()
  epsilon <- vector()
  
  ## Error
  epsilon[1] <- rnorm(1, mu, (sigma + (omega / TS)))
  
  ## Dependent variable
  alpha <- tau / TS
  Y[1] <- y0 + delta + (alpha * (1 + burnin)) + epsilon[1]
  
  for(t in 2:TS){
    
    ## Error
    epsilon[t] <- rnorm(1, mu, (sigma + ((omega * t) / TS)))
    
    ## Dependent variable
    Y[t] <- delta + (alpha * (t + burnin)) + epsilon[t]
    
    if(p > 0){
      ## Incorporate the autoregression
      for(i in 1:min(p, (t - 1))){
        Y[t] = Y[t] + (phis[t, i] * Y[t - i])
      }
    }
    
    if(q > 0){
      ## Incorporate the moving average
      for(j in 1:min(q, (t - 1))){
        Y[t] = Y[t] + (thetas[t, j] * epsilon[t - j])
      }
    }
  }
  
  return(Y)
}
