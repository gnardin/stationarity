#' @title Time Series Data Generator
#'
#' @description
#' Generates stationary or non-stationary time series data with mean or
#' variance trend and different error distributions.
#' 
#' @param TS Size of the time series
#' @param y0 Y[0] value
#' @param delta Drift constant
#' @param tau Trend on the mean
#' @param phi Matrix of autoregressive parameters over time
#' @param theta Matrix of moving average parameters over time
#' @param error Type of error and parameters
#'        Normal      - c(ERROR_N, mean, stdv)
#'        Exponential - c(ERROR_E, mean, lambda)
#'        Triangle    - c(ERROR_T, lower, upper, mode)
#' @param omega Trend on the variance
#' @param burnin Number of samples thrown away at the beginning of time series generation
#' @param continue Continues a previous time series (Supports only AR(1) and NO MOVING AVERAGE)
#' 
#' @return Time series of size N
#' 
#' @examples
#' x <- 100
#' ts.data.generator(x, 0, 0, 0, 1, 0, c(ERROR_N,0,1), 0, 10, TRUE)
#' 
#' @importFrom stats rnorm
#' @importFrom smoothmest rdoublex
#' @importFrom triangle rtriangle
#' 
#' @export "ts.data.generator"
#' 
ts.data.generator <- function(TS, y0, delta, tau, phi, theta, error, omega, burnin, continue){
  
  stopifnot(TS > 1, error[1] %in% c(ERROR_N, ERROR_E, ERROR_T))
  
  terror <- error[1]
  ## NORMAL error
  if(terror == ERROR_N){
    stopifnot(length(error) >= 3)
    mu <- error[2]
    sigma <- error[3]
  ## EXPONENTIAL error
  } else if(terror == ERROR_E){
    stopifnot(length(error) >= 3)
    mu <- error[2]
    lambda <- error[3]
  ## TRIANGLE error
  } else if(terror == ERROR_T){
    stopifnot(length(error) >= 4)
    lower <- error[2]
    upper <- error[3]
    mode <- error[4]
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
    if(terror == ERROR_N){
      epsilon[1] <- rnorm(1, mu, sigma)
    } else if(terror == ERROR_E){
      epsilon[1] <- rdoublex(1, mu, lambda)
    } else if(terror == ERROR_T){
      epsilon[1] <- rtriangle(1, lower, upper, mode)
    }
    
    ## Dependent variable
    alpha <- tau / TS
    
    if(continue){
      Y[1] <- delta + alpha + epsilon[1]
      
      if(p > 0){
        ## Incorporate the autoregression
        for(i in 1:min(p, length(y0))){
          Y[1] = Y[1] + (phis[1, i] * y0[length(y0) - i + 1])
        }
      }
    } else {
      Y[1] <- y0 + delta + alpha + epsilon[1]
    }
    
    for(t in 2:burnin){
      
      ## Error
      if(terror == ERROR_N){
        epsilon[t] <- rnorm(1, mu, sigma)
      } else if(terror == ERROR_E){
        epsilon[t] <- rdoublex(1, mu, lambda)
      } else if(terror == ERROR_T){
        epsilon[t] <- rtriangle(1, lower, upper, mode)
      }
      
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
  if(terror == ERROR_N){
    epsilon[1] <- rnorm(1, mu, (sigma + (omega / TS)))
  } else if(terror == ERROR_E){
    epsilon[1] <- rdoublex(1, mu, (lambda + (omega / TS)))
  } else if(terror == ERROR_T){
    epsilon[1] <- rtriangle(1, lower - (omega / TS),
        upper + (omega / TS), mode)
  }
  
  ## Dependent variable
  alpha <- tau / TS
  
  if(continue){
    Y[1] <- delta + (alpha * (1 + burnin)) + epsilon[1]
    
    if(p > 0){
      ## Incorporate the autoregression
      for(i in 1:min(p, length(y0))){
        Y[1] = Y[1] + (phis[1, i] * y0[length(y0) - i + 1])
      }
    }
  } else {
    Y[1] <- y0 + delta + (alpha * (1 + burnin)) + epsilon[1]
  }
  
  for(t in 2:TS){
    
    ## Error
    if(terror == ERROR_N){
      epsilon[t] <- rnorm(1, mu, (sigma + ((omega * t) / TS)))
    } else if(terror == ERROR_E){
      epsilon[t] <- rdoublex(1, mu, (lambda + ((omega * t) / TS)))
    } else if(terror == ERROR_T){
      epsilon[t] <- rtriangle(1, lower - ((omega * t) / TS),
          upper + ((omega * t) / TS), mode)
    }
    
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
