#' @title Time Series Data Generator with Break in the Variance
#'
#' @description
#' Generates non-stationary time series data with break in the variance and
#' normal error distribution.
#' 
#' @param N Number of time series
#' @param TS Size of the time series
#' @param delta Mean
#' @param phi Autoregressive parameter
#' @param theta Moving average parameter
#' @param error Type of error and parameters
#'        Normal      - c(ERROR_N, mean, stdv[1], stdv[2])
#'        Exponential - c(ERROR_E, mean, lambda[1], lambda[2])
#'        Triangle    - c(ERROR_T, lower, upper, mode[1], mode[2])
#' @param seeds Vector of seeds
#' @param burnin Number of samples thrown away at the beginning of time series generation
#' 
#' @return N time series of size TS
#' 
#' @examples
#' ts.break.variance(5, 5000, 0, 0.9, 0, c(ERROR_N, 0, 1, 2), c(645,983,653,873,432), 10)
#' 
#' @export "ts.break.variance"
#' 
ts.break.variance <- function(N, TS, delta, phi, theta, error, seeds, burnin){
  
  stopifnot(!is.null(seeds), N > 0, TS > 1, N <= length(seeds))
  
  errors <- list()
  terror <- error[1]
  ## NORMAL error
  if(terror == ERROR_N){
    stopifnot(length(error) >= 4)
    errors[[1]] <- c(error[1], error[2], error[3])
    errors[[2]] <- c(error[1], error[2], error[4])
    ## EXPONENTIAL error
  } else if(terror == ERROR_E){
    stopifnot(length(error) >= 4)
    errors[[1]] <- c(error[1], error[2], error[3])
    errors[[2]] <- c(error[1], error[2], error[4])
    ## TRIANGLE error
  } else if(terror == ERROR_T){
    stopifnot(length(error) >= 5)
    errors[[1]] <- c(error[1], error[2], error[3], error[4])
    errors[[2]] <- c(error[1], error[2] - error[5],
        error[3] + error[5], error[4])
  }
  
  firstHalf <- as.integer(TS / 2)
  secondHalf <- TS - firstHalf
  
  ts <- array(0, dim=c(TS, N))
  for(i in 1:N){
    set.seed(seeds[i])
    
    ts1 <- ts.data.generator(firstHalf, 0, delta, 0,
        phi, theta, errors[[1]], 0, burnin, FALSE)
    
    ts2 <- ts.data.generator(secondHalf, ts1[length(ts1)], delta, 0,
        phi, theta, errors[[2]], 0, 0, TRUE)
        
    ts[,i] <- c(ts1, ts2)
  }
  
  return(ts)
}
