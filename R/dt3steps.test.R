#' @title Decision Tree 3-Steps Stationarity Test
#'
#' @description
#' Decision tree 3-steps stationarity test.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' 
#' @return Test   (KPPS, PSR, ERS, BOOTSTRAP)
#'         Result (0: Non-Stationary, 1: Stationary, NA: Unable to test)
#' 
#' @export "dt3steps.test"
#' 
dt3steps.test <- function(data, alpha){
  steps <- NULL
  
  result <- kwiatkowski.phillips.schmidt.shin.test(data, alpha)
  steps <- rbind(steps, c("KPSS", result))
  if(is.na(result)){
    return(list(NA, steps))
  }
  
  ## Stationary
  if(result == STATIONARY){
    
    result <- priestley.subba.rao.test(data, alpha)
    steps <- rbind(steps, c("PSR", result))
    if(is.na(result)){
      return(list(NA, steps))
    }
    
    ## Stationary
    if(result == STATIONARY){
      result <- bootstrap.test(data, alpha)
      steps <- rbind(steps, c("BOOTSTRAP", result))
    }
    
    ## Non-Stationary
  } else if(result == NONSTATIONARY){
    result <- elliot.rothenberg.stock.test(data, alpha)
    steps <- rbind(steps, c("ERS", result))
  }
  
  steps <- data.frame(as.character(steps[,1]),
      as.integer(steps[, 2]), stringsAsFactors = FALSE)
  names(steps) <- c("Test", "Result")
  
  return(list(result, steps))
}
