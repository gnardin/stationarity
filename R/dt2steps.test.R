#' @title Decision Tree 2-Steps Stationarity Test
#'
#' @description
#' Decision tree 2-steps stationarity test.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' @param mode 1: KPSS-PSR+ERS, 2 - KPSS-BOOTSTRAP+ERS, 3: ERS-PSR,
#' 4: ERS-BOOTSTRAP, 5: KPSS-PSR, 6: KPSS-BOOTSTRAP
#' 
#' @return Test   (KPPS, PSR, ERS, BOOTSTRAP)
#'         Result (0: Non-Stationary, 1: Stationary, NA: Unable to test)
#' 
#' @export "dt2steps.test"
#' 
dt2steps.test <- function(data, alpha, mode){
  steps <- NULL
  
  ## KPSS-PSR+ERS
  if(mode == 1){
    result <- kwiatkowski.phillips.schmidt.shin.test(data, alpha)
    steps <- rbind(steps, c("KPSS", result))
    if(is.na(result)){
      return(list(NA, steps))
    }
    
    ## Stationarity
    if(result == 1){
      result <- priestley.subba.rao.test(data, alpha)
      steps <- rbind(steps, c("PSR", result))
      
      ## Non-Stationary
    } else if(result == 0){
      result <- elliot.rothenberg.stock.test(data, alpha)
      steps <- rbind(steps, c("ERS", result))
    }
    
    ## KPSS-BOOTSTRAP+ERS
  } else if(mode == 2){
    result <- kwiatkowski.phillips.schmidt.shin.test(data, alpha)
    steps <- rbind(steps, c("KPSS", result))
    if(is.na(result)){
      return(list(NA, steps))
    }
    
    ## Stationarity
    if(result == 1){
      result <- bootstrap.test(data, alpha)
      steps <- rbind(steps, c("BOOTSTRAP", result))
      
      ## Non-Stationary
    } else if(result == 0){
      result <- elliot.rothenberg.stock.test(data, alpha)
      steps <- rbind(steps, c("ERS", result))
    }
    
    ## ERS-PSR
  } else if(mode == 3){
    result <- elliot.rothenberg.stock.test(data, alpha)
    steps <- rbind(steps, c("ERS", result))
    if(is.na(result)){
      return(list(NA, steps))
    }
    
    ## Stationarity
    if(result == 1){
      result <- priestley.subba.rao.test(data, alpha)
      steps <- rbind(steps, c("PSR", result))
    }
    
    ## ERS-BOOTSTRAP
  } else if(mode == 4){
    result <- elliot.rothenberg.stock.test(data, alpha)
    steps <- rbind(steps, c("ERS", result))
    if(is.na(result)){
      return(list(NA, steps))
    }
    
    ## Stationarity
    if(result == 1){
      result <- bootstrap.test(data, alpha)
      steps <- rbind(steps, c("BOOTSTRAP", result))
    }
    
    ## KPSS-PSR
  } else if(mode == 5){
    result <- kwiatkowski.phillips.schmidt.shin.test(data, alpha)
    steps <- rbind(steps, c("KPSS", result))
    if(is.na(result)){
      return(list(NA, steps))
    }
    
    ## Stationarity
    if(result == 1){
      result <- priestley.subba.rao.test(data, alpha)
      steps <- rbind(steps, c("PSR", result))
    }
    
    ## KPSS-BOOTSTRAP
  } else if(mode == 6){
    result <- kwiatkowski.phillips.schmidt.shin.test(data, alpha)
    steps <- rbind(steps, c("KPSS", result))
    if(is.na(result)){
      return(list(NA, steps))
    }
    
    ## Stationarity
    if(result == 1){
      result <- bootstrap.test(data, alpha)
      steps <- rbind(steps, c("BOOTSTRAP", result))
    }
  }
  
  steps <- data.frame(as.character(steps[,1]),
      as.integer(steps[,2]), stringsAsFactors=FALSE)
  names(steps) <- c("Test", "Result")
  
  return(list(result, steps))
}
