#' @title Sequential Test Procedure Algorithm Stationarity Test
#'
#' @description
#' Sequential test procedure algorithm stationarity test.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' @param mode 1: ERS, 2: SP
#' @param window Structural change window size
#' 
#' @return ERS|SP, BG, AT, SC, Total results (0: Non-Stationary, 1: Stationary,
#' NA: Not used test)
#' 
#' @export "algo.test"
#' 
algo.test <- function(data, alpha, mode, window){
  
  result <- rep(NA, 5)
  
  # ERS | SP tests Positive Unit-Root
  #                Trend Mean
  #                Break of large time series
  finish <- FALSE
  test <- NA
  if(mode == 1){
    test <- elliot.rothenberg.stock.test(data, alpha)
    
    if(is.na(test)){
      finish <- TRUE
    }
  } else if(mode == 2){
    test <- schmidt.phillips.test(data, alpha)
  }
  
  if(!finish){
    if((!is.na(test)) & (test == NONSTATIONARY)){
      result[1] <- NONSTATIONARY
      result[5] <- NONSTATIONARY
    } else {
      result[1] <- STATIONARY
      
      # BG tests Trend/Break AutoCorrelation
      #          Misclassified Break Mean and Trend Variance
      bg <- breusch.godfrey.test(data, alpha)
      
      if(!is.na(bg)){
        if(bg == NONSTATIONARY){
          result[2] <- NONSTATIONARY
          result[5] <- NONSTATIONARY
        } else {
          result[2] <- STATIONARY
          
          # AT tests Trend/Break Variance
          at <- arch.test(data, alpha)
          
          if(!is.na(at)){
            if(at == NONSTATIONARY){
              result[3] <- NONSTATIONARY
              result[5] <- NONSTATIONARY
            } else {
              result[3] <- STATIONARY
              
              ## Adjust Structure Change test percentage size
              # SC test Break Mean
              if(window > 100){
                window <- 1
              } else if(window < 0){
                window <- 0
              } else {
                window <- window / 100
              }
              
              sc <- structure.change.test(data, alpha, "ME", window)
              
              if(!is.na(sc)){
                if(sc == NONSTATIONARY){
                  result[4] <- NONSTATIONARY
                  result[5] <- NONSTATIONARY
                } else {
                  result[4] <- STATIONARY
                  result[5] <- STATIONARY
                }
              }
            }
          }
        }
      }
    }
  }
  
  return(result)
}
