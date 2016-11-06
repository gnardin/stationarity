#' @title Sequential Test Procedure Algorithm Stationarity Test
#'
#' @description
#' Sequential test procedure algorithm stationarity test.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' @param mode 1: ALGO-ERS, 2: ALGO-ERS+SC50, 3: ALGO-ERS+SC60,
#' 4: ALGO-ERS+SC70, 5: ALGO-ERS+SC80, 6: ALGO-ERS+SC90
#' 
#' @return 0: Non-Stationary, 1: Stationary, NA: Unable to test
#' 
#' @export "algo.test"
#' 
algo.test <- function(data, alpha, mode){
  
  result <- NA
  
  # ERS tests Positive Unit-Root
  #           Trend Mean
  #           Break of large time series
  test <- NA
  test <- elliot.rothenberg.stock.test(data, alpha)
  
  if(!is.na(test)){
    if(test == NONSTATIONARY){
      result <- NONSTATIONARY
    } else {
      # BG tests Trend/Break AutoCorrelation
      #          Misclassified Break Mean and Trend Variance
      bg <- breusch.godfrey.test(data, alpha)
      
      if(!is.na(bg)){
        if(bg == NONSTATIONARY){
          result <- NONSTATIONARY
        } else {
          # AT tests Trend/Break Variance
          at <- arch.test(data, alpha)
          
          if(!is.na(at)){
            if(at == NONSTATIONARY){
              result <- NONSTATIONARY
            } else {
              
              if(mode != 1){
                # SC test Break Mean
                window <- ((mode - 2) / 10) + 0.5
                sc <- structure.change.test(data, alpha, "ME", window)
                
                if(!is.na(sc)){
                  if(sc == NONSTATIONARY){
                    result <- NONSTATIONARY
                  } else {
                    result <- STATIONARY
                  }
                }
              } else {
                result <- STATIONARY
              }
            }
          }
        }
      }
    }
  }
  return(result)
}
