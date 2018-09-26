#' @title Sequential Testing Algorithm (STA)
#'
#' @description
#' Sequential testing algorithm for stationarity testing.
#'
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' @param window Structural change window size proportion (0 - 1)
#'
#' @return ERS, BG, AT, SC, Total results (0: Non-Stationary, 1: Stationary,
#' NA: Not used test)
#'
#' @export "sta.test"
#'
sta.test <- function(data, alpha, window){

  result <- rep(NA, 5)

  test <- elliot.rothenberg.stock.test(data, alpha)

  if(!is.na(test)){
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
              if(window > 1){
                window <- 1
              } else if(window < 0){
                window <- 0
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
