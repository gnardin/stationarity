#' @title Elliot-Rothenberg-Stock (ERS) Stationarity Test
#'
#' @description
#' Elliot-Rothenberg-Stock stationarity test.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' 
#' @return 0: Non-Stationary, 1: Stationary, NA: Unable to test
#' 
#' @importFrom fUnitRoots urersTest
#' 
elliot.rothenberg.stock.test <- function(data, alpha){
  p <- NULL
  tryCatch(p <- urersTest(data, type="DF-GLS", model="constant", doplot=FALSE),
      error=function(e){return(NA)})
  
  if(!is.null(p) && !is.na(p)){
    if(!is.na(p@test$test@teststat)){
      if(p@test$test@teststat <= p@test$test@cval[2]){
        return(STATIONARY)
      } else {
        return(NONSTATIONARY)
      }
    }
  }
  return(NA)
}
