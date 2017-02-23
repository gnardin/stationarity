#' @title Schmidt-Phillips (SP) Stationarity Test
#'
#' @description
#' Schmidt-Phillips stationarity test.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' 
#' @return 0: Non-Stationary, 1: Stationary, NA: Unable to test
#' 
#' @importFrom fUnitRoots urspTest
#' 
schmidt.phillips.test <- function(data, alpha){
  p <- NULL
  tryCatch(p <- urspTest(data, signif=alpha, doplot=FALSE),
      error=function(e){return(NA)})
  
  if(!is.null(p) && !is.na(p)){
    if(!is.na(p@test$test@teststat)){
      if(p@test$test@teststat <= p@test$test@cval){
        return(STATIONARY)
      } else {
        return(NONSTATIONARY)
      }
    }
  }
  return(NA)
}
