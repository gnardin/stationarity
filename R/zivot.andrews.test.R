#' @title Zivot-Andrews (ZA) Stationarity Test
#'
#' @description
#' Zivot-Andrews stationarity test.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' 
#' @return 0: Non-Stationary, 1: Stationary, NA: Unable to test
#' 
#' @importFrom urca ur.za
#' 
zivot.andrews.test <- function(data, alpha){
  p <- NULL
  tryCatch(p <- ur.za(data),
      error=function(e){return(NA)})
  
  if(!is.null(p)){
    if(!is.null(p@teststat) && !is.na(p@teststat)){
      if(p@teststat <= p@cval[2]){
        return(STATIONARY)
      } else {
        return(NONSTATIONARY)
      }
    }
  }
  return(NA)
}
