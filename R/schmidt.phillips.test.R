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
#' @importFrom urca ur.sp
#' 
schmidt.phillips.test <- function(data, alpha){
  p <- NULL
  tryCatch(p <- ur.sp(data, signif=alpha),
      error=function(e){return(NA)})
  
  if(!is.null(p)){
    if(!is.null(p@teststat) && !is.na(p@teststat)){
      if(p@teststat <= p@cval){
        return(STATIONARY)
      } else {
        return(NONSTATIONARY)
      }
    }
  }
  return(NA)
}
