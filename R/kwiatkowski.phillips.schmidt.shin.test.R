#' @title Kwiatkowski–Phillips–Schmidt–Shin (KPSS) Stationarity Test
#'
#' @description
#' Kwiatkowski–Phillips–Schmidt–Shin stationarity test.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' 
#' @return 0: Non-Stationary, 1: Stationary, NA: Unable to test
#' 
#' @importFrom tseries kpss.test
#' 
kwiatkowski.phillips.schmidt.shin.test <- function(data, alpha){
  p <- NULL
  tryCatch(p <- kpss.test(data)$p.value,
      error=function(e){return(NA)})
  
  if(!is.na(p) && !is.null(p)){
    if(p <= alpha){
      return(NONSTATIONARY)
    } else {
      return(STATIONARY)
    }
  }
  return(NA)
}
