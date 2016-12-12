#' @title Dickey-Fuller (DF) Stationarity Test
#'
#' @description
#' Dickey-Fuller stationarity test.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' 
#' @return 0: Non-Stationary, 1: Stationary, NA: Unable to test
#' 
#' @importFrom tseries adf.test
#' 
dickey.fuller.test <- function(data, alpha){
  p <- NULL
  tryCatch(p <- adf.test(data, alternative="stationary", k=0)$p.value,
      error=function(e){return(NA)})
  
  if(!is.na(p) && !is.null(p)){
    if(p <= alpha){
      return(STATIONARY)
    } else {
      return(NONSTATIONARY)
    }
  }
  return(NA)
}
