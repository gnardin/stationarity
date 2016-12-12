#' @title Augmented Dickey-Fuller (ADF) Stationarity Test
#'
#' @description
#' Augmented Dickey-Fuller stationarity test.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' 
#' @return 0: Non-Stationary, 1: Stationary, NA: Unable to test
#' 
#' @importFrom tseries adf.test
#' 
augmented.dickey.fuller.test <- function(data, alpha){
  p <- NULL
  tryCatch(p <- adf.test(data, alternative="stationary")$p.value,
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
