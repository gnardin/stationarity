#' @title Phillips-Perron (PP) Stationarity Test
#'
#' @description
#' Phillips-Perron stationarity test.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' 
#' @return 0: Non-Stationary, 1: Stationary, NA: Unable to test
#' 
#' @importFrom tseries pp.test
#' 
phillips.perron.test <- function(data, alpha){
  p <- NULL
  tryCatch(p <- pp.test(data, alternative="stationary")$p.value,
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
