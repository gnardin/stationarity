#' @title Structure Change (SC) Stationarity Test
#'
#' @description
#' Strcuture Change stationarity test.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' @param type Type of fluctuation process "ME", "OLS-CUSUM", "OLS-MOSUM"
#' @param window Percent of time series size to use as window
#' 
#' @return 0: Non-Stationary, 1: Stationary, NA: Unable to test
#' 
#' @importFrom timeSeries filter
#' @importFrom strucchange sctest
#' @importFrom strucchange efp
#' 
structure.change.test <- function(data, alpha, type, window){
  
  if((is.null(window)) || is.na(window) || (length(window) == 0)){
    window <- 0.01
  } else {
    if(window < 0){
      window <- 0
    } else if(window > 1){
      window <- 1
    }
  }
  
  lag <- filter(data, c(0, 1), method= "conv", sides=1)
  
  p <- NULL
  if(type == "ME"){
    p <- try(efp(data ~ lag, h=window, type="ME"))
  } else if(type == "OLSMOSUM"){
    p <- try(efp(data ~ lag, h=window, type="OLS-MOSUM"))
  } else if(type == "OLSCUSUM"){
    p <- try(efp(data ~ lag, type="OLS-CUSUM"))
  }
  
  if(class(p) != "try-error"){
    scValue <- sctest(p)$p.value
    if(!is.na(scValue)){
      if(scValue <= alpha){
        return(NONSTATIONARY)
      } else {
        return(STATIONARY)
      }
    }
  }
  return(NA)
}
