#' @title Breusch-Pagan (BP) Stationarity Test
#'
#' @description
#' Breusch-Pagan stationarity test.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' 
#' @return 0: Non-Stationary, 1: Stationary, NA: Unable to test
#' 
#' @importFrom timeSeries filter
#' @importFrom lmtest bptest
#' 
breusch.pagan.test <- function(data, alpha){
  
  lag <- filter(data, c(0,1), method="conv", sides=1)
  
  p <- NULL
  tryCatch(p <- bgtest(data ~ 1 + lag, order=1, type="F", fill=NA),
      error=function(e){return(NA)})
  
  if(!is.null(p) && !is.na(p)){
    if(p$p.value <= alpha){
      return(NONSTATIONARY)
    } else {
      return(STATIONARY)
    }
  }
  return(NA)
}
