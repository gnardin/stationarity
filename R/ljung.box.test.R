#' @title Ljung-Box (LB) Stationarity Test
#'
#' @description
#' Ljung-Box stationarity test.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' 
#' @return 0: Non-Stationary, 1: Stationary, NA: Unable to test
#' 
#' @importFrom tseries arma
#' @importFrom stats Box.test
#' 
ljung.box.test <- function(data, alpha){
  
  model <- try(arma(data, order=c(1, 0)))
  
  if(class(model) != "try-error"){
    p <- NULL
    tryCatch(p <- Box.test(model$residuals, lag=1, type="Ljung-Box"),
        error=function(e){return(NA)})
    
    if(!is.null(p) && !is.na(p)){
      if(p$p.value <= alpha){
        return(NONSTATIONARY)
      } else {
        return(STATIONARY)
      }
    }
  }
  return(NA)
}
