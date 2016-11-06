#' @title Arch Test (AT) Stationarity Test
#'
#' @description
#' Arch Test stationarity test.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' 
#' @return 0: Non-Stationary, 1: Stationary, NA: Unable to test
#' 
#' @importFrom tseries arma
#' @importFrom FinTS ArchTest
#' 
arch.test <- function(data, alpha){
  
  model <- try(arma(data, order=c(1, 0)))
  
  if(class(model) != "try-error"){
    p <- NULL
    tryCatch(p <- ArchTest(model$residuals, lags=1, demean=FALSE))
    
    if(!is.null(p)){
      if(p$p.value <= alpha){
        return(NONSTATIONARY)
      } else {
        return(STATIONARY)
      }
    }
  }
  return(NA)
}
