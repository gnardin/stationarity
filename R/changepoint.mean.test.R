#' @title Change Point Mean Stationarity Test
#'
#' @description
#' Change Point mean changes stationarity test.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' @param penalty "None", "SIC", "BIC", "MBIC", AIC", "Hannan-Quinn", "Asymptotic",  "Manual" and "CROPS" penalties.
#' @param method "AMOC", "PELT", "SegNeigh" or "BinSeg"
#' 
#' @return 0: Non-Stationary, 1: Stationary, NA: Unable to test
#' 
#' @importFrom changepoint cpt.mean
#' @importFrom changepoint ncpts
#' 
changepoint.mean.test <- function(data, alpha, penalty, method){
  p <- NULL
  tryCatch(p <- cpt.mean(data, penalty=penalty, method=method))
  
  if(!is.null(p)){
    if(ncpts(p) != 0){
      return(NONSTATIONARY)
    } else {
      return(STATIONARY)
    }
  }
  return(NA)
}
