#' @title Change Point Variance Stationarity Test
#'
#' @description
#' Change Point variance changes stationarity test.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' @param penalty "None", "SIC", "BIC", "MBIC", AIC", "Hannan-Quinn", "Asymptotic",  "Manual" and "CROPS" penalties.
#' @param method "AMOC", "PELT", "SegNeigh" or "BinSeg"
#' 
#' @return 0: Non-Stationary, 1: Stationary, NA: Unable to test
#' 
#' @importFrom changepoint cpt.var
#' @importFrom changepoint ncpts
#' 
changepoint.variance.test <- function(data, alpha, penalty, method){
  p <- NULL
  tryCatch(p <- cpt.var(data, penalty=penalty, method=method),
      error=function(e){return(NA)})
  
  if(!is.null(p)){
    if(ncpts(p) != 0){
      return(NONSTATIONARY)
    } else {
      return(STATIONARY)
    }
  }
  return(NA)
}
