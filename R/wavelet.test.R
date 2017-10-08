#' @title Wavelet Stationarity Test
#'
#' @description
#' Wavelet stationarity test.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' 
#' @return 0: Non-Stationary, 1: Stationary, NA: Unable to test
#' 
#' @importFrom locits hwtos2
#' 
wavelet.test <- function(data, alpha){
  
  num <- log2(length(data))
  if((num %% as.integer(num)) == 0){
    p <- NULL
    tryCatch(p <- hwtos2(data, alpha=alpha),
        error=function(e){return(NA)})
    
    if(!is.null(p)){
      if(!is.null(p$nreject) && !is.na(p$nreject)){
        if(p$nreject < 1){
          return(STATIONARY)
        } else {
          return(NONSTATIONARY)
        }
      }
    }
  }
  return(NA)
}
