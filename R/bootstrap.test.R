#' @title Bootstrap Wavelet Packet Tests
#'
#' @description
#' Bootstrap wavelet packet tests.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' 
#' @return 0: Non-Stationary, 1: Stationary, NA: Unable to test
#' 
#' @import wavethresh
#' @importFrom costat BootTOS
#' 
bootstrap.test <- function(data, alpha){
  
  num <- log2(length(data))
  if((num %% as.integer(num)) == 0){
    p <- NULL
    
    tryCatch(p <- BootWPTOS(data, levs=1, indices=1)$p.value,
        error=function(e){return(NA)})
    
    if(!is.null(p) && !is.na(p)){
      if(p <= alpha){
        return(NONSTATIONARY)
      } else {
        return(STATIONARY)
      }
    }
  }
  return(NA)
}
