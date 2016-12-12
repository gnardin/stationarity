#' @title Bootstrap Stationarity Test
#'
#' @description
#' Bootstrap stationarity test.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' 
#' @return 0: Non-Stationary, 1: Stationary, NA: Unable to test
#' 
#' @importFrom costat BootTOS
#' 
bootstrap.test <- function(data, alpha){
  num <- log2(length(data))
  if((num %% as.integer(num)) == 0){
    p <- NULL
    
    tryCatch(p <- BootTOS(data)$p.value,
        error=function(e){return(NA)})
    
    if(!is.na(p) && !is.null(p)){
      if(p <= alpha){
        return(NONSTATIONARY)
      } else {
        return(STATIONARY)
      }
    }
  }
  return(NA)
}
