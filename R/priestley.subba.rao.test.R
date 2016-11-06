#' @title Priestley-Subba Rao (PSR) Stationarity Test
#'
#' @description
#' Priestley-Subba Rao stationarity test.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' 
#' @return 0: Non-Stationary, 1: Stationary, NA: Unable to test
#' 
#' @importFrom fractal stationarity
#' 
priestley.subba.rao.test <- function(data, alpha){
  p <- NULL
  tryCatch(p <- stationarity(data, significance=alpha),
      error=function(e){return(NA)})
  if(!is.null(p)){
    if((attr(p, "pvals")[1]) <= alpha){
      return(NONSTATIONARY)
    } else {
      return(STATIONARY)
    }
  }
  return(NA)
}
