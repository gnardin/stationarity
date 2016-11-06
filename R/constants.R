#' @title Stationarity Constants
#'
#' @description
#' Define the constants used by the stationarity package.
#' 
.onLoad <- function(libname, pkgname){
  options(warn = -1)
  assign("NONSTATIONARY", 0, envir = .GlobalEnv)
  assign("STATIONARY", 1, envir = .GlobalEnv)
}