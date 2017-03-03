#' @title Change Point Stationarity Test
#'
#' @description
#' Change Point changes stationarity test.
#' 
#' @param data Time series data
#' @param type "Student", "Bartlett", "GLR", "Exponential", "GLRAdjusted",
#' "FET", "Mann-Whitney", "Mood", "Lepage", "Kolmogorov-Smirnov",
#' "Cramer-von-Mises"
#' 
#' @return 0: Non-Stationary, 1: Stationary, NA: Unable to test
#' 
#' @importFrom cpm detectChangePoint
#' @importFrom stats lm
#' 
cpm.test <- function(data, type){
  
  types <- c("Student", "Bartlett", "GLR", "Exponential", "GLRAdjusted",
      "FET", "Mann-Whitney", "Mood", "Lepage", "Kolmogorov-Smirnov",
      "Cramer-von-Mises")
  
  index <- which(tolower(types) == tolower(type))
  
  lag <- filter(data, c(0,1), method="conv", sides=1)
  
  model <- try(lm(data ~ -1 + lag))
  
  if(class(model) != "try-error"){
    p <- NULL
    tryCatch(p <- detectChangePoint(model$residuals, cpmType=types[index]),
        error=function(e){return(NA)})
    
    if(!is.null(p) && !is.na(p)){
      if(p$changeDetected){
        return(NONSTATIONARY)
      } else {
        return(STATIONARY)
      }
    }
  }
  return(NA)
}
