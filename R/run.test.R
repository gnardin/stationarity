#' @title Run Time Series Stationarity Test
#'
#' @description
#' Performs a time series stationarity test.
#' 
#' @param test Name of the time series stationarity test
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' 
#' @return 0: Non-Stationary, 1: Stationary, 2: Stationary around non-zero mean,
#' 3: Stationary around trend, NA: Unable to perform the test
#' 
#' @export "run.test"
#' 
run.test <- function(test, data, alpha){
  
  result <- NA
  
  testName <- tolower(test)
  
  if(testName == "df"){
    result <- dickey.fuller.test(data, alpha)
  } else if(testName == "adf"){
    result <- augmented.dickey.fuller.test(data, alpha)
  } else if(testName == "pp"){
    result <- phillips.perron.test(data, alpha)
  } else if(testName == "sp"){
    result <- schmidt.phillips.test(data, alpha)
  } else if(testName == "za"){
    result <- zivot.andrews.test(data, alpha)
  } else if(testName == "ers"){
    result <- elliot.rothenberg.stock.test(data, alpha)
  } else if(testName == "kpss"){
    result <- kwiatkowski.phillips.schmidt.shin.test(data, alpha)
  } else if(testName == "psr"){
    result <- priestley.subba.rao.test(data, alpha)
  } else if(testName == "wavelet"){
    result <- wavelet.test(data, alpha)
  } else if(testName == "bootstrap"){
    result <- bootstrap.test(data, alpha)
  } else if(testName == "bootwptos"){
    result <- bootwptos.test(data, alpha)
  } else if(substr(testName, 1, 4) == "cptm"){
    method <- NULL
    tryCatch(method <- substr(test, 6, nchar(test)))
    
    result <- changepoint.mean.test(data, alpha, "MBIC", method)
  } else if(substr(testName, 1, 4) == "cptv"){
    method <- NULL
    tryCatch(method <- substr(test, 6, nchar(test)))
    
    result <- changepoint.variance.test(data, alpha, "MBIC", method)
  } else if(testName == "lb"){
    result <- ljung.box.test(data, alpha)
  } else if(testName == "bg"){
    result <- breusch.godfrey.test(data, alpha)
  } else if(testName == "at"){
    result <- arch.test(data, alpha)
  } else if(testName == "bp"){
    result <- breusch.pagan.test(data, alpha)
  } else if(substr(testName, 1, 2) == "sc"){
    
    terms <- unlist(strsplit(test, "-"))
    
    type <- terms[2]
    
    window <- NULL
    if((type == "ME") | (type == "OLSMOSUM")){
      tryCatch(window <- as.double(terms[3]) / 100)
    }
    
    result <- structure.change.test(data, alpha, type, window)
  } else if(substr(testName, 1, 3) == "cpm"){
    type <- NULL
    tryCatch(type <- substr(test, 5, nchar(test)))
    
    result <- cpm.test(data, type)
  } else if(testName == "enders"){
    result <- enders.test(data, alpha)
  } else if(testName == "cdw"){
    result <- cdw.test(data, alpha)
  } else if(substr(testName, 1, 5) == "wwrun"){
    
    terms <- unlist(strsplit(test, "-"))
    
    tryCatch(window <- as.double(terms[2]))
    
    result <- wwrun.test(data, alpha, window)
  } else if(testName == "k+s"){
    result <- kolmogorov.smirnov.test(data, alpha)
  }
  
  return(result)
}