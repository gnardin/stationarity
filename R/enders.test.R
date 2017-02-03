#' @title Enders sequential test procedure for unit roots
#'
#' @description
#' Procedure to test for unit roots.
#' 
#' @param data Time series data
#' @param alpha Value of alpha for the statistics test
#' 
#' @return 0: Non-Stationary, 1: Stationary, 2: Stationary around non-zero mean,
#' 3: Stationary around trend
#' 
#' @importFrom stats coef lm
#' 
enders.test <- function(data, alpha){
  
  result <- NA
  
  ## Delta data(t) - data(t - 1)
  datax <- data[-1] - data[-length(data)]
  
  ## Lag 1 data
  data.lag <- data[-length(data)]
  
  ## Adjust data length
  data <- data[-1]
  
  ## Time
  dataLen <- length(data)
  time <- 1:dataLen
  
  GoToStep1 <- TRUE
  GoToStep2 <- FALSE
  GoToStep3 <- FALSE
  GoToStep4 <- FALSE
  GoToStep5 <- FALSE
  
  ##
  ## STEP 1: Test if there is a unit root
  ##
  model.A <- lm(datax ~ time + data.lag)
  model.A.coef <- coef(summary(model.A))
  t.calc.A <- model.A.coef[rownames(model.A.coef) == "data.lag", 3]
  t.critical <- tau.tau.critical.function(dataLen)
  
  ## Decision rule
  if(t.calc.A <= t.critical){
    ## Reject H0 and conclude that there is no unit root
    result <- STATIONARY
  } else {
    ## Fail to reject H0: unit root might be present
    GoToStep2 <- TRUE
  }
  ## END STEP 1
  
  ##
  ## STEP 2: Test if the trend belongs in the estimating equation
  ##
  if(GoToStep2){
    model.B <- lm(datax ~ data.lag)
    SSRA <- sum(model.A$residuals^2)
    SSRB <- sum(model.B$residuals^2)
    
    ## Estimate: r = number of restrictions = 2 (from the hypothesis)
    r <- 2
    
    ## Estimate: T = number of usable observations
    T <- length(datax)
    
    ## Estimate: k = number of estimated parameters in the unrestricted
    ## model = 3 (b0, b1, gamma)
    k <- 3
    
    phi3.calc <- ((SSRB - SSRA) / r) / (SSRA / (T - k))
    phi3.critical <- phi3.critical.function(dataLen)
    
    ## Decision rule
    if(phi3.calc <= phi3.critical){
      ## Fail to reject H0. Might be a unit root and a trend
      GoToStep3 <- TRUE
    } else {
      ## Reject H0: might be a unit root and a trend
      model.C <- lm(datax ~ time)
      model.C.coef <- coef(summary(model.C))
      p.value.C <- model.C.coef[rownames(model.C.coef) == "time", 4]
      
      if(p.value.C <= alpha){
        ## Reject H0 and go back to STEP 1
        p.value.A <- model.A.coef[rownames(model.A.coef) == "data.lag", 4]
        
        if(p.value.A <= alpha){
          ## Reject H0 and conclude that is trend stationary
          result <- STATIONARY_TREND
        } else {
          ## Fail to reject H0 and conclude that it has unit root
          result <- NONSTATIONARY
        }
      } else {
        GoToStep3 <- TRUE
      }
    }
  }
  ## END STEP 2
  
  ##
  ## STEP 3: Test if the trend belongs in the estimating equation
  ##
  if(GoToStep3){
    model.D <- lm(datax ~ data.lag)
    model.D.coef <- coef(summary(model.D))
    t.calc.D <- model.D.coef[rownames(model.D.coef) == "data.lag", 3]
    tau.miu.critical <- tau.miu.critical.function(dataLen)
    
    if(t.calc.D <= tau.miu.critical){
      ## Reject H0 and conclude that there is no unit root
      result <- STATIONARY
    } else{
      ## Fail to reject H0
      GoToStep4 <- TRUE
    }
  }
  ## END STEP 3
  
  ##
  ## STEP 4: Estimate a model without a trend or drift
  ##
  if(GoToStep4){
    model.E <- lm(datax ~ data.lag - 1)
    SSRD <- sum(model.D$residuals^2)
    SSRE <- sum(model.E$residuals^2)
    
    ## Estimate: r = number of restrictions = 2 (from the hypothesis)
    r <- 2
    
    ## Estimate: T = number of usable observations
    T <- length(datax)
    
    ## Estimate: k = number of estimated parameters in the unrestricted
    ## model = 2 (b0, gamma)
    k <- 1
    
    phi1.calc <- ((SSRE - SSRD) / r) / (SSRD / (T - k))
    phi1.critical <- phi1.critical.function(dataLen)
    
    if(phi1.calc <= phi1.critical){
      ## Fail to reject H0: indicates that b0=0.
      GoToStep5 <- TRUE
    } else{
      ## Reject H0 might be a unit root and an intercept
      model.F <- lm(datax ~ 1)
      model.F.coef <- coef(summary(model.F))
      p.value.F <- model.F.coef[rownames(model.F.coef) == "(Intercept)", 4]
      
      if(p.value.F <= alpha){
        ## Reject H0 and go conclude that the model has an intercept term
        p.value.D <- model.D.coef[rownames(model.D.coef) == "data.lag", 4]
        
        if(p.value.D <= alpha){
          ## Reject H0 and conclude that is stationary around a nonzero mean
          result <- STATIONARY_NONZERO_MEAN
        } else {
          ## Fail to reject H0 and conclude that it has unit root and a drift
          result <- NONSTATIONARY
        }
      } else {
        GoToStep5 <- TRUE
      }
    }
  }
  ## END STEP 4
  
  ##
  ## STEP 5
  ##
  if(GoToStep5){
    model.G <- lm(datax ~ data.lag - 1)
    model.G.coef <- coef(summary(model.G))
    t.calc.G <- model.G.coef[rownames(model.G.coef) == "data.lag", 3]
    
    ## tau.critical = -1.95 for any sample size at significance level 0.05
    tau.critical <- -1.95
    
    if(t.calc.G <= tau.critical){
      ## Reject H0 and conclude that there is no unit root
      result <- STATIONARY
    }else{
      ## Fail to reject H0; suggests that the series has a unit root
      result <- NONSTATIONARY
    }
  }
  ## END STEP 5
  
  return(result)
}


#' @title Tau.tau critical value
#' 
#' @description
#' Use the DF table for critical values to calculate the t-critical value
#' based on sample size. Use Table A in Enders (2009) and Dickey-Fuller (1976).
#' 
#' @param size Sample size
#' 
#' @note Includes only values for significance 0.05
#' @note Uses interpolation to calculate the t.critical
#' @note Uses maximum sample size of 1000 to calculate the linear interpolation
#' in the case sample size greater than 500
#' 
tau.tau.critical.function <- function(size){
  t.critical <- ifelse(size <= 25, -3.6,
      ifelse(size <= 50,
          -3.6 + (-3.5 + 3.6) * ((size - 25) / (50 - 25)),
          ifelse(size <= 100,
              -3.5 + (-3.45 + 3.5) * ((size - 50) / (100 - 50)),
              ifelse(size <= 250,
                  -3.45+(-3.43 + 3.45) * ((size - 100) / (250 - 100)),
                  ifelse(size <= 500,
                      -3.43 + (-3.42 + 3.43) * ((size - 250) / (500 - 250)),
                      -3.42 + (-3.41 + 3.42) * ((size - 500) / (1000 - 500)))))))
  return(t.critical)
}


#' @title phi3 critical value
#' 
#' @description
#' Use the DF table for critical values to calculate the phi3 critical value
#' based on sample size. Use Table A in Enders (2009) and Dickey-Fuller (1976).
#' 
#' @param size Sample size
#' 
#' @note Includes only values for significance 0.05
#' @note Uses interpolation to calculate the t.critical
#' @note Uses maximum sample size of 1000 to calculate the linear interpolation
#' in the case sample size greater than 500
#' 
phi3.critical.function <- function(size){
  phi3.critical <- ifelse(size <= 25, 8.65,
      ifelse(size <= 50,
          8.65 + (7.81 - 8.65) * ((size - 25) / (50 - 25)),
          ifelse(size <= 100,
              7.81 + (7.44 - 7.81) * ((size - 50) / (100 - 50)),
              ifelse(size <= 250,
                 7.44 + (7.25 - 7.44) * ((size - 100) / (250 - 100)),
                  ifelse(size <= 500,
                      7.25 + (7.20 - 7.25) * ((size - 250) / (500 - 250)),
                      7.20 + (7.16 - 7.20) * ((size - 500) / (1000 - 500)))))))
  return(phi3.critical)
}

#' @title Tau.miu critical value
#' 
#' @description
#' Use the DF table for critical values to calculate the t-critical value
#' based on sample size. Use Table A in Enders (2009) and Dickey-Fuller (1976).
#' 
#' @param size Sample size
#' 
#' @note Includes only values for significance 0.05
#' @note Uses interpolation to calculate the t.critical
#' @note Uses maximum sample size of 1000 to calculate the linear interpolation
#' in the case sample size greater than 500
#' 
tau.miu.critical.function <- function(size){
  t.critical <- ifelse(size <= 25, -3.0,
      ifelse(size <= 50, 
         -3.0 + (-2.93 + 3.0) * ((size - 25) / (50 - 25)),
          ifelse(size <= 100,
              -2.93 + (-2.89 + 2.93) * ((size - 50) / (100 - 50)),
              ifelse(size <= 250,
                  -2.89 + (-2.88 + 2.89) * ((size - 100) / (250 - 100)),
                  ifelse(size <= 500,
                      -2.88 + (-2.87 + 2.88) * ((size - 250) / (500 - 250)),
                      -2.87 + (-2.86 + 2.87) * ((size - 500) / (1000 - 500)))))))
  return(t.critical)
}

#' @title phi1 critical value
#' 
#' @description
#' Use the DF table for critical values to calculate the t-critical value
#' based on sample size. Use Table A in Enders (2009) and Dickey-Fuller (1976).
#' 
#' @param size Sample size
#' 
#' @note Includes only values for significance 0.05
#' @note Uses interpolation to calculate the t.critical
#' @note Uses maximum sample size of 1000 to calculate the linear interpolation
#' in the case sample size greater than 500
#' 
phi1.critical.function <- function(size){
  phi1.critical <- ifelse(size <= 25, 6.30,
      ifelse(size <= 50,
          6.30 + (5.80 - 6.30) * ((size - 25) / (50 - 25)),
          ifelse(size <= 100,
              5.80 + (5.57 - 5.80) * ((size - 50) / (100 - 50)),
              ifelse(size <= 250,
                  5.57 + (5.45 - 5.57) * ((size - 100) / (250 - 100)),
                  ifelse(size <= 500,
                      5.45 + (5.41 - 5.45) * ((size - 250) / (500 - 250)),
                      5.41 + (5.38 - 5.41) * ((size - 500) / (1000 - 500)))))))
  return(phi1.critical)
}
