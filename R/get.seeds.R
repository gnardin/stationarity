#' @title Seed Generator
#'
#' @description
#' Uploads or generates random seeds. The file must have a single seed per row
#' and no column header.
#' 
#' @param N Number of seeds to upload or generate
#' @param filename Name of the file to load the seeds from (Default: NULL)
#' 
#' @return Vector with N seeds
#' 
#' @examples
#' get.seeds(1000)
#' 
#' @importFrom stats runif
#' @importFrom utils read.table
#' @importFrom utils write.table
#' @export "get.seeds"
#' 
get.seeds <- function(N, filename=NULL){
  
  ## Test the Number of seeds to collect or generate
  if(N <= 0){
    stop("Illegal value of N. Value needs to be greater than 0.")
  }
  
  seeds <- NULL
  if(!is.null(filename)){
    if(file.exists(filename)){
      s <- read.table(filename, header=FALSE)
      seeds <- as.vector(s[,1])
      if(length(seeds) >= N){
        seeds <- seeds[1:N]
      } else {
        seeds <- NULL
      }
    }
  }
  
  if(is.null(seeds)){
    seeds <- as.integer(runif(N, min=0, max=(N * 100)))
    
    if(!is.null(filename)){
      write.table(seeds, file=filename, quote=FALSE,
          row.names=FALSE, col.names=FALSE)
    }
  }
  
  return(seeds)
}