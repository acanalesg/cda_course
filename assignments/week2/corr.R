corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  source("complete.R")
  a <- complete(directory)
 
  #index
  idx <- a[ a$nobs > threshold, "id" ] 
  
  single_corr <- function(id)
  {
    f <- sprintf("%s/%03d.csv", directory, as.numeric(id) )
    d <- read.csv(f)
    cor( d$sulfate, d$nitrate, use = "complete.obs") 
    
  }
  
  sapply(idx,single_corr)
  
  
}