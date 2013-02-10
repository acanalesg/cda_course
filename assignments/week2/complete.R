complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  
  single_complete <- function(id)
  {
    f <- sprintf("%s/%03d.csv", directory, as.numeric(id) )
    d <- read.csv(f)
    sum(complete.cases(d) ) 
  
  }
  
  output <- cbind(as.numeric(id), sapply(id, single_complete))
  #col.names(output) <- c("id","nobs")
  output <- data.frame( output )
  names(output) <- c("id","nobs")
  output
  
}
