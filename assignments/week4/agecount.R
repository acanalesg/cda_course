agecount <- function(age = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  if (is.null(age)) {
    stop("Wrong age")
  }
  
  ## Read "homicides.txt" data file
  homicides <- tolower( readLines("homicides.txt") )
  
  ## Extract ages of victims; ignore records where no age is
  ## given
  #r <- regexec("([0-9]*) years old", homicides)
  #m <- regmatches(homicides, r)
  
  cases <- grep(paste("", age, "years old"), homicides)
  
  
  ## Return integer containing count of homicides for that age
  length(cases)
}
