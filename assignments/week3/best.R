best <- function(state, outcome) {
  ## Read outcome data
  df <- read.csv("/courses/computing_data_analysis/assignments/week3/outcome-of-care-measures.csv"
                      , colClasses = "character")
  
  o <- c(11,17,23)
  names(o) <- c("heart attack","heart failure", "pneumonia")
  
  
  if( ! state %in% df$State)
    stop("Invalid state")
  
  if( ! outcome %in% names(o))
    stop("Invale outcome")
  ## Check that state and outcome are valid
  
  
  df <- df[ df$State == state, c(2, o[outcome]) ]
  df[, 2] <- as.numeric(df[, 2])
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  df[ which.min(df[, 2]), 1]
}