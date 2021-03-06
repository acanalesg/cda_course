rankhospital <- function(state, outcome, num="better") {
  ## Read outcome data
  df <- read.csv("/courses/computing_data_analysis/assignments/week3/outcome-of-care-measures.csv"
                 , colClasses = "character")
  
  o <- c(11,17,23)
  names(o) <- c("heart attack","heart failure", "pneumonia")
  
  
  if( ! state %in% df$State)
    stop("Invalid state")
  
  if( ! outcome %in% names(o))
    stop("Invalid outcome")
  ## Check that state and outcome are valid
  
  # HospitalName, DeathRate
  df <- df[ df$State == state, c(2, o[outcome]) ]
  df[, 2] <- as.numeric(df[, 2])
  
  df <- df[complete.cases(df),]
  df <- df[ order( df[, 2], df[, 1], decreasing = FALSE ), ]
  
  ## Return hospital ranking
  if ( num == "better") num <- 1
  if ( num == "worst" ) num <- nrow(df)

  df[num,1]
}