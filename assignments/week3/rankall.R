rankall <- function(outcome, num="best") {
  ## Read outcome data
  df <- read.csv("/courses/computing_data_analysis/assignments/week3/outcome-of-care-measures.csv"
                 , colClasses = "character")
  
  o <- c(11,17,23)
  names(o) <- c("heart attack","heart failure", "pneumonia")
  
  
  
  if( ! outcome %in% names(o))
    stop("Invalid outcome")
  ## Check that state and outcome are valid
  
 #State is 7  
  # HospitalName, DeathRate
  df <- df[ , c(7, 2, o[outcome]) ]
  df[, 3] <- as.numeric(df[, 3])
  
  df <- df[complete.cases(df),]
  
  df <- df[ order( df[, 1], df[, 3], df[,2], decreasing = FALSE ), ]

  ## Return hospital ranking
  if ( num == "best") num <- 1
  
  a <- sapply( split(df, df$State), FUN=function(d){ 
         if(num == "worst") num <- nrow(d) 
         d[num,2]}) 
  # Stores the state in the name  
  a <- cbind(a, names(a))
  colnames(a) <- c("hospital","state")
  data.frame(a)

  
}