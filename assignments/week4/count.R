count <- function(cause = NULL) {
  ## Check that "cause" is non-NULL; else throw error
  if (is.null(cause)) {
    stop("Wrong cause")
  }

    
  ## Check that specific "cause" is allowed; else throw error
  cause <- tolower(cause)
  if ( !(cause %in% c("asphyxiation", "blunt force"
                     , "other", "shooting"
                     , "stabbing", "unknown") ) ){
      
                strop("Wrong cause")
  }
  
  ## Read "homicides.txt" data file
  homicides <- tolower( readLines("homicides.txt") )
   
  
  ## Extract causes of death
  cases <- grep(paste("cause:",cause), homicides)
  
  ## Return integer containing count of homicides for that cause
  length(cases)
}