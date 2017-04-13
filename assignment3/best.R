##check the data

# head(outcome)
# names(outcome)
# class(outcome)
# ncol(outcome)
# nrow(outcome)
# dim(outcome)
# outcome[, 11] <- as.numeric(outcome[, 11])

best <- function(state, ocname) {
  ## Check that outcome is valid
  colIdx <- if(grepl(tolower(ocname), "heart attack")) 11 
  else if(grepl(tolower(ocname), "pneumonia")) 23 
  else if(grepl(tolower(ocname), "heart failure")) 17
  else -1
  
  if(colIdx == -1){
    message("invalid outcome")
    stop();
  }
  
  ## Read outcome data
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(!is.data.frame(outcome) && !nrow(outcome) > 1) {
    message("invalid input data")
    stop()
  }
  
  ## Check that state is valid
  allstates<-unique(outcome$State);
  if(!is.element(state, allstates)) {
    message("invalid state")
    stop()
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate        
  outcome[, colIdx] <- as.numeric(outcome[, colIdx])
  bestavg <- min(outcome[, colIdx][!is.na(outcome[, colIdx]) & outcome$State == state])
  bestHs <- outcome$Hospital.Name[which(outcome[,colIdx]== bestavg & outcome$State == state)]
  bestHs
}
