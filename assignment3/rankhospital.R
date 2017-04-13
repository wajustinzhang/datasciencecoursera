rankhospital <- function(state, ocname, num = "best") {
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
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  outcome[, colIdx] <- as.numeric(outcome[, colIdx])
  #orderIndex <- order(outcome[, colIdx][!is.na(outcome[, colIdx]) & outcome$State == state])
  orderIndex <- order(outcome[, colIdx][!is.na(outcome[, colIdx]) & outcome$State == state], outcome$Hospital.Name[!is.na(outcome[, colIdx]) & outcome$State == state])
  
  rank<-outcome[!is.na(outcome[, colIdx]) & outcome$State == state,][orderIndex,][c(2,colIdx)]
  
  # add a "rank" column
  length <-length(rank[,1])
  rank["Rank"]<-1:length
  
  ret = if(num == "best") rank[1,] 
        else if(num == "worst") rank[length,]
        else if(is.numeric(num) && num <=length) rank[num,] 
        else NA
  ret
}
