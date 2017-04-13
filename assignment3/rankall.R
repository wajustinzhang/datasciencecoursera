rankall <- function(ocname, num = "best") {
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
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  ret = data.frame(Hospital.Name=NA, State=NA)
  
  for(state in allstates){
    outcome[, colIdx] <- as.numeric(outcome[, colIdx])
    orderIndex <- order(outcome[, colIdx][!is.na(outcome[, colIdx]) & outcome$State == state], outcome$Hospital.Name[!is.na(outcome[, colIdx]) & outcome$State == state])
    rank<-outcome[!is.na(outcome[, colIdx]) & outcome$State == state,][orderIndex,][c(2, 7)]
    
    # add a "rank" column
    length <-length(rank[,1])
    retrank = if(num == "best") rank[1,] 
              else if(num == "worst") rank[length,]
              else if(is.numeric(num) && num <=length) rank[num,] 
              else data.frame(Hospital.Name=NA, State=state)
    
    ret<-rbind(ret, retrank)
  }
  
  ret<-ret[!is.na(ret$State),]
  ret<-ret[order(ret$State),]
  ret
}