#' @author Amey Sanjay Mahajan
#' 
#' Function returning the required ranked hospital in the given state for the given outcome
#' 
rankhosital <- function(state,outcome,num = "best"){
  #Reads the data
  wholeData <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  arr <- array()
  dataForOutcome <- data.frame()
  orderedDataForState <- data.frame()
  index <- 1
  wantedIndex <- 0
  
  #Checking the validity for states and outcomes
  for(i in 1:length((wholeData[,"State"]))){
    if(state != (wholeData[,"State"][i]) & i == length((wholeData[,"State"]))){
      stop("INVALID STATE")
    }
    else if(outcome != "heart attack" & outcome!= "heart failure" & outcome != "pneumonia"){
      stop("INVALID OUTCOME")
    }
    else{
      if(state == (wholeData[,"State"][i])){
        break
      }
    }
  }
  # Returns the required ranked hospital in the particular state
  for(j in 1:length((wholeData[,"State"]))){
    if(state == wholeData[,"State"][j]){
      arr <- c(arr,j)
    }
  }
  if(outcome == "heart failure"){
    for(j in 2:length(arr)){
      wantedIndex <- arr[j]
      if(wholeData[,17][wantedIndex]!="Not Available"){
        dataForOutcome <- rbind(dataForOutcome,wholeData[wantedIndex,])
      }
    }
    orderedDataForState <- dataForOutcome[order(as.numeric(dataForOutcome$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"),dataForOutcome$"Hospital.Name"),]
  }
  if(outcome == "heart attack"){
    for(j in 2:length(arr)){
      wantedIndex <- arr[j]
      if(wholeData[,11][wantedIndex]!="Not Available"){
        dataForOutcome <- rbind(dataForOutcome,wholeData[wantedIndex,])
      }
    }
    orderedDataForState <- dataForOutcome[order(as.numeric(dataForOutcome$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"),dataForOutcome$"Hospital.Name"),]
  }
  if(outcome == "pneumonia"){
    for(j in 2:length(arr)){
      wantedIndex <- arr[j]
      if(wholeData[,23][wantedIndex]!="Not Available"){
        dataForOutcome <- rbind(dataForOutcome,wholeData[wantedIndex,])
      }
    }
    orderedDataForState <- dataForOutcome[order(as.numeric(dataForOutcome$"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"),dataForOutcome$"Hospital.Name"),]
  }
  bestResult <- orderedDataForState[1,2]
  worstResult <- orderedDataForState[length(orderedDataForState[,1]),2]
  Result <- orderedDataForState[num,2]
  
  if(num == "best"){
    return(bestResult)
  }
  else if(num == "worst"){
    return(worstResult)
  }
  else if(as.numeric(num) > length(orderedDataForState)){
    NA
  }
  else{
    return(Result)
  }
}
