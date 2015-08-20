#' @author Amey Sanjay Mahajan
#' 
#' Function returning the hospital giving the best results with respect to outcomes in all the states for the given outcome
#' 
best <- function(state,outcome){
  wholeData <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  arr <- array()
  minimum <- 0
  index <- 1
  wantedIndex <- 0
  for(i in 1:length(wholeData[,"State"])){
    if(state != wholeData[,"State"][i] & i == length(wholeData[,"State"])){
      stop("INVALID STATE")
    }
    else if(outcome != "heart attack" & outcome!= "heart failure" & outcome != "pneumonia"){
      stop("INVALID OUTCOME")
    }
    else{
      if(state == wholeData[,"State"][i]){
        break
      }
    }
  }
  for(j in 1:length(wholeData[,"State"])){
    if(state == wholeData[,"State"][j]){
      arr <- c(arr,j)
    }
  }
  if(outcome == "heart attack"){
    minimum <- wholeData[,11][1]
    for(j in 2:length(arr)){
      wantedIndex <- arr[j]
      if(wholeData[,11][wantedIndex] != "Not Available" & minimum != "Not Available"){
        if(as.numeric(wholeData[,11][wantedIndex]) < as.numeric(minimum)){
          minimum <- wholeData[,11][wantedIndex]
          index <- wantedIndex
        }
      }
    }
  }
  if(outcome == "heart failure"){
    minimum <- wholeData[,17][1]
    for(j in 2:length(arr)){
      wantedIndex <- arr[j]
      if(wholeData[,17][wantedIndex]!="Not Available" & minimum !="Not Available"){
        if(as.numeric(wholeData[,17][wantedIndex]) < as.numeric(minimum)){
          minimum <- wholeData[,17][wantedIndex]
          index <- wantedIndex
        }
      }
    }
  }
  if(outcome == "pneumonia"){
    minimum <- wholeData[,23][1]
    for(j in 2:length(arr)){
      wantedIndex <- arr[j]
      if(wholeData[,23][wantedIndex] != "Not Available" & minimum != "Not Available"){
        if(as.numeric(wholeData[,23][wantedIndex]) < as.numeric(minimum)){
          minimum <- wholeData[,23][wantedIndex]
          index <- wantedIndex
        }
      }
    }
  }
  wholeData[,2][index]
}
