#' @author Amey Sanjay Mahajan
#' 
#' Function returning the required ranked hospital in all the states for the given outcome
#' 
rankall <- function(outcome,num = "best"){
  #Reads the data
  readData <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  rankedHospitals <- data.frame(NA,stringsAsFactors = FALSE)
  allStates <- array()
  
  #Checking the validity for outcomes
  if(outcome != "heart attack" & outcome!= "heart failure" & outcome != "pneumonia"){
    stop("INVALID OUTCOME")
  }
  
  # Returns the required ranked hospital in all states
  for(s in readData[,"State"]){
    allStates<- unique(c(allStates,s))
  }
  for(states in allStates){
    if(!is.na(states)){
      rankedHospitals<- rbind(rankedHospitals,rankhosital(states,outcome,num))
    }
  }
  rankedHospitals<- cbind(rankedHospitals,allStates)
  colnames(rankedHospitals) <- c("hospital","state")
  rankedHospitals <- rankedHospitals[-1,]
  orderedRankedHospitals <- rankedHospitals[order(rankedHospitals$"state"),]
  return(orderedRankedHospitals)
}