# Filtering main 
source("FilteringHelper.R")

Filtering <- function(observation, initialState, filterFun, check.input=To){
  if(check.input) checkInput(observation, initialState)
  filterFun(observation, initialState, check.input=F)
}


