# Filtering main 
source("FilteringHelper.R")

Filtering <- function(observation, initialState, filterFun, check.input=T){
  if(check.input) checkInput(observation, initialState)
  filterFun(observation, initialState, check.input=F)
}


