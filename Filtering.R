# Filtering main 
source("FilteringHelper.R")

Filtering <- function(observation, initialState, filterGenerator, filterArgs, estimationTarget=list(), check.input=T){
  if(check.input) checkInput(observation, initialState)
  if(is.null(estimationTarget)) estimationTarget <- list()
  if(length(estimationTarget)==0){
    filterFun <- do.call(what=filterGenerator,args=filterArgs)
    return(list(filter.obj=filterFun(observation, initialState, check.input=F)))
  } else {
    objective <- function(par){
      for(i_par in names(par)){
        filterArgs[[i_par]] <<- array(par[[i_par]], dim(estimationTarget[[i_par]]))
      }
      filterFun <- do.call(what=filterGenerator,args=filterArgs)
      filter.obj <- filterFun(observation, initialState, check.input=T)
      filter.obj[["logLik"]]
    }
    fit.obj <- optim(par=estimationTarget,fn=objective,control=list(fnscale=-1))
    filterFun <- do.call(what=filterGenerator,args=filterArgs)
    return(list(filter.obj=filterFun(observation, initialState, check.input=T), fit.obj=fit.obj))
  }
}


