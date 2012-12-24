# Filtering main 
source("FilteringHelper.R")

Filtering <- function(observation, initialState, filterGenerator, filterArgs, estimationTarget=list(), optimOptions=NULL,check.input=T){
  if(check.input) checkInput(observation, initialState)
  if(is.null(estimationTarget)) estimationTarget <- list()
  if(length(estimationTarget)==0){
    filterFun <- do.call(what=filterGenerator,args=filterArgs)
    return(list(filter.obj=filterFun(observation, initialState, check.input=F)))
  } else {
    objective <- function(par){
      for(i_par in names(par)){
        filterArgs[[i_par]] <<- array(par[[i_par]], dim(estimationTarget[[i_par]]))
        # you can trace various trial values of par
        print(i_par)
        print(filterArgs[[i_par]])
      }
      filterFun <- do.call(what=filterGenerator,args=filterArgs)
      filter.obj <- filterFun(observation, initialState, check.input=T)
      filter.obj[["logLik"]]
    }
    if(is.null(optimOptions)){
      fit.obj <- optim(par=estimationTarget,fn=objective, control=c(list(fnscale=-1)))
    } else {
      fit.obj <- optim(par=estimationTarget,fn=objective,method=optimOptions[["method"]]
                       ,lower=optimOptions[["lower"]]
                       ,upper=optimOptions[["upper"]]
                       ,control=c(list(fnscale=-1), optimOptions[["control"]]))
    }
    filterFun <- do.call(what=filterGenerator,args=filterArgs)
    return(list(filter.obj=filterFun(observation, initialState, check.input=T), fit.obj=fit.obj))
  }
}


