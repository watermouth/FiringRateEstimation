# input checker for filter
checkInput <- function(observation, initialState){
  error.string <- ""
  if (!is.matrix(observation)) {
    error.string <- paste(error.string, "'observation' must be a matrix!\n", 
                          sep = "")
  }
  if(error.string != ""){
    stop(error.string)
  }
}

