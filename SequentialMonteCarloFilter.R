# Sequential Monte Carlo Filter

# INPUT
# smcParameters: list
#    numParticles: 
# OUTPUT

SequentialMonteCarloFilter <- function(smcParameters, systemModelFun, observationProbabilityFun,...){
  modelParameters <- list(...)
  source("FilteringHelper.R")
  source("smartSampling.R")
  return(
    function(observation, initialState, check.input = TRUE){
      if(check.input) checkInput(observation=observation,initialState=initialState)
      # sequantial Monte Carlo Filtering
      numParticles <- smcParameters[["numParticles"]]
      numObs <- ncol(observation)
      lenObsVec <- nrow(observation)
      dimOfInitialState <- dim(initialState)
      if(is.null(dimOfInitialState)){
        lenStateVec <- length(initialState)
      } else {
        lenStateVec <- dimOfInitialState[1]
      }
      # state vector(matrix) xtt(filtered state), xt(predicted state)
      xtt <- array(dim=c(lenStateVec, numObs+1, numParticles)) # include initial state
      xt  <- array(dim=c(lenStateVec, numObs, numParticles))
      wt  <- array(dim=c(numObs, numParticles))
      logPredictionProb <- vector(mode="numeric", length=numParticles)
      xtt[,1,1:numParticles] <- initialState
      # non-linear filtering
      for(i_obs in 1:numObs){
        # prediction
#         xt[, i_obs, ] <- systemModelFun(array(xtt[,i_obs,],c(lenStateVec, numParticles)), modelParameters)
        xt[, i_obs, ] <- systemModelFun(xtt[,i_obs,], modelParameters) 
        # probability weights
        wt[i_obs,] <- observationProbabilityFun(observation[,i_obs], xt[,i_obs,], modelParameters)
        sumwt <- sum(wt[i_obs,])
        logPredictionProb[i_obs] <- log(sumwt / numParticles)
        # normalized weights
        wwt <- wt[i_obs,] / sumwt
        # resampling 
        indices_particles <- smartsampling(x=(1:numParticles),size=numParticles,replace=T, prob=wwt)
        xtt[, i_obs+1, ] <- xt[, i_obs, indices_particles]
      }
      cumLogPredictionProb <- sum(logPredictionProb)
      smc.obj <- list(logLik=cumLogPredictionProb, xtt=xtt, xt=xt, wt=wt)
      return(smc.obj)
    }
  )
}





