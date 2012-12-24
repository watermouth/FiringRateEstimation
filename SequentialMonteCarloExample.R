rm(list=ls())
source("Filtering.R")
source("SampleVirtualFiringRates.R")
source("SequentialMonteCarloFilter.R")

# virtual firing rate sampling
timeSteps <- 200
hiddenParam <- list(systemVariance=0.05^2, observationVariance=0.07^2)
sysFun <- function(n){ sampleLocalGaussian(n=n,init_x=0.7,sd=sqrt(hiddenParam$systemVariance)) }
obsFun <- function(x){ gaussianObservation(x=x,sd=sqrt(hiddenParam$observationVariance)) }
sampledData <- SampleVirtualFiringRates(n=timeSteps,systemFun=sysFun, observFun=obsFun
                                        , seed=88,allowNegative=F)
observation <- matrix(sampledData$y, nrow=1)

# filter
ssmParam <- list(systemVariance=0.1^2, observationVariance=0.1^2) # ad hoc parameters
# ssmParam <- list(systemVariance=0.055^2, observationVariance=0.011^2) # estimated paramters
# ssmParam <- list(systemVariance=0.05^2, observationVariance=0.1^2) # hidden paramters

modelParams <- list(sysSd=sqrt(ssmParam$systemVariance), obsSd=sqrt(ssmParam$observationVariance))
# system model
smcSystem_simpleLocalGaussian <- function(stateMatrix, modelParameters){
  if(!is.matrix(stateMatrix)) stateMatrix <- t(stateMatrix)
  numParticles <- ncol(stateMatrix)
  matrix(rnorm(n=numParticles,mean=stateMatrix[1,],sd=modelParameters[["sysSd"]]), ncol=numParticles)
}
# observation model probabilty
smcObservationProb_gaussian <- function(observation, stateMatrix, modelParameters){
  if(!is.matrix(stateMatrix)) stateMatrix <- t(stateMatrix)
  matrix(dnorm(x=observation, mean=c(stateMatrix), sd=modelParameters[["obsSd"]]), ncol=ncol(stateMatrix))
}
# sequential monte carlo parameters
smcParameters <- list(numParticles=500)

# estimation target using marginal likelihood maximization 
estimationTarget <- NULL
# if you want to estimate parameters, set those initial values to the following list
estimationTarget <- list(sysSd=modelParams$sysSd, obsSd=modelParams$obsSd)
filterArgs <- list(smcParameters=smcParameters,systemModelFun=smcSystem_simpleLocalGaussian,observationProbabilityFun=smcObservationProb_gaussian
                   ,sysSd=modelParams$sysSd, obsSd=modelParams$obsSd)
filteringObject <- Filtering(observation=observation,initialState=c(0.5),filterGenerator=SequentialMonteCarloFilter,check.input=T
                             , optimOptions=list(method="L-BFGS-B",
                                                 lower=0.02,
                                                 upper=1,
                                                 control=list(maxit=500, ndeps=c(1e-2,1e-3)))
                             , filterArgs=filterArgs, estimationTarget=estimationTarget)
filter.obj <- filteringObject$filter.obj
fit.obj <- filteringObject$fit.obj

# exclude xtt's first element which is initial value of time dimension
filteredState <- apply(X=filter.obj$xtt[1,-1,],MARGIN=1,FUN=mean)
filteredStateVar <- apply(X=filter.obj$xtt[1,-1,], MARGIN=1, FUN=var)
filteredStateP1Sigma <- filteredState + sqrt(filteredStateVar)
filteredStateM1Sigma <- filteredState - sqrt(filteredStateVar)

# plot
plot(observation[1,] )
points(filteredState, pch=20, col="blue")
points(filteredStateP1Sigma, pch=20, col="green")
points(filteredStateM1Sigma, pch=20, col="green")

# print estimated params
print("initial params")
print(c(ssmParam,recursive=T))
print("hidden params")
print(c(hiddenParam,recursive=T))
print("estimated params")
print(fit.obj$par)
print("rate of finding observations in 1 sigma")
print(length((which(filteredStateM1Sigma < observation & filteredStateP1Sigma > observation))) / timeSteps)
print("variance of deviation( std(observation - filteredState) )")
print(sqrt(var(filteredState - observation[1,])))
