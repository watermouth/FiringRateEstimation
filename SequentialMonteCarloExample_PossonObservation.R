rm(list=ls())
source("Filtering.R")
source("SampleVirtualFiringRates.R")
source("SequentialMonteCarloFilter.R")

# virtual firing rate sampling
timeSteps <- 200
hiddenParam <- list(systemVariance=0.5^2) # too big value will make impossible to estimate parameters
sysFun <- function(n){ sampleLocalGaussian(n=n,init_x=10,sd=sqrt(hiddenParam$systemVariance)) }
obsFun <- function(x){ poissonObservation(x=x) }
sampledData <- SampleVirtualFiringRates(n=timeSteps,systemFun=sysFun, observFun=obsFun
                                        , seed=88,allowNegative=F)
observation <- matrix(sampledData$y, nrow=1)
systemState <- sampledData$x

# filter
ssmParam <- list(systemVariance=0.1^2) # ad hoc parameters
# ssmParam <- list(systemVariance=0.055^2) # estimated paramters
# ssmParam <- list(systemVariance=0.05^2) # hidden paramters

modelParams <- list(sysSd=sqrt(ssmParam$systemVariance))
# system model
smcSystem_simpleLocalGaussian <- function(stateMatrix, modelParameters){
  if(!is.matrix(stateMatrix)) stateMatrix <- t(stateMatrix)
  numParticles <- ncol(stateMatrix)
  matrix(rnorm(n=numParticles,mean=stateMatrix[1,],sd=modelParameters[["sysSd"]]), ncol=numParticles)
}
# observation model probabilty
smcObservationProb_poisson <- function(observation, stateMatrix, modelParameters){
  if(!is.matrix(stateMatrix)) stateMatrix <- t(stateMatrix)
  matrix(dpois(x=observation, lambda=c(stateMatrix)), ncol=ncol(stateMatrix))
}
# sequential monte carlo parameters
numParticles <- 100
smcParameters <- list(numParticles=numParticles)

# estimation target using marginal likelihood maximization 
estimationTarget <- NULL
# if you want to estimate parameters, set those initial values to the following list
estimationTarget <- list(sysSd=modelParams$sysSd)
filterArgs <- list(smcParameters=smcParameters,systemModelFun=smcSystem_simpleLocalGaussian
                   ,observationProbabilityFun=smcObservationProb_poisson
                   ,sysSd=modelParams$sysSd)
filteringObject <- Filtering(observation=observation,initialState=matrix(rpois(n=numParticles,lambda=10),ncol=numParticles),filterGenerator=SequentialMonteCarloFilter,check.input=T
                             , optimOptions=list(method="L-BFGS-B",
                                                 lower=0.1,
                                                 upper=0.5,
                                                 control=list(maxit=500, ndeps=c(1e-2)))
                             , filterArgs=filterArgs, estimationTarget=estimationTarget)
filter.obj <- filteringObject$filter.obj
fit.obj <- filteringObject$fit.obj

# exclude xtt's first element which is initial value of time dimension
filteredState <- apply(X=filter.obj$xtt[1,-1,],MARGIN=1,FUN=mean)
filteredStateVar <- apply(X=filter.obj$xtt[1,-1,], MARGIN=1, FUN=var)
filteredStateP1Sigma <- filteredState + sqrt(filteredStateVar)
filteredStateM1Sigma <- filteredState - sqrt(filteredStateVar)

# plot
plot(systemState)
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
print("rate of finding systemStates in 1 sigma")
print(length((which(filteredStateM1Sigma < systemState & filteredStateP1Sigma > systemState))) / timeSteps)
print("variance of deviation( std(systemState - filteredState) )")
print(sqrt(var(filteredState - systemState)))
