source("Filtering.R")
source("SampleVirtualFiringRates.R")

# virtual firing rate sampling
hiddenParam <- list(systemVariance=0.05^2, observationVariance=0.1^2)
sysFun <- function(n){ sampleLocalGaussian(n=n,init_x=0.7,sd=sqrt(hiddenParam$systemVariance)) }
obsFun <- function(x){ gaussianObservation(x=x,sd=sqrt(hiddenParam$observationVariance)) }
sampledData <- SampleVirtualFiringRates(n=100,systemFun=sysFun, observFun=obsFun, seed=88,allowNegative=F)
observation <- matrix(sampledData$y, nrow=1)

# filter
source("KalmanFilter.R")
ssmParam <- list(systemVariance=0.01, observationVariance=0.1)
estimationTarget <- NULL
estimationTarget <- list(HHt=array(ssmParam$systemVariance,c(1,1,1)),
                         GGt=array(ssmParam$observationVariance,c(1,1,1)))
filterArgs <- list(P0=matrix(1), HHt=array(ssmParam$systemVariance,c(1,1,1)), GGt=array(ssmParam$observationVariance,c(1,1,1)))
filteringObject <- Filtering(observation=observation,initialState=0,filterGenerator=KalmanFilter,
                    filterArgs=filterArgs,
                    estimationTarget=estimationTarget
                    )
filter.obj <- filteringObject$filter.obj
fit.obj <- filteringObject$fit.obj
filteredState <- filter.obj$att[1,]
filteredStateP1Sigma <- filteredState + sqrt(filter.obj$Ptt[1,1,])
filteredStateM1Sigma <- filteredState - sqrt(filter.obj$Ptt[1,1,])

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



