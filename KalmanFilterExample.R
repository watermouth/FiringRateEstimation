source("Filtering.R")
source("SampleVirtualFiringRates.R")

# virtual firing rate sampling
sysFun <- function(n){ sampleLocalGaussian(n=n,init_x=0.5,sd=0.01)}
obsFun <- function(x){ gaussianObservation(x=x,sd=0.05)}
sampledData <- SampleVirtualFiringRates(n=100,systemFun=sysFun, observFun=obsFun, seed=88)
observation <- matrix(sampledData$y, nrow=1)

# filter
source("KalmanFilter.R")
ssmParam <- list(systemVariance=0.01, observationVariance=0.1)
kf <- KalmanFilter(P0=matrix(1), HHt=array(ssmParam$systemVariance,c(1,1,1)), GGt=array(ssmParam$observationVariance,c(1,1,1)))
kf.obj <- kf(observation=observation,initialState=0)

filteredState <- kf.obj$att[1,]
filteredStateP1Sigma <- filteredState + kf.obj$Ptt[1,1,]
filteredStateM1Sigma <- filteredState - kf.obj$Ptt[1,1,]

# plot
plot(observation[1,] )
points(filteredState, pch=20, col="blue")
points(filteredStateP1Sigma, pch=20, col="green")
points(filteredStateM1Sigma, pch=20, col="green")





