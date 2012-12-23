# kalman filter using fkf
source("FilteringHelper.R")

# INPUT
# Tt:  system model coefficient
# HHt: system model noize variance matrix ^2
# Zt:  observation model coefficient
# GGt: observation model noize variance matrix ^2

# OUTPUT
# att: filtered state
# at:  predicted state
# Ptt: variance of att
# Pt:  variance of at
KalmanFilter <- function(P0=matrix(1), dt=matrix(0), ct=matrix(0), Tt=array(1,c(1,1,1)), Zt=array(1,c(1,1,1)),
                         HHt=array(1,c(1,1,1)), GGt=array(1,c(1,1,1))){
  return(
    function(observation, initialState, check.input = TRUE){
      if(check.input) checkInput(observation=observation,initialState=initialState)
      library(FKF)
      fkf.obj <- fkf(a0=initialState,P0=P0,dt=dt,ct=ct,Tt=Tt,Zt=Zt,HHt=HHt,GGt=GGt,yt=observation,check.input=check.input)
      return(fkf.obj)
    }
  )
}

