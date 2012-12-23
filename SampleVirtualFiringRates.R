SampleVirtualFiringRates <- function(n, systemFun=sampleLocalGaussian, observFun=gaussianObservation, seed=8887){
  # system model: fun
  x <- systemFun(n)
  if(any(x < 0)) stop("error: negative firing rates are generated")
  stopifnot(all(x >= 0))
  # observation model
  y <- observFun(x)
  return(list(x=x,y=y))
}

# system model
sampleLocalGaussian <- function(n, init_x=0.5, sd=0.01){
  # x[t] = x[t-1] + rnorm()
  diff_x <- rnorm(n=n,mean=0,sd=sd)
  init_x + c(cumsum(diff_x))
}

# observation model
gaussianObservation <- function(x, sd=0.01){
  # y[t] = x[t] + rnorm()
  rnorm(n=length(x),mean=x,sd=sd)
}

# observation model
bernoulliObservation <- function(x){
  # y[t] = Bernoulli(x[t])
  rbinom(n=length(x),size=1,prob=x)
}
