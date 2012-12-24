# resamping size points from vector x according to probability
smartsampling <- function(x, size, replace=T, prob=NULL){
  xl <- length(x)
  # if prob == NULL, then prob == constant and it means return x
  if (is.null(prob)){
    if(size %% xl  == 0){
      return(rep(x, size / xl ))
    } else {
      if(size - xl > 0){
        temp <- floor(size / xl)
        return(c(rep(x, temp), sample(x, size - temp * xl, replace=replace)))
      } else {
        return(sample(x, size,replace=replace))
      }
    }
  }
  # normalize
  prob <- prob / sum(prob)
  if (all(prob[1] == prob)){
    return(x)
  }
  ## First, get each points deterministically
  sProb <- size * prob
  detCounts <- floor(sProb)
  sampled <- c(apply(matrix(c(x, detCounts), ncol=2),1, function(v) rep(v[1], v[2])))
  ## Second, update prob and random sampling according to the prob
  prob <- sProb - detCounts
  # normalize
  prob <- prob / sum(prob)
  if(any(is.na(prob))){
    stop('particle weights are not positive!')
    browser()
  }
  sampled <- c(sampled, x[sample(1:xl,size - sum(detCounts),replace,prob)], recursive=T)
  return(sampled)
}

# smartsampling(1:10, 5)



