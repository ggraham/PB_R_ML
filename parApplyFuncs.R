parApplyMI<-function(x, R=50){
  mi<-replicate(R, singleParApplyMI(x))
  return(c(MI=mean(mi), SD=sd(mi)))
}
singleParApplyMI<-function(x){
  ind<- sample(24, 24, replace = TRUE)
  x1 <- x[ind]
  x2 <- x[(ind+24)]
  while (min(x1)==max(x1)){
    ind<- sample(24, 24, replace = TRUE)
    x1 <- x[ind]
    x2 <- x[(ind+24)]
  }
  while (min(x2)==max(x2)){
    ind<- sample(24, 24, replace = TRUE)
    x1 <- x[ind]
    x2 <- x[(ind+24)]
  }
  d<-discretize2d(x1 = x1, x2 = x2, numBins1 = 7, numBins2 = 7)
  return(mi.Dirichlet(d, a = 1/49))
}