parBootMI<-function(d, data1, method="mi"){
  require(boot)
  if(method=="mi"){
    bobj<-boot(data=1:24, statistic=bootMI, R=10000, dat=data1[d,])
    return(c(mean(bobj$t), sd(bobj$t)))
  }
  if(method=="cor"){
    bobj<-boot(data=1:24, statistic=bootCor, R=50, dat=data1[d,])
    return(c(mean(bobj$t), sd(bobj$t)))
  }
  
}
bootMI<-function(x=NULL, ind, dat=NULL){
  require(entropy)
  x1<-dat[ind]
  x2<-dat[ind+24]
  while (min(x1)==max(x1)){
    print(paste("Sampling error type 1", x1[1]))
    ind<-sample(24, 24, replace = TRUE)
    x1 <-dat[ind]
    x2 <-dat[(ind+24)]
  }
  while (min(x2)==max(x2)){
    print(paste("Sampling error type 2", x2[1]))
    ind<-sample(24, 24, replace = TRUE)
    x1 <-dat[ind]
    x2 <-dat[(ind+24)]
  }
  d<-discretize2d(x1 = x1, x2 = x2, numBins1 = 7, numBins2 = 7)
  return(mi.Dirichlet(d, a = 1/49))
}
bootCor<-function(x=NULL, ind, dat=NULL){
  x1<-dat[ind]
  x2<-dat[ind+24]
  while (min(x1)==max(x1)){
    print(paste("Sampling error type 1", x1[1]))
    ind<-sample(24, 24, replace = TRUE)
    x1 <-dat[ind]
    x2 <-dat[(ind+24)]
  }
  while (min(x2)==max(x2)){
    print(paste("Sampling error type 2", x2[1]))
    ind<-sample(24, 24, replace = TRUE)
    x1 <-dat[ind]
    x2 <-dat[(ind+24)]
  }
  #print(x1)
  #print(x2)
  return(cor(x = x1, y = x2))
}