newCreateResample<-function(y, n){
  lev<-levels(y)
  n1<-which(y==lev[1])
  n2<-which(y==lev[2])
  lout<-list()
  for (i in 1:n){
    n1k<-sample(n1, replace = TRUE)
    n2k<-sample(n2, replace = TRUE)
    lout[[i]]<-sample(c(n1k, n2k), replace = FALSE)
  }
  names(lout)<-paste("ggResample", 1:n, sep = "")
  return(lout)
}