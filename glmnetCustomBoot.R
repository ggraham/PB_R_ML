bootResample<-function(resp, dat){
  require(ROSE)
  dat<-data.frame(response = resp, dat)
  nam<-colnames(dat)
  nam.fix<-make.names(nam)
  colnames(dat)<-nam.fix
  res<-ROSE(response ~ ., data = dat, p = 0.5)$data
  yout<-res[,1]
  res<-res[,-1]
  colnames(res)<-nam[-1]
  res<-as.matrix(res)
  return(list(Data = res, Response = yout))
  #lev<-levels(y)
  #n1<-which(y==lev[1])
  #n2<-which(y==lev[2])
  #lout<-list()
  #for (i in 1:n){
  #  n1k<-sample(n1, replace = TRUE)
  #  n2k<-sample(n2, replace = TRUE)
  #  lout[[i]]<-sample(c(n1k, n2k), replace = FALSE)
  #}
  #names(lout)<-paste("ggResample", 1:n, sep = "")
  #return(lout)
}
bootGlmNet<-function(y, x, nboot=100, alpha = 1, lambda = 0.01){
  library(foreach)
  library(doMC)
  set.seed(77)
  registerDoMC(2)
  dat<-foreach( i = 1:nboot, .combine = cbind, .inorder = TRUE) %dopar% {
    resampDat<-bootResample(resp = y, dat = x)
    inter<-glmNetBootInternal(origdat = x, origy = y, dat = resampDat$Data, y = resampDat$Response, alpha = alpha, lambda = lambda)
    out<-c(inter$AUC, inter$SENS, inter$SPEC, inter$COEF)
    names(out)<-c("AUC", "SENS", "SPEC", colnames(x))
    out
  }
  #  dat$SENS<-c(dat$SENS, inter$SENS)
  #  dat$SPEC<-c(dat$SPEC, inter$SPEC)
  #  dat$COEF<-rbind(dat$COEF, inter$COEF)
  #}
  return(dat)

}
glmNetBootInternal<-function(origdat, origy, dat, y, alpha, lambda){
  lev<-factor(origy, levels = c("YES", "NO"), labels=c(0, 1))
  require(pROC)
  require(glmnet)
  g<-glmnet(x = dat, y = y, family = "binomial", alpha = alpha)
  pred<-predict(object = g, newx = origdat, type = "class", s = lambda)[,1]
  rocObj<-try(roc(response = as.numeric(lev), predictor = as.numeric(factor(pred, levels = c("YES", "NO"), labels=c(0,1)))))
  co<-coef(g, lambda = lambda)
  sens<-sensitivity(data = factor(pred, levels = c("YES", "NO")), reference = factor(y, levels = c("YES", "NO")), positive="YES")
  spec<-specificity(data = factor(pred, levels = c("YES", "NO")), reference = factor(y, levels = c("YES", "NO")), negative="NO")
  return(list(AUC = as.numeric(rocObj$auc), COEF = as.numeric(co), SENS = sens, SPEC = spec))
  #print(str(rose.roc))
  #return(list(AUC = rose.roc$auc, COEF = as.numeric(co), SENS = rose.roc$"false.positive.rate", SPEC = rose.roc$"true.positive.rate"))
}