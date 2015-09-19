getAllAIC<-function(var1, varB=NULL, misoTable){
  AICout<-c()
  for ( i in 1:nrow(misoTable) ){
    if( is.null(varB)==TRUE){
      glm1<-glm(varIn~., data = data.frame(varIn=factor(var1), misoTable[i,]), family = binomial(logit))
      vPval<-2
    } else {
      glm1<-glm(varIn~varB+., data = data.frame(varIn=factor(var1), varB=factor(varB), misoTable[i,]), family = binomial(logit))
      vPval<-3
    }
    AICout<-c(AICout, anova(glm1, test="Chisq")$Pr[vPval])
  }
  names(AICout)<-row.names(misoTable)
  return(AICout)
}