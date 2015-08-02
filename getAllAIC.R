getAllAIC<-function(var1, misoTable){
  AICout<-c()
  for ( i in 1:nrow(misoTable) ){
    glm1<-glm(varIn~., data = data.frame(varIn=factor(var1), misoTable[i,]), family = binomial(logit))
    
    AICout<-c(AICout, anova(glm1, test="Chisq")$Pr[2])
  }
  names(AICout)<-row.names(misoTable)
  return(AICout)
}