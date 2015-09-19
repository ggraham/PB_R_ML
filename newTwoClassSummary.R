newTwoClassSummary<-function(data, lev, model){
  require(pROC)
  if (length(levels(data$obs)) > 2) 
    stop(paste("Your outcome has", length(levels(data$obs)), 
               "levels. The twoClassSummary() function isn't appropriate."))
  if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
    stop("levels of observed and predicted data do not match")
  #print(data)
  #print(lev)
  rocObj<-try(roc(response = as.numeric(factor(data$obs, levels = lev, labels=c(0,1))), predictor = as.numeric(factor(data$pred, levels = lev, labels=c(0,1)))))
  if(class(rocObj)[1]=="try-error"){
    print("roc issue")
    out<-c(0.5, sensitivity(data = factor(data$pred, levels = lev), reference = factor(data$obs, levels = lev), positive=lev[1]),  specificity(data = factor(data$pred, levels = lev), reference = factor(data$obs, levels = lev), negative=lev[2]))
    names(out)<-c("AUC", "Sens", "Spec")
    return(out)
  } else {
  out<-c(rocObj$auc, sensitivity(data = factor(data$pred, levels = lev), reference = factor(data$obs, levels = lev), positive=lev[1]), specificity(data = factor(data$pred, levels = lev), reference = factor(data$obs, levels = lev), negative=lev[2]))
  names(out)<-c("AUC", "Sens", "Spec")
  return(out)
  }
}