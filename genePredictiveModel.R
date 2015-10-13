genePredictiveModel<-function(x, dat=miso_MXE_data_CI_filter){
  set.seed(77)
  library(glmnet)
  library(caret)
  library(xgboost)
  library(Matrix)
  library(data.table)
  library(doMC)
  registerDoMC(4)
  
  splice<-dat
  resp<-factor(ifelse(phenoDat2$Metastasis.at.Diagnosis[x]=="NO", "N", "Y"))
  #dat<-data.frame(Fusion=phenoDat2$Fusion.by.RNA.PCR[2:24],
  #                Sample=phenoDat2$Sample[2:24],
  #                Site=phenoDat2$Site.of.Sampling[2:24])
  #dat<-cbind(dat, data.frame(t(splice[,2:24])))
  #print(head(dat))
  print(resp)
  trC<-trainControl(method = "repeatedcv", repeats = 10000, number=2, p = 0.6, verboseIter = TRUE, summaryFunction = ggDefaultSummary, allowParallel = TRUE)
  tr<-train(x = t(splice[,x]), y = resp, method = "glmnet", metric="Kappa", trControl = trC
            ,tuneGrid = expand.grid(
  #            alpha=c(0, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1),
              alpha=c(0.8),
                                    lambda=c(1, 0.5, 0.1, 0.05, 0.01, 0.001, 0.0001)
                                   )
            )
  return(tr)
}