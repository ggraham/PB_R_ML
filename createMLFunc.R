require(caret)
require(doMC)

createMLFunction<-function(input_table_list, ncores=2){
  set.seed(5)
  registerDoMC(ncores)
  grid<-expand.grid(C=0.001)
  trC<-trainControl(method = "cv", 
                    number = 5,
                    repeats = 5,
                    #timingSamps=1,
                    verboseIter = FALSE, 
                    allowParallel = TRUE,
                    classProbs=FALSE)
  
  model_list<-list()
  for (i in 1:nrow(input_table_list$PSI)){
    print(i)
    resp<-input_table_list$PSI[i,]
    model<-train(x = t(input_table_list$FPKM), 
                 y = as.numeric(resp),
                 method = "svmRadialCost",
                 preProcess = c("center", "scale"),
                 tuneGrid=grid,
                 trControl = trC,
                 metric="RMSE")
    model_list[[i]]<-varImp(model, scale = FALSE, drop = FALSE)
    gc()
  }
  return(model_list)
}