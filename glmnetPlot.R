glmnetPlot<-function(glmnetList){
  library(ggplot2)
  library(reshape2)
  library(wesanderson)
  li<-length(glmnetList)
  lout<-list()
  for ( i in 1:li ){
     res<-glmnetList[[i]]$results
#     b.lam<-res$lambda[which.max(res$Kappa)]
#     res<-res[which(res$lambda==b.lam),]
    if (grepl("PB", names(glmnetList)[i])) {
      res<-cbind(res, Source = "PB")
    } else {
      res<-cbind(res, Source = "RefSeq")
    }
#    b.alp<-res$alpha[which.max(res$ROC)]
#    res<-res[which(res$alpha==b.alp),]
    lout[[i]]<-res
  }
  names(lout)<-names(glmnetList)
  lout.m<-melt(lout, id.vars = c("alpha", "lambda", "Accuracy", "Kappa", "Sens", "Spec", "AccuracySD", "KappaSD", "SensSD", "SpecSD", "Source"))
  print(head(lout.m))
  lout.m$L1<-factor(as.character(lout.m$L1), levels = names(glmnetList))
  a<-ggplot(lout.m, aes(x=lambda, y=Sens, col=L1, shape=Source))
  a<-a+geom_line(size=1)
  a<-a+geom_point(size=4)
  #a<-a+geom_errorbar(width=0.05)
  a<-a+theme_bw()
  a<-a+scale_color_manual(values=colorRampPalette(rep(c(wes_palette("Zissou")),2))(14))
  #a<-a+ylab("Specificity")
  a<-a+xlab("Lambda")
  a<-a+ggtitle(label = "GLMNET - Elastic Net")
  a<-a+scale_x_log10()
  #a<-a+ylim(c(0, 1))
  plot(a)
}