glmnetPlot<-function(glmnetList){
  library(ggplot2)
  library(reshape2)
  library(wesanderson)
  li<-length(glmnetList)
  lout<-list()
  for ( i in 1:li ){
    res<-glmnetList[[i]]$results
    b.lam<-res$lambda[which.max(res$ROC)]
    res<-res[which(res$lambda==b.lam),]
#    b.alp<-res$alpha[which.max(res$ROC)]
#    res<-res[which(res$alpha==b.alp),]
    lout[[i]]<-res
  }
  names(lout)<-names(glmnetList)
  lout.m<-melt(lout, id.vars = c("alpha", "lambda", "ROC", "Sens", "Spec", "ROCSD", "SensSD", "SpecSD"))
  print(head(lout.m))
  lout.m$L1<-factor(as.character(lout.m$L1), levels = names(glmnetList))
  a<-ggplot(lout.m, aes(x=alpha, y=ROC, ymin=Spec-SpecSD, ymax=Spec+SpecSD, col=L1))
  a<-a+geom_line(size=1)
  #a<-a+geom_errorbar(width=0.05)
  a<-a+theme_bw()
  a<-a+scale_color_manual(values=colorRampPalette(c(wes_palette("Royal1"), wes_palette("Zissou")))(9))
  a<-a+ylab("AUC")
  a<-a+xlab("Alpha")
  a<-a+ggtitle(label = "GLMNET - Elastic Net")
  a<-a+ylim(c(0.6, 0.75))
  plot(a)
}