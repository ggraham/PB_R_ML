ggPlotIsoforms<-function(gene=NULL, samples=c(25:27), order=25, sampleNames=c("TC32 WT", "TC32 shFli1", "TC32 YK"), isoform_dat=isoform_FPKMs){
  require(ggplot2)
  isoNames<-grep(gene, row.names(isoform_dat), value = TRUE)
  #isoNames<-reorder(isoNames, as.numeric(isoform_dat[isoNames,order]))
  isoNames<-factor(isoNames, levels = isoNames[order(isoform_dat[isoNames, order], decreasing = FALSE)])
  #print(isoNames)
  #plot_frame<-data.frame(stringsAsFactors = TRUE)
  vec<-c()
  for ( i in samples ){
    vec<-c(vec, isoform_dat[as.character(isoNames),i])
    #print(cbind(Isoform=isoNames,FPKM=as.numeric(isoform_dat[isoNames,i]),Sample=rep(sampleNames[abs(min(samples)-(i+1))],length(isoNames))))
  }
  print(vec)
  plot_frame<-data.frame(Isoform=rep(isoNames, length(samples)),FPKM=vec,Sample=rep(sampleNames, each = length(isoNames)))
  #print(as.numeric(levels(plot_frame$FPKM)))
  #plot_frame$FPKM<-as.numeric(levels(plot_frame$FPKM))[as.numeric(plot_frame$FPKM)]
  plot_frame[plot_frame$FPKM<=1,2]<-1
  print((plot_frame))
  a<-ggplot(data = plot_frame, aes(x=Isoform, y=FPKM, fill=Sample))
  a<-a+geom_bar(stat = "identity", position="dodge")
  a<-a+scale_y_log10()
  a<-a+coord_flip()
  a
}