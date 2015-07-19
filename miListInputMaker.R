miListInput<-function(data_PSIs, data_FPKMs){
  data_PSIs<-as.matrix(data_PSIs)
  data_FPKMs<-as.matrix(data_FPKMs)
  #filter on FPKM variance
  filter_vec_FPKM<-apply(data_FPKMs, MARGIN = 1, FUN = "var")
  FPKMs_filter<-data_FPKMs[(filter_vec_FPKM>=quantile(filter_vec_FPKM,0.25)),]
  #filter on median FPKM
  FPKMs_filter<-FPKMs_filter[apply(FPKMs_filter, MARGIN = 1, FUN = function(x) median(x)>=2),]
  print(dim(FPKMs_filter))
  #
  #filter on PSI variance
  filter_vec_PSI<-apply(data_PSIs, MARGIN = 1, FUN = "var")
  PSIs_filter<-data_PSIs[(filter_vec_PSI>=quantile(filter_vec_PSI,0.25)),]
  #filter on PSI distribution
  #PSIs_filter<-PSIs_filter[apply(PSIs_filter, MARGIN = 1, FUN = function(x) all(sum(x>=0.1)>=2, sum(x<=0.9)>=2)),]
  print(dim(PSIs_filter))
  #
  gr<-expand.grid(psi.n=row.names(PSIs_filter), gene.n=row.names(FPKMs_filter))
  gr.dat<-cbind(PSIs_filter[gr$psi.n,], 
                FPKMs_filter[gr$gene.n,])
  out<-list(DATA=gr.dat, NAMES=gr)
  return(out)
}