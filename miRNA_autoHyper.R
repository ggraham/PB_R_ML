miRNA_autoHyper<-function(miRNA, miRNAhits, set=length(miRNAhits)){
  tot<-length(miRNA)
  sel<-length(miRNAhits)
  miRNAnames<-unique(miRNAhits$miRNA)
  outvec<-data.frame(stringsAsFactors = FALSE)
  for ( i in miRNAnames){
    a<-sum(miRNAhits$miRNA==i)
    b<-sum(miRNA$miRNA==i)
    outvec<-rbind(outvec, c(b, a, phyper(a, b, tot-b, sel, lower.tail = FALSE)))
  }
  row.names(outvec)<-miRNAnames
  print(head(outvec))
  outvec<-cbind(outvec, p.adjust(outvec[,3], method = "BH"))
  colnames(outvec)<-c("N in total", "N in sample", "pval", "corPval")
  return(outvec)
}