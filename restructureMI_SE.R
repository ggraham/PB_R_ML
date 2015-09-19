restructureMI_SE<-function(x, ex_grid){
  psi_names<-unique(ex_grid[,1])
  gene_names<-unique(ex_grid[,2])
  mi.matrix<-matrix(x$MI, ncol=length(psi_names), nrow=length(gene_names), dimnames = list(gene_names, psi_names))
  sd.matrix<-matrix(x$SD, ncol=length(psi_names), nrow=length(gene_names), dimnames = list(gene_names, psi_names))
  return(list(MI=mi.matrix, SD=sd.matrix))
}