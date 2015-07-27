topGOAutomate<-function(nclust=14, cluster=NULL, ont=NULL, MIdata=NULL, goAnnot=NULL){
  hclust_obj<-hclust(dist(MIdata, method = "euclidean"), method = "ward.D")
  k_c<-factor(as.integer(cutree(hclust_obj, k = nclust)==cluster))
  names(k_c)<-names(cutree(hclust_obj, k=14))
  #
  topGO_obj<-new("topGOdata", allGenes=k_c, annot=annFUN.gene2GO, gene2GO=goAnnot$BP, ontology="BP")
  #
  topGO_fisher<-runTest(object = topGO_obj, algorithm = "classic", statistic = "fisher")
  topGO_ks<-runTest(object = topGO_obj, algorithm = "classic", statistic = "ks")
  topGO_weight<-runTest(object = topGO_obj, algorithm = "weight01", statistic = "fisher")
  return(GenTable(topGO_obj, classic=topGO_fisher, KS=topGO_ks, weight=topGO_weight, orderBy="weight", ranksOf="classic", topNodes=20))


  
}