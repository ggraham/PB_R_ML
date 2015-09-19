topGOAutomate<-function(nclust=14, cluster=NULL, ont=NULL, MIdata=NULL, names_logical=NULL, goAnnot=NULL, hclust_obj=NULL){
  require(topGO)
#   if(is.null(hclust_obj)){
#     hclust_obj<-hclust(dist(MIdata, method = "euclidean"), method = "ward.D")
#   }
  if(!is.null(names_logical)){
    k_c<-names_logical
  } else {
    k_c<-factor(as.integer(cutree(hclust_obj, k = nclust)==cluster))
    names(k_c)<-names(cutree(hclust_obj, k=5))
  }
  print(table(k_c))
print(class(k_c))
  topGO_obj<-new("topGOdata", allGenes=k_c, annot=annFUN.gene2GO, gene2GO=goAnnot$BP, ontology="BP", nodeSize=5)
  #
  test.stat <- new("elimCount", testStatistic = GOFisherTest, name = "Elim Test", cutOff = 0.01)
  resultElim <- getSigGroups(topGO_obj, test.stat)
  test.stat <- new("weightCount", testStatistic = GOFisherTest, name = "Weight Test", sigRatio = "ratio")
  resultWeight <- getSigGroups(topGO_obj, test.stat)
  test.stat <- new("classicCount", testStatistic = GOFisherTest, name = "Classic Test")
  resultFisher <- getSigGroups(topGO_obj, test.stat)

  return(GenTable(topGO_obj, Weight=resultWeight, Classic=resultFisher, Elim=resultElim, orderBy="Weight", ranksOf="Classic", topNodes=20))
}
genInput<-function(k=NULL, eventRanges, eventCutree){
  print(table(eventCutree%in%k))
  test<-unique(subsetByOverlaps(hq.gff.transcripts, eventRanges[eventCutree%in%k])$gene_id)
  test.in<-(unique(hq.gff.transcripts$gene_id)%in%test)
  test.in<-as.factor(as.numeric(test.in))
  names(test.in)<-unique(hq.gff.transcripts$gene_id)
  return(test.in)
}