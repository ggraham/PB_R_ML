parseAndAnnotate<-function(PBOfInterest, pbGRanges, txdb){
  require(org.Hs.eg.db)
  listOut<-list(BP=list(), CC=list(), MF=list())
  delVec<-c()
  validKeys<-AnnotationDbi::keys(org.Hs.eg.db, keytype = "UCSCKG")
  #pB<-txtProgressBar(min=0, max=length(PBOfInterest), width = 100, title = "Retriving GO terms", style = 3)
  for ( i in 1:length(PBOfInterest)){
    #if ( i %% 10 == 0 ){
    #  setTxtProgressBar(pb = pB, value = i)
    #}
    set<-transcriptsByOverlaps(x = txdb, ranges = pbGRanges[pbGRanges$gene_id==PBOfInterest[i]], type="any")
    print(set)
    if (length(set)==0){
      delVec<-c(delVec, i)
      next()
    }
    if (!all((keys = set$tx_name) %in% validKeys)){
      delVec<-c(delVec, i)
      next()
    }
    GOs.terms<-select(org.Hs.eg.db, keys = set$tx_name, columns = c("SYMBOL", "GOALL"), keytype = c("UCSCKG"))
    return(GOs.terms)
    #listOut$BP[[i]]<-c(unique(GOs.terms$GOALL[GOs.terms$ONTOLOGY=="BP"]))
    #listOut$CC[[i]]<-c(unique(GOs.terms$GOALL[GOs.terms$ONTOLOGY=="CC"]))
    #listOut$MF[[i]]<-c(unique(GOs.terms$GOALL[GOs.terms$ONTOLOGY=="MF"]))
  }
  #close(pB)
  #listOut$BP<-listOut$BP[-delVec]
  #listOut$MF<-listOut$MF[-delVec]
  #listOut$CC<-listOut$CC[-delVec]
  #PBOfInterest.trim<-PBOfInterest[-delVec]
  #names(listOut$BP)<-PBOfInterest.trim
  #names(listOut$CC)<-PBOfInterest.trim
  #names(listOut$MF)<-PBOfInterest.trim
  #return(listOut)
}