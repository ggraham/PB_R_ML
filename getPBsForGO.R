getPBsForGO<-function(GOterm=NULL){
  require(biomaRt)
  require(GenomicRanges)
  require(GenomicFeatures)
  outList<-list()
  outNames<-c()
  biom<-useDataset(dataset = "hsapiens_gene_ensembl",
                   mart = useMart(host="feb2014.archive.ensembl.org",
                                  biomart="ENSEMBL_MART_ENSEMBL"))
  for ( k in 1:length(GOterm) ){
 
    tBar<-txtProgressBar(min=1, max = length(GOterm), style=3)
    bm<-getBM(filters="go_id", values=GOterm[k], mart=biom, attributes = c("start_position", "end_position", "chromosome_name", "hgnc_symbol"))
    if (!nrow(bm)>=1) {
      next
    } else {
      #print(head(bm))
      if (k %% 10 == 0){
        setTxtProgressBar(pb = tBar, value = k)
      }
      #print(k)
      PBs<-c()
      trans_hit<-findOverlaps(query = GRanges(seqnames = paste("chr", bm$chromosome_name, sep=""), ranges = IRanges(start = bm$start_position, end = bm$end_position, names = bm$hgnc_symbol)), subject = hq.gff.transcripts)
      #print(head(trans_hit))
      #print(length(trans_hit@subjectHits))
      if (length(trans_hit@subjectHits)==0){
        next
      }
      #print(trans_hit)
      trans_hit_GRanges<-hq.gff.transcripts[trans_hit@subjectHits]
      #print(trans_hit_GRanges)
      PBs<-unique(trans_hit_GRanges$gene_id)
      #print(PBs)
      outNames<-c(outNames, GOterm[k])
      outList[[length(outList)+1]]<-PBs
      #print(outList)
    }

  }
  close(tBar)
  names(outList)<-outNames
  return(outList)
}