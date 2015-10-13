geneToPB<-function(g, pb.granges=hq.gff.transcripts){
  require(org.Hs.eg.db)
  require(GenomicRanges)
  require(GenomicFeatures)
  hs<-org.Hs.eg.db
  coord<-select(hs, keys = g, columns = c("CHR", "CHRLOC", "CHRLOCEND"), keytype = "SYMBOL")
  loci<-GRanges(seqnames = paste("chr", coord$CHR, sep = ""), 
                ranges = IRanges(start = abs(coord$CHRLOC), 
                                 end = abs(coord$CHRLOCEND), 
                                 names = g), 
                strand = ifelse(coord$CHRLOC<0, "-", "+"))
  pbloci<-subsetByOverlaps(hq.gff.transcripts, loci)
  return(list(GENE = unique(pbloci$gene_id), START = min(start(loci)), END = max(end(loci)), CHR = as.character(unique(seqnames(loci))[1]), STRAND = unique(as.character(strand(loci)))[1]))
}
# pbToGene(pb, pb.granges=hq.gff.transcripts){
#   require(org.Hs.eg.db)
#   require(GenomicRanges)
#   require(GenomicFeatures)
# }