makeEventList<-function(filenames){
  lout<-list()
  for (i in 1:length(filenames)){
    lout[[i]]<-read.table(filenames[i], header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  }
  return(lout)
}

collectHeatmapPSIs<-function(flist, names){
  evlist<-makeEventList(flist)
  names(evlist)<-names
  evlist_short<-lapply(evlist, FUN = function (x) x[,c(1:4)])
  evlist_short_intersect_names<-Reduce(function(x,y) intersect(x,y), 
                                       x = lapply(evlist_short, FUN = function(x) x$event_name))
  evlist_short_names<-lapply(evlist_short, function(x) {row.names(x)<-x[,1]; return(x)})
  evlist_short_names_intersect<-lapply(evlist_short_names, FUN = function(x) x[evlist_short_intersect_names,])
  ev_common<-sapply(evlist_short_names_intersect, function(x) x[,2])
  row.names(ev_common)<-evlist_short_intersect_names
  if (sum(rowSums(ev_common)==0)!=0){
    return(ev_common[-which(rowSums(ev_common)==0),])
  } else {
    return(ev_common)
  }



}