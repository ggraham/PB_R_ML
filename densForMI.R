densForMI<-function(tabMI, tabSD){
  tabMI[tabSD>=0.3]<-NA
  sumMI<-apply(tabMI,1,function(x) sum(x, na.rm = TRUE))
  contribMI<-apply(tabMI,1,function(x) sum(x>=0.5, na.rm = TRUE))
  contribMI[is.na(contribMI)]<-nrow(tabMI)
  return(sumMI/contribMI)
}