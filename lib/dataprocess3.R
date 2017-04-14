###Data processing function
Train_Half <- function(Mat){
  rownumber <- ceiling(nrow(Mat)/2)
  return(Mat[1:rownumber,])
}
Split_Train <- function(DataMat){
  DataMat = as.data.frame(DataMat)
  ddply(DataMat,.(DataMat$AuthorID),Train_Half)[,-1]}

dataprocess3<-function(dat){
  #dat$AuthorID <- sub("_.*","",dat$Coauthor)
  #dat$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", dat$Coauthor)
  #dat$Coauthor <- gsub("<","",sub("^.*?\\s","", dat$Coauthor))
  #dat$Paper <- gsub("<","",dat$Paper)
  #dat$PaperID <- rownames(dat)
  dat<-dat[,c(3,4)]
  dat$AuthorID<-as.numeric(dat$AuthorID)
  dat<-dat[order(dat$AuthorID),]
  number<-seq(1:nrow(dat))
  dat<-data.frame(number,dat)
  return(dat)
}

trainpro<-function(dat){
  trainset<-Split_Train(dat)
  return(trainset)
}

f23<-function(M){
  total<-NULL
  for(i in 1:nrow(M)){
    total<-paste(total,M$Journal[i],sep = " ")
  }
  return(total)
}