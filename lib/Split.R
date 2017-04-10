Train_Half <- function(Mat){
  rownumber <- ceiling(nrow(Mat)/2)
  return(Mat[1:rownumber,])
}
Test_Half <- function(Mat){
  rownumber <- ceiling(nrow(Mat)/2)
  return(Mat[(rownumber+1):nrow(Mat),])
}

Split_Train <- function(DataMat){ddply(DataMat,.(DataMat$AuthorID),Train_Half)[,-1]}
Split_Test <- function(DataMat){ddply(DataMat,.(DataMat$AuthorID),Test_Half)[,-1]}
