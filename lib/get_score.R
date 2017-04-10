#function for computing score, input is dataframe "labels"
get_score <- function(label_i,lamda){
     Fea_wol_1 <- data.frame(cbind(Fea_wol,label_1=label_i))
     Fea_wol_1 <- aggregate(Fea_wol_1,by=list(Fea_wol_1$label_1),FUN=mean)
     score <- sum((as.matrix(Fea_wol_1[,-c(1,ncol(Fea_wol_1))])%*%lamda))
 }