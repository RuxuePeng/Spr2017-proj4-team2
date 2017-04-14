load("~/Desktop/data/MJones_A1.Rdata")
load("~/Desktop/data/MJones_xi.Rdata")
load("~/Desktop/data/11-journal.Rdata")

b<-Log_A1 + Log_P_xi+pred_prob
load("~/Desktop/data/11-paper.Rdata")

b<-b+pred_prob
predict_label<-apply(b, 2, which.max)

new_mydata<-data.frame(test[,2],test[,1])
colnames(new_mydata)<-c("AuthorID", "coauthor")
#View(new_mydata)
GoldStand<-get_GoldStand(new_mydata)
#F1<-rep(NA,12)
get_F1_accu(predict_label,new_mydata,GoldStand$a_c,GoldStand$b_d)
