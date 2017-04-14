# Define a function which input is a list contains all citation of a specific author
# return is a data frame which 
# 1st column is the unique coauthor. 
# 2nd column is number of times xi coauthors with this coauthor
# 3rd column is p(Co|xi)
# 4th column is number of xi coauthors with this coauthor when this coauthor is seen coauthor
# 5th column is p(seen | co, xi)
# 6th column is P(unseen | co, xi)

df<-function(A){
  # A is a list and a identity author
  coa<-data.frame(name=matrix(unlist(A)),stringsAsFactors = FALSE)
  coa<-as.data.frame(apply(coa,2,function(x)gsub('-','',x)),stringsAsFactors = FALSE)
  coa<-as.data.frame(apply(coa,2,function(x)gsub('\\s+','',x)),stringsAsFactors = FALSE)
  # Count the number of time one paricular coauthor appears
  df<-as.data.frame(table(coa$name),stringsAsFactors = FALSE)
  
  # Check if there any "" character
  #df<-df[-which(df$Var1 == ""),]
  
  # Order df by frequency, and remove the first row which is the author him/herself
  df<-df[order(df$Freq,decreasing = T),][-1,]
  
  # The number of coauthors of A in each paper
  count<-as.numeric(lapply(A,length))
  # Calculate p(Co|xi)
  df$prob_cox1<- sum(count != 1)/length(count)
  
  #number of times xi coauthors with seen coauthors
  df$numer_xi_seen_coau<-(df$Freq>1)*(df$Freq)
  
  # Calculate p(seen|co,xi)
  df$prob_seen_cox1<-sum(df$numer_xi_seen_coau)/(sum(df$Freq))
  # Calculate p(unseen|co,xi)
  df$prob_unseen_cox1<-1-df$prob_seen_cox1
  
  return(df)
}
