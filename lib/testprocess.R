f3<-function(list){
  for(i in 1:length(list)){
    list[[i]]<-list[[i]]
  }
  return(list)
}
#probability
testfinal<-function(vec){
  probtemp<-matrix(rep(0,nrow(temp)*length(vec)),nrow=nrow(temp),byrow=TRUE)
  for(i in 1:length(vec)){
   # flag<-sum(vec[i]==temp.num)
    flag = vec[i] %in% temp.num
    if(flag==0){probtemp[,i]<-rep(1/nrow(temp),nrow(temp))}else{
      probtemp[,i]<-final[,which(vec[i]==temp.num)]}
  }
  return(probtemp)
}
#judge id
judgement<-function(list){
  result<-NULL
  for(i in 1:length(list)){
    prob<-testfinal(list[[i]])
    prob[prob==0]<-0.0001
    result[i]<-temp$AuthorID[which.max(apply(prob,1,prod))]
  }
  return(result)
}

