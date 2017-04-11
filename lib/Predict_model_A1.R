# This function is used for prediction the label
# Input is a citation which just contains coauthors
# Output is label, 0 means given citation doesn't contain any coauthors 

citation_A1<-function(citation_A1,author_name){
  test_i<-citation_A1
  test_i<-strsplit(test_i,split = ";")
  test_i<-as.data.frame(matrix(unlist(test_i)),stringsAsFactors = FALSE)
  test_i<-as.data.frame(apply(test_i,2,function(x)gsub('\\s+','',x)),stringsAsFactors = FALSE)
  test_i<-as.data.frame(test_i[!test_i$V1 == author_name,],stringsAsFactors = FALSE)
  colnames(test_i)<-"coauthor"
  #class(test1)
  
  if (nrow(test_i) == 0 ){
    predict_label<-0
  }else{
    # extract the number of times xi coauthors with A1k
    seen_df_i<-matrix(NA, nrow = n,ncol = nrow(test_i))
    colnames(seen_df_i) <- test_i$coauthor
    for(i in 1:ncol(seen_df_i)){
      if(test_i[i,1] %in% colnames(SEEN_DF)){
        seen_df_i[,i]<-SEEN_DF[,colnames(SEEN_DF)==test_i[i,1]]
      }else{
        seen_df_i[,i]<-rep(0,n)
      }
    }
    
    # Calculate P(A1k|xi)
    P_A1_x<-matrix(NA,nrow=n,ncol = ncol(seen_df_i))
    for(i in 1:n){
      for (j in 1: ncol(seen_df_i)){
        if(nxi_seen[i] !=0){
          P_A1k_seen_coxi<-seen_df_i[i,j]/nxi_seen[i]
          P_A1_x[i,j] <- P_A1k_seen_coxi*seen_term[i] +unseen_term[i]
        }else{
          P_A1_x[i,j] <- unseen_term[i]
        }
      }
    }
    # Calculate P(Xi|C)
    P_Xi_c<-rep(NA,length=n)
    for(i in 1: n){
      LOG<-0
      for(j in 1:ncol(P_A1_x)){
        LOG<-LOG+log(P_A1_x[i,j])
      }
      P_Xi_c[i]<-LOG + log(p_xi[i])
    }
    
    # Predict xi
    predict_label<-names(nxi_seen)[which.max(P_Xi_c)]
  }
  
  return(predict_label)
}
