# This function is used for geting sum(log(P(A1k | xi)))+P(xi)
# Input is a citation which just contains coauthors
# Output is a vector which is the log value

Log_A1_value<-function(citation_A1,author_name){
  test_i<-citation_A1
  test_i<-strsplit(test_i,split = ";")
  test_i<-as.data.frame(matrix(unlist(test_i)),stringsAsFactors = FALSE)
  test_i<-as.data.frame(apply(test_i,2,function(x)gsub('\\s+','',x)),stringsAsFactors = FALSE)
  test_i<-as.data.frame(test_i[!test_i$V1 == author_name,],stringsAsFactors = FALSE)
  colnames(test_i)<-"coauthor"
  #class(test1)
  
  # if the citation doesn't contain coauthors
  if (nrow(test_i) == 0){
    log_P_A1k_xi<-P_N_xi
  }else{
    # else the citation doesn't contain coauthors
    # extract the number of times xi coauthors with A1k
    seen_df_i<-matrix(NA, nrow = m,ncol = nrow(test_i))
    colnames(seen_df_i) <- test_i$coauthor
    for(i in 1:ncol(seen_df_i)){
      if(test_i[i,1] %in% colnames(SEEN_DF)){
        seen_df_i[,i]<-SEEN_DF[,colnames(SEEN_DF)==test_i[i,1]]
      }else{
        seen_df_i[,i]<-rep(0,m)
      }
    }
    
    # Calculate P(A1k|xi)
    P_A1_x<-matrix(NA,nrow=m,ncol = ncol(seen_df_i))
    for(i in 1:m){
      for (j in 1: ncol(seen_df_i)){
        if(nxi_seen[i] !=0){
          P_A1k_seen_coxi<-seen_df_i[i,j]/nxi_seen[i]
          P_A1_x[i,j] <- P_A1k_seen_coxi*seen_term[i] +unseen_term[i]
        }else{
          P_A1_x[i,j] <- unseen_term[i]
        }
      }
    }
    # Calculate P(A1|xi)
    log_P_A1k_xi<-rep(NA,length=n)
    for(i in 1: m){
      LOG<-0
      for(j in 1:ncol(P_A1_x)){
        LOG<-LOG+log(P_A1_x[i,j])
      }
      log_P_A1k_xi[exist_author_id[i]]<-LOG
    }
    log_P_A1k_xi[setdiff(1:n,exist_author_id)]<--25
  }
  
  log_P_A1k_xi[is.na(log_P_A1k_xi)]<--25
  log_P_A1k_xi[log_P_A1k_xi == -Inf] <- -25
  
  return(log_P_A1k_xi)
}
