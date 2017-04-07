#######################################################################################
## function to compute F1 score vector 
## of all the possible partition methods in same level of hierarchical clustering ##
#NOTE: need to have: Data as list first,each list contain info about 1 citation; 
#                    y as the true label vector to compute a+c and b+d
#######################################################################################
F1 = function(one_data = Data[[1]], a_c = a_c,b_d = b_d){

  #Grid: the total number of example
  #pairlist: all the possible pair assignments of all the articles
  #pairnumber: number of all the possible pair assignments
  Grid = nrow(one_data)
  w = 1:max(as.numeric(one_data$PaperID))
  pairlist = expand.grid(w,w)
  pairlist = pairlist[(pairlist[,1]>pairlist[,2]),]
  pairnumber = nrow(pairlist)

  
  #(1) initialization
  partition = expand.grid(1:Grid,1:Grid)
  partition = partition[( partition[,1] > partition[,2]),]
  # assign initial labels of all the partition methods
  # which is just unique number for each example
  column = seq(1,Grid)
  labels = matrix(column, nrow=length(column), ncol= nrow(partition)) 
  F1 = rep(NA,ncol(labels))
  
  #(2) loop over different partitioning in this level and compute F1 score vec
  for(i in 1:nrow(partition)){
    ## extract previous predictions
    label_us=labels[,i]
    ## choose partition method
    r1 <- partition$Var1[i]
    r2 <- partition$Var2[i]
    ## find the two clusters of r1 and r2
    merge_list = c(which(label_us==r1),which(label_us==r2))
    ## empty the 1st position for new assignment
    label_us = label_us + 1
    ## merge and assign new cluster as 1st cluster
    label_us[r1] = 1
    label_us[r2] = 1
    ## update labels matrix with new label
    labels[,i]=label_us
    
    ## compute match/mismatch of this partition method
    a_b <- 0
    for(k in 1:nrow(pairlist)){
      q1 <- pairlist[k,1]
      q2 <- pairlist[k,2]
      #a_b means a+b
      a_b <- a_b+(label_us[q1]==label_us[q2])
    }
    
    c_d <- pairnumber-a_b
    a = -c_d+a_c+b_d
    ## compute the precision,recall and F1 score of this partition method
    precision = a/a_b
    recall = a/a_c
    F1[i]= (2*precision*recall)/(precision+recall)   
  }
  return(list(F1vec = F1,labels = labels,model_list = partition))
}
