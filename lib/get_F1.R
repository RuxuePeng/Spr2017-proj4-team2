#######################################################################################
## function to compute F1 score vector 
## of all the possible partition methods in same level of hierarchical clustering ##
#NOTE: need to have: Data as list first,each list contain info about 1 citation; 
#                    y as the true label vector to compute a+c and b+d
#######################################################################################
Fun1 = function(R,y,init_label){
  a_b <- 0
  a = 0
  q1 <- R[1]
  q2 <- R[2]
  a = init_label[q1]==init_label[q2]
  a_b = (init_label[q1]==init_label[q2])&(y[q1]==y[q2])
  return(c(a = a, a_b = a_b))
}

get_F1 = function(init_label,data,A_C = a_c,B_D = b_d){
  #pairlist: all the possible pair assignments of all the articles
  #pairnumber: number of all the possible pair assignments
  y = data$AuthorID
  w = 1:length(init_label)
  pairlist = expand.grid(w,w)
  pairlist = pairlist[(pairlist[,1]>pairlist[,2]),]
  pairnumber = nrow(pairlist)
  
  ## compute match/mismatch of this partition method
    a_b <- 0
    a = 0
    # PARALLEL PROCESSING
    cores <- detectCores()-1
    cl2 = makeCluster(cores,type = "FORK")
    df = parApply(cl2,pairlist,1,Fun1,y,init_label)
    stopCluster(cl2)
    
    a = rowSums(df)[1]
    a_b = rowSums(df)[2]
    c_d <- pairnumber-a_b
    c = A_C - a
    d = c_d - c
    accu = (a+d)/(a_b + c_d)
    ## compute the precision,recall and F1 score of this partition method
    precision = a/a_b
    recall = a/A_C
    F1 = (2*precision*recall)/(precision+recall)   
   return(F1)
}
