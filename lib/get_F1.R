#######################################################################################
## function to compute F1 score vector 
## of all the possible partition methods in same level of hierarchical clustering ##
#NOTE: need to have: Data as list first,each list contain info about 1 citation; 
#                    y as the true label vector to compute a+c and b+d
#######################################################################################
get_F1 = function(init_label, A_C = a_c,B_D = b_d){
  #pairlist: all the possible pair assignments of all the articles
  #pairnumber: number of all the possible pair assignments
  w = 1:length(init_label)
  pairlist = expand.grid(w,w)
  pairlist = pairlist[(pairlist[,1]>pairlist[,2]),]
  pairnumber = nrow(pairlist)
  
  ## compute match/mismatch of this partition method
    a_b <- 0
    for(k in 1:nrow(pairlist)){
      q1 <- pairlist[k,1]
      q2 <- pairlist[k,2]
      #a_b means a+b
      a_b <- a_b+(init_label[q1]==init_label[q2])
    }
    c_d <- pairnumber-a_b
    a = -c_d+A_C+B_D
    ## compute the precision,recall and F1 score of this partition method
    precision = a/a_b
    recall = a/A_C
    F1 = (2*precision*recall)/(precision+recall)   
   return(F1)
}