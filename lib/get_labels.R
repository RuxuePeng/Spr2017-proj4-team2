get_labels = function(init_label = init_cluster$cluster){

Grid = length(unique(init_label))
w = 1:nrow(dtm_train)
pairlist = expand.grid(w,w)
pairlist = pairlist[(pairlist[,1]>pairlist[,2]),]
pairnumber = nrow(pairlist)

#(1) initialization
partition = expand.grid(1:Grid,1:Grid)
partition = partition[( partition[,1] > partition[,2]),]
# assign initial labels of all the partition methods
# which is just unique number for each example
column = init_cluster$cluster
labels = matrix(column, nrow=length(column), ncol= nrow(partition)) 

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
  label_us[merge_list] = 1
  ## update labels matrix with new label
  labels[,i]=label_us
  }
  return(labels)
}