# iterate over hierarchial clustering levels
Hier_clus_test= function(Fea_wl,Fea_wol,Lamda = best_lamda,data = Test[[1]]){
  #hard start
  lamda = Lamda
  G = get_GoldStand(data)
  y = G$y
  a_c =G$a_c
  b_d = G$b_d 
  
  N = length(unique(y)) + 10
  init_cluster = kmeans(Fea_wl,centers = N,nstart =1)
  
  # initialization
  label_i= init_cluster$cluster
  
  for (v in 1: (N -length(unique(y)))){
    labels= get_labels(init_label= label_i, data)
    cores <- detectCores()-1
    cl1 = makeCluster(cores,type = "FORK")
    score<- parApply(cl1,labels,2,get_score,lamda,Fea_wol)
    stopCluster(cl1)
    #score <- aaply(labels,2,get_score,lamda, Fea_wol)
    # extract the best score and N_hat and its F1 score
    ind_N_hat <- which.max(score)
    #ind_N_star <- which(F1==max(F1))
    label_i <-labels[,ind_N_hat]
    
  }
  F1_N_hat = get_F1(label_i,A_C = a_c,B_D = b_d,data = data)
  return(list(label_i,F1_N_hat))
}