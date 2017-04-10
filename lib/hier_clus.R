# iterate over hierarchial clustering levels

Hier_clus= function(Fea_wl,Fea_wol,lamda = rep(1/ncol(Fea_wol),ncol(Fea_wol)),data = Train[[1]]){
  #hard start
  init_cluster = kmeans(Fea_wl,centers = 50,nstart =5)
  G = get_GoldStand(data)
  y = G$y;a_c =G$a_c;b_d = G$b_d 
  # initialization
  label_i= init_cluster$cluster
  
  for (v in 1: (50 -length(unique(y)))){
    labels= get_labels(init_label= label_i)
    for(j in 1:2){
     score <- aaply(labels,2,get_score)
      # extract the best score and N_hat and its F1 score
      ind_N_hat <- which.max(score)
      #ind_N_star <- which(F1==max(F1))
      label_i <-labels[,ind_N_hat]
      F1_N_hat = get_F1(label_i)
      
      # extract N_star
      for(i in 1:ncol(labels)){
        tmp = get_F1(init_label = labels[,i])
        if(tmp>F1_N_hat){
          ind_N_star = i
          break()
        }
      }
      label_j <- labels[,ind_N_star]
      # update lamda
      Fea_wol_star <- data.frame(cbind(label_1=label_i,Fea_wol))
      Fea_wol_hat <- data.frame(cbind(label_1=label_j,Fea_wol))
      F_T_star <- colSums(aggregate(Fea_wol_star,by=list(Fea_wol_star$label_1),FUN=mean))[-c(1,2)]
      F_T_hat <- colSums(aggregate(Fea_wol_hat,by=list(Fea_wol_hat$label_1),FUN=mean))[-c(1,2)]
      incre <- sqrt(sum((F_T_hat-F_T_star)^2))
      lamda = lamda + F_T_star - F_T_hat
    }
  }
  return(lamda)
}

