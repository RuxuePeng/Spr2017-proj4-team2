get_feature = function(data = Train[[1]]){
  #create features
  overlap_data = create_overlap(data[,1])
  paper_data = create_paper(data)
  Journal_data = create_journal(data)
  
  # Use PCA to reduce feature
  PCA_overlap = prcomp(overlap_data, center = T, scale. = T)
  #num = which(PCA_paper$sdev > 1e-02)
  num1 = 95
  overlap_data = PCA_overlap$x[,(1:num1)]
  
  PCA_paper = prcomp(paper_data, center = T, scale. = T)
  num2 = 121
  #num = which(PCA_paper$sdev > 1e-02)
  paper_data = PCA_paper$x[,(1:num2)]
  T
  PCA_journal = prcomp(Journal_data, center = T, scale. = T)
  num3 = 121
  #num = which(PCA_journal$sdev > 1e-02)
  Journal_data = PCA_journal$x[,(1:num3)]
  
  Fea_wol <- cbind(overlap_data,paper_data,Journal_data) 
  Fea_wl = cbind(label = unlist(data$AuthorID),Fea_wol)
  return(list(Fea_wol,Fea_wl))
}