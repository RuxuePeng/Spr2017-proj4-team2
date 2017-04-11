get_feature = function(data = Train[[1]]){
  #create features
  overlap_data = create_overlap(data[,1])
  paper_data = create_paper(data)
  Journal_data = create_journal(data)
  
  # Use PCA to reduce feature
  PCA_overlap = prcomp(overlap_data, center = T, scale. = T)
  num1 = which(PCA_overlap$sdev > 1e-02)
  #num1 = 95
  overlap_data = PCA_overlap$x[,num1]
  
  PCA_paper = prcomp(paper_data, center = T, scale. = T)
  #num2 = 121
  num2 = which(PCA_paper$sdev > 1e-02)
  paper_data = PCA_paper$x[,num2]
  T
  PCA_journal = prcomp(Journal_data, center = T, scale. = T)
  #num3 = 121
  num3 = which(PCA_journal$sdev > 1e-02)
  Journal_data = PCA_journal$x[,num3]
  
  Fea_wol <- cbind(overlap_data,paper_data,Journal_data) 
  Fea_wl = cbind(AuthorID = unlist(data$AuthorID),Fea_wol)
  return(Fea_wl)
}