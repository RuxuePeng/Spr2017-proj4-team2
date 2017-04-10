get_feature = function(data = Train[[1]]){
  #create features
  overlap_data = create_overlap(data[,1])
  paper_data = create_paper(data)
  Journal_data = create_journal(data)
  
  # Use PCA to reduce feature
  PCA_paper = prcomp(paper_data, center = T, scale. = T)
  num = which(PCA_paper$sdev > 1e-02)
  paper_data = PCA_paper$x[,num]
  
  PCA_journal = prcomp(Journal_data, center = T, scale. = T)
  num = which(PCA_journal$sdev > 1e-02)
  Journal_data = PCA_journal$x[,num]
  
  Fea_wol <- cbind(overlap_data,paper_data,Journal_data) 
  Fea_wl = cbind(label = unlist(data$AuthorID),Fea_wol)
  return(list(Fea_wol,Fea_wl))
}