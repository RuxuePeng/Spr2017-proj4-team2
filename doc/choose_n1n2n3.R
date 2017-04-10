f2 = function(data){
  PCA_overlap = prcomp(create_overlap(data[,1]), center = T, scale. = T)
  return(ncol(PCA_overlap$x))
}

dim_train = laply(Train,f2)
dim_test = laply(Test,f2)
num1 = min(c(dim_train,dim_test))

f2 = function(data){
  PCA_overlap = prcomp(create_paper(data), center = T, scale. = T)
  return(ncol(PCA_overlap$x))
}

dim_train = laply(Train,f2)
dim_test = laply(Test,f2)
num2 = min(c(dim_train,dim_test))

f2 = function(data){
  PCA_overlap = prcomp(create_journal(data), center = T, scale. = T)
  return(ncol(PCA_overlap$x))
}

dim_train = laply(Train,f2)
dim_test = laply(Test,f2)
num3 = min(c(dim_train,dim_test))
