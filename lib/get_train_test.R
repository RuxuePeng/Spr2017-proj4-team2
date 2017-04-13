#################################################################################
# train_test function is used to generate the trainset and testset of a given 
# dataset, random split the data into half and half in each group, each group 
# here means each identity author ID.

# Output is a list which the first element is the trainset and the second element 
# is the testset
#################################################################################

get_train_test<-function(sub_df){
  n<-nrow(sub_df)
  indice<-sample(1:n, ceiling(n/2))
  train <- sub_df[indice, ]
  test <- sub_df[-indice,]
  return(list(train = train, test = test))
}

train_test<-function(mydata){
  train_test<-list()
  number_unique<-length(unique(Data$AGupta.txt$AuthorID))
  for (i in 1: number_unique){
    sub_df<-subset(Data$AGupta.txt, AuthorID == i)
    train_test[[i]]<-get_train_test(sub_df)}

  train_test<-do.call(function(...) mapply(bind_rows,...,SIMPLIFY = F), args = train_test)

  return(train_test)
}


