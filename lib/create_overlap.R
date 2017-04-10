## Create overlapping coauthor
triming = function(x){gsub("^\\s|\\s$", "", x)}
library(plyr)

create_overlap = function(Authorcol){
  tmp = strsplit(Authorcol,split = ";")
  tmp = llply(tmp,triming)
  # removing the author himself/herself
  tmp = llply(tmp,function(t) t[-1])
  overlap_mat = matrix(NA,ncol = length(Authorcol),nrow = length(Authorcol))
  for(i in 1:length(tmp)){
    for(j in 1:length(tmp)){
      overlap_mat[i,j] = sum(tmp[[i]] %in% tmp[[j]])
    }
  }
  return(overlap_mat)
}