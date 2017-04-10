get_GoldStand = function(mydata){
  y = mydata$AuthorID
  df = expand.grid(1:nrow(mydata),1:nrow(mydata))
  a_c = 0
  for(i in 1:nrow(df)){
    q1 = df[i,1]
    q2 = df[i,2]
    a_c = a_c + (y[q1]==y[q2])
  }
  a_c = as.numeric(a_c)
  b_d = choose(nrow(mydata),2)-a_c
  return(list (y = y,a_c = a_c,b_d = b_d))
}
