create_paper = function(data = Train[[1]]){
  it_train <- itoken(data$Paper,
                     preprocessor = tolower, 
                     tokenizer = word_tokenizer,
                     ids = data$PaperID,
                     # turn off progressbar because it won't look nice in rmd
                     progressbar = FALSE)
  vocab <- create_vocabulary(it_train, stopwords = c("a", "an", "the", "in", "on",
                                                     "at", "of", "above", "under"))
  vectorizer <- vocab_vectorizer(vocab)
  dtm_train <- create_dtm(it_train, vectorizer)
  tfidf <- TfIdf$new()
  dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
  return(as.matrix(dtm_train_tfidf))
}
