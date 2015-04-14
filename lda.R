lda<-function(input){
  require(topicmodels)
  dtm <- DocumentTermMatrix(input[[2]])
k <- length(unique(input[[1]]$topic))
lda <-LDA(dtm,k)
}
  