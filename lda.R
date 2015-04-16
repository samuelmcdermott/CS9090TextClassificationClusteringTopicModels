lda<-function(input){
  require(topicmodels)
  require(RTextTools)
  input.matrix <- create_matrix(cbind(input$title,input$text), language = "english", removeNumbers = TRUE, stemWords = TRUE)
  
k <- length(unique(input$topic))
lda <-LDA(input.matrix,10)
topics(lda)
}
  