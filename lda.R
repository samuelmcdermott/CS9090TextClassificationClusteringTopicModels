lda<-function(input,n,k){
  require(topicmodels)

  #put everything in right format (& make the topic a numeric factor)
  input$topic <- factor(input$topic)
  input$title <- as.character(input$title)
  input$text <- as.character(input$text)
  #get the document term matrix
  dtm<- convertToDtm(cbind(input$text,input$title),n)

  rowTotals <- apply(dtm , 1, sum) #find the sum of words in each Document
  dtm <- dtm[rowTotals> 0, ]#remove all docs without words
  #create a topic model with k topics
  lda <-LDA(dtm,k)
  #return this topic model
  return(lda)
}
