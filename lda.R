lda<-function(input,n,k){
  require(topicmodels)

  #input <- input[sample(1:nrow(input),2000,replace=FALSE),]
  #put everything in right format (& make the topic a numeric factor)
  input$topic <- factor(input$topic)
  input$title <- as.character(input$title)
  input$text <- as.character(input$text)

 dtm<- convertToDtm(cbind(input$text,input$title),n)
 #return(dtm)
 rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
 dtm   <- dtm[rowTotals> 0, ]           #remove all docs without words
lda <-LDA(dtm,k)
return(lda)
}
