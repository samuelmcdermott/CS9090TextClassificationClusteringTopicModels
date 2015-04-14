features <-function(input,k){
  require(RTextTools)
  input.matrix <- create_matrix(cbind(input$title,input$text), language = "english", removeNumbers = TRUE, stemWords = TRUE, weighting = weightTfIdf)
 # lets fold this up
 for(fold in 1:k){
   #train using (k-1)n/k instances and test using n/k
   sizeOfTest <- floor((k-1)*nrow(input)/k)
   testLower <-  ((fold-1)*sizeOfTest)+2
   testUpper<- testLower + sizeOfTest + 2
   input.corpus <-create_container(input.matrix,input$topic,trainSize = c(1:testLower-1, testUpper+1:nrow(input)), testSize =testLower:testUpper, virgin = FALSE)
   }
#
#  #models <- train_models(input.corpus,algorithms = c("SVM","MAXENT"))
#  #results <- classify_models(input.corpus, models)
#  analytics <- create_analytics(input.corpus, svm)
 return(input.matrix)
}