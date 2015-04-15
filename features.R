features <-function(input,k){
  require(RTextTools)
  input.matrix <- create_matrix(cbind(input$title,input$text), language = "english", removeNumbers = TRUE, stemWords = TRUE, weighting = weightTfIdf)
  analytics <-NULL
 # lets fold this up
 for(fold in 1:k){
   #train using (k-1)n/k instances and test using n/k
   sizeOfTest <- floor(nrow(input)/k)
   print(nrow(input))
   print(sizeOfTest)
   testLower <-  ((fold-1)*sizeOfTest)+2
   print(testLower)
   testUpper<- testLower + sizeOfTest
   print(testUpper)
   if(testUpper+1 >= nrow(input)){testUpper <- nrow(input)-1}
   input.corpus <-create_container(input.matrix,input$topic,trainSize = c(1:1, 297:1178), testSize =testLower:testUpper, virgin = FALSE)
   models <- train_models(input.corpus,algorithms = c("SVM"))
   results <- classify_models(input.corpus, models)
   analytics <- list(analytics,create_analytics(input.corpus, results))
   return(analytics)
   }
}