features <-function(input,k){
  require(RTextTools)
  require(e1071)
  input.matrix <- create_matrix(cbind(input$title,input$text), language = "english", removeNumbers = TRUE, stemWords = TRUE, weighting = weightTfIdf)
  analytics <-NULL
 # lets fold this up
 for(fold in 1:k){
   #train using (k-1)n/k instances and test using n/k
   sizeOfTest <- floor(nrow(input)/k)
   print(nrow(input))
   print(sizeOfTest)
   testLower <-  ((fold-1)*sizeOfTest)
   print(testLower)
   testUpper<- testLower + sizeOfTest
   print(testUpper)
   if(testUpper+1 >= nrow(input)){testUpper <- nrow(input)-1}
   input.corpus <-create_container(input.matrix,as.numeric(factor(input$topic)),trainSize = c(1:testLower,testUpper:nrow(input)), testSize =testLower+1:testUpper-1, virgin = FALSE)
   models <- train_models(input.corpus,algorithms = c("SVM","MAXENT"))
   results <- classify_models(input.corpus, models)
   analytics <- create_analytics(input.corpus, results)
   #don't be Naive!
   mat = as.matrix(input.matrix)
   NBClassifier = naiveBayes(mat[testLower+1:testUpper-1],as.numeric(factor(input$topic[testLower+1:testUpper-1])))
   NBpredicted = predict(NBclassifier,mat[c(1:testLower,testUpper:nrow(input))])
   table<- table(mat[c(1:testLower,testUpper:nrow(input))],predicted)
   
   return(table)
   }
}