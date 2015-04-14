features <-function(input,k){
  require(RTextTools)
  input.matrix <- create_matrix(cbind(input$title,input$text), language = "english", removeNumbers = TRUE, stemWords = TRUE, weighting = weightTfIdf)
#   lets fold this up
#  for(fold in 1:k){
#    #train using (k-1)n/k instances and test using n/k
#    testSize <- floor((k-1)*n/k)
#    testLower <-  ((fold-1)*testSize)+2
#    testUpper<- testLower + testSize + 2
#      }
# input.corpus <-create_container(input.matrix,input$topic,  testSize = 1:floor(nrow(input)/10) , virgin = FALSE)
# #trainSize = (1:testLower-1, testUpper+1:n), testSize =testLower:testUpper
#  #models <- train_models(input.corpus,algorithms = c("SVM","MAXENT"))
#  #results <- classify_models(input.corpus, models)
#  analytics <- create_analytics(input.corpus, svm)
 return(input.matrix)
}