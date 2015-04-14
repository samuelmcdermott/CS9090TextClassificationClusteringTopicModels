features <-function(input,k){
  require(RTextTools)
 #kfold stuff obvs
 input.matrix <- create_matrix(cbind(input$title,input$text), language = "english", removeNumbers = TRUE, stemWords = TRUE, weighting = weightTfIdf)
 input.corpus <-create_container(input.matrix, input$topic, trainSize = 1:20, testSize = 1:8, virgin = FALSE)
 svm <- cross_validate(input.corpus,9,algorithm = "SVM")
 #models <- train_models(input.corpus,algorithms = c("SVM","MAXENT"))
 #results <- classify_models(input.corpus, models)
 analytics <- create_analytics(input.corpus, svm)
 return(analytics)
}