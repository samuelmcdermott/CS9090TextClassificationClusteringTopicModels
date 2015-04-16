featureClassification<-function(input,k,n){
  require(RTextTools)
  require(e1071)
  #put everything in right format (& make the topic a numeric factor)
  input$topic <- as.numeric(factor(input$topic))
  input$title <- as.character(input$title)
  input$text <- as.character(input$text)
  
  input.matrix <- create_matrix(cbind(input$title,input$text), language = "english",ngramLength = n, removeNumbers = TRUE, removePunctuation = TRUE, removeSparseTerms = 0.1, removeStopwords = TRUE, stemWords = TRUE, toLower = TRUE, weighting = tm::weightTfIdf)
  NB.matrix <-as.matrix(input.matrix)
  
  NB.analytics <-NULL
  SVM.analytics<-NULL
  MAXENT.analytics<-NULL
  GLMNET.analytics <-NULL
  NNET.analytics <- NULL
 # lets fold this up
 for(fold in 1:k){
   print(fold)
   #train using (k-1)n/k instances and test using n/k
   sizeOfTest <- floor(nrow(input)/k)
   testLower <-  ((fold-1)*sizeOfTest)
   testUpper<- testLower + sizeOfTest
   if(testUpper+1 >= nrow(input)){testUpper <- nrow(input)-1}
   
   print("training models..")
   input.corpus <-create_container(input.matrix,as.numeric(factor(input$topic)),trainSize = c(1:testLower,testUpper:nrow(input)), testSize =testLower+1:testUpper-1, virgin = FALSE)
   models <- train_models(input.corpus,algorithms = c("SVM","MAXENT","GLMNET","NNET"))
   results <- classify_models(input.corpus, models)
   
   #SVM
   print("SVM analytics")
   SVM.analytics <- append(SVM.analytics,foldAnalytics(cbind(results$SVM_LABEL,input$topic[testLower+1:testUpper-1]),unique(input$topic)))
   
   #MAXENT
   print("MAXENT analytics")
   MAXENT.analytics <- append(MAXENT.analytics,foldAnalytics(cbind(results$MAXENT_LABEL,input$topic[testLower+1:testUpper-1]),unique(input$topic)))
      
   #GLMNET
   print("GLMNET analytics")
   GLMNET.analytics <- append(GLMNET.analytics,foldAnalytics(cbind(results$GLMNET_LABEL,input$topic[testLower+1:testUpper-1]),unique(input$topic)))
   
   #NNET
   print("NNET analytics")
   NNET.analytics <- append(NNET.analytics,foldAnalytics(cbind(results$NNET_LABEL,input$topic[testLower+1:testUpper-1]),unique(input$topic)))
   
  #don't be Naive!
  print("Naive bayes")
  NB.model = naiveBayes(NB.matrix[c(1:testLower,testUpper:nrow(input)),], input$topic[c(1:testLower,testUpper:nrow(input))])
  NB.predicted = predict(NB.model,NB.natrix[testLower+1:testUpper-1,])
  NB.analytics <- append(NB.analytics,foldAnalytics(cbind(NB.predicted,input$topic[testLower+1:testUpper-1]),unique(input$topic)))
  
  
   }
 print("Micro and Macro")
 SVM.micro <- cbind("SVM-mic",microOverall(SVM.analytics))
 SVM.macro <- cbind("SVM-mac",macroOverall(SVM.analytics))
 
 MAXENT.micro <- cbind("MAXENT-mic",microOverall(MAXENT.analytics))
 MAXENT.macro <- cbind("MAXENT-mac",macroOverall(MAXENT.analytics))
 
 GLMNET.micro <- cbind("GLMNET-mic",microOverall(GLMNET.analytics))
 GLMNET.macro <- cbind("GLMNET-mac",macroOverall(GLMNET.analytics))
 
 NNET.micro <- cbind("NNET-mic",microOverall(NNET.analytics))
 NNET.macro <- cbind("NNET-mac",macroOverall(NNET.analytics))
 
 NB.micro <- cbind("NB-mic",microOverall(NB.analytics))
 NB.macro <- cbind("NB-mac",macroOverall(NB.analytics))
 
 
 
 allAnalytics <- rbind(SVM.micro,SVM.macro, MAXENT.micro, MAXENT.macro, GLMNET.micro, GLMNET.macro, NNET.micro, NNET.macroNB.micro, NB.macro)
 write.csv(allAnalytics,"allAnalytics.csv")
 
return(allAnalytics)
}