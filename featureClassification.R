featureClassification<-function(input,n,k,classifier,...){
  require(RTextTools)
  require(e1071)
  require(RWeka)
  #just incase user inputted incorrectly
  classifier <- toupper(classifier)

  #put everything in right format (& make the topic a numeric factor)
  input$topic <- as.numeric(factor(input$topic))
  input$title <- as.character(input$title)
  input$text <- as.character(input$text)
  #create a dtm
  input.matrix <- convertToDtm(cbind(input$title,input$text),n)


  print(paste("Creating ",n,"-gram features"))
  #these will store the analytical information during and after folding
  analytics <-NULL
  allAnalytics <- NULL

  # lets fold this up
  for(fold in 1:k){
    print(paste("Fold ", fold))
    #train using (k-1)n/k instances and test using n/k, see documentation on what this is doing
    sizeOfTest <- floor(nrow(input)/k)
    testLower <-  ((fold-1)*sizeOfTest)+1 #position in corpus
    testUpper<- testLower + sizeOfTest #position in corpus
    if(testUpper >= nrow(input)){testUpper <- nrow(input)-1} #correction for when we reach the top


    if(classifier=="NB"){
      #Naive Bayes
      print("Using Naive bayes classifier")
      NB.matrix <-as.matrix(input.matrix) #for Naive Bayes we need it as an actual matrix
      #train using the training data
      NB.model = naiveBayes(NB.matrix[c(1:testLower,testUpper:(nrow(input))),], as.factor(input[c(1:testLower,testUpper:(nrow(input))),c("topic")]))
      #predict using the testing data
      NB.predicted = predict(NB.model,NB.matrix[(testLower+1):(testUpper-1),])
      #get the analytics for this fold and append to previous folds
      analytics <- append(analytics,foldAnalytics(cbind(NB.predicted,input[(testLower+1):(testUpper-1),c("topic")]),unique(input$topic)))
    }else{
      #create a container to use with RTextTools
      input.corpus <-create_container(input.matrix,as.factor(input$topic),trainSize = c(1:testLower,testUpper:(nrow(input))), testSize =c((testLower+1):(testUpper-1)), virgin = FALSE)
      #which classifier from RTextTools
      print(paste("Using ",classifier," classifier"))
      #train the classifier, carrying any arguments that user inputted for classifier
      model <- train_model(input.corpus,classifier,list(...))
      #predict using the test data
      result <- classify_model(input.corpus, model)
      #get the analytics for this fold and append to previous folds
      analytics<- append(analytics,foldAnalytics(cbind(result[,1],input$topic[(testLower+1):(testUpper-1)]),unique(input$topic)))

    }
  }

  print("Calculating Micro and Macro averages")
  #get the micro and macro
  micro <- unname(microOverall(analytics))
  macro <- unname(macroOverall(analytics))
  micro <- cbind("micro",micro)
  macro <- cbind("macro",macro)
 print(micro)
 print(macro)
 names(micro) <- c("avg-type","precision","accuracy","recall")
 names(macro) <- c("avg-type","precision","accuracy","recall")
  allAnalytics <- rbind(allAnalytics,macro,micro)
  names(allAnalytics) <- c("avg-type","precision","accuracy","recall")
  write.csv(allAnalytics,paste0(n,"gram_",k,"fold_",classifier,".csv"))

  return(allAnalytics)
}