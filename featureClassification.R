featureClassification<-function(input,n,k, classifier,...){
  require(RTextTools)
  require(e1071)

  #just incase user inputted incorrectly
  classifier <- toupper(classifier)

  #put everything in right format (& make the topic a numeric factor)
  input$topic <- as.numeric(factor(input$topic))
  input$title <- as.character(input$title)
  input$text <- as.character(input$text)

  #create a dtm
  input.matrix <- create_matrix(cbind(input$title,input$text), language = "english",ngramLength = n,removeNumbers = TRUE, removePunctuation = TRUE, removeStopwords = TRUE, stemWords = TRUE, toLower = TRUE, weighting = tm::weightTfIdf)
  #for Naive Bayes we need it as an actual matrix
  if(classifier == "NB"){
  NB.matrix <-as.matrix(input.matrix)
  }

  print(paste("Creating ",n,"-gram features"))
  #these will store the analytical information during and after folding
  analytics <-NULL
  allAnalytics <- NULL

  # lets fold this up
  for(fold in 1:k){
    print(paste("Fold ", fold))
    #train using (k-1)n/k instances and test using n/k, see documentation on what this is doing
    sizeOfTest <- floor(nrow(input)/k)
    testLower <-  ((fold-1)*sizeOfTest)+1
    testUpper<- testLower + sizeOfTest
    if(testUpper >= nrow(input)){testUpper <- nrow(input)-1}

    input.corpus <-create_container(input.matrix,as.numeric(factor(input$topic)),trainSize = c(1:testLower,testUpper:(nrow(input))), testSize =c((testLower+1):(testUpper-1)), virgin = FALSE)

    if(classifier=="NB"){
      #don't be Naive!
      print("Using Naive bayes classifier")
      NB.model = naiveBayes(NB.matrix[c(1:testLower,testUpper:(nrow(input))),], input$topic[c(1:testLower,testUpper:(nrow(input)))])
      NB.predicted = predict(NB.model,NB.matrix[testLower+1:testUpper-1,])
      analytics <- append(analytics,foldAnalytics(cbind(NB.predicted,input$topic[testLower+1:testUpper-1]),unique(input$topic)))
    }else{
      print(paste("Using ",classifier," classifier"))
      model <- train_model(input.corpus,classifier,list(...))
      result <- classify_model(input.corpus, model)
      analytics<- append(analytics,foldAnalytics(cbind(result[,1],input$topic[testLower+1:testUpper-1]),unique(input$topic)))

    }
  }

  print("Calculating Micro and Macro averages")

  micro <- unname(microOverall(analytics))
  macro <- unname(macroOverall(analytics))
 print(micro)
 print(macro)
  allAnalytics <- rbind(allAnalytics,macro,micro)
  names(allAnalytics) <- c("avg-type","precision","accuracy","recall","f-measure")
  write.csv(allAnalytics,paste0(n,"gram"-k,"fold",classifier,".csv"))

  return(allAnalytics)
}