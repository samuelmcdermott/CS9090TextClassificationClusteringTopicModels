foldAnalytics <-function(predictedAndActual,classes){
  classesList <-list() #stores the list of classes analytics
  for(c in 1:length(classes)){
    #get the predictions and actual classes for the actual classes that are this class (true set)
    trueset <- predictedAndActual[predictedAndActual[,2]==classes[c],]
    #get the predictions and actual classes for the actual classes that aren't this class (false set)
    falseset <- predictedAndActual[predictedAndActual[,2]!=classes[c],]
    #get the performance measures for the model, fold and class
    falsePositive <- length(which(falseset[,1]==classes[c]))
    trueNegative <- length(which(falseset[,1]!=classes[c]))
    falseNegative <- length(which(trueset[,1]!=classes[c]))
    truePositive <- length(which(trueset[,1]==classes[c]))
    precision <- truePositive/(truePositive+falsePositive)
    accuracy <- (truePositive+trueNegative)/(length(predictedAndActual[,1]))
    recall<- truePositive/(truePositive+falseNegative)
    #make a list of these measures
    measureList = list(falsePositive=falsePositive,trueNegative = trueNegative, truePositive = truePositive, falseNegative = falseNegative,precision=precision, accuracy = accuracy, recall=recall)
    #if any of the results are NaN, they should be 0
    measureList = rapply( measureList, f=function(x) ifelse(is.nan(x),0,x), how="replace" )
    #add this measure list to the class list
    classesList <- append(classesList,list(measureList))
  }
  return(list(classesList))
}