microOverall = function(res){
  microAverage = data.frame() #output frame
  numClasses = length(res[[1]]) #number of classes
  numFolds = length(res) #number of folds
  sumFP= 0 #sum of false positives across all classes, across all folds
  sumTN=0 #sum of true negatives across all classes, across all folds
  sumTP=0 #sum of true positives across all classes, across all folds
  sumFN = 0 #sum of false negatives across all classes, across all folds
  for(c in 1:numClasses){
    for(i in 1:numFolds){
      sumFP = sumFP + res[[i]][[c]]$falsePositive #add the measure for this fold and class
      sumTN = sumTN + res[[i]][[c]]$trueNegative
      sumTP = sumTP + res[[i]][[c]]$truePositive
      sumFN = sumFN + res[[i]][[c]]$falseNegative
      }
  }
  #calculate micro average across all classes for this measure:
  microAverage[1,1] = sumTP/(sumTP+sumFP) #precision
  microAverage[1,2] = (sumTP +sumTN)/(sumTP + sumFP+sumFN+sumTN) #accuracy
  microAverage[1,3] = sumTN/(sumFP+sumTN) #recall

return(microAverage)
}
