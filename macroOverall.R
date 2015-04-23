macroOverall <- function(res){
  macroOverallAverage <- data.frame() #output frame
  numClasses <- length(res[[1]]) #number of classes
  numFolds <- length(res) #number of folds

  for(measure in 1:3){
    sum <- 0 #the sum for this measure across all classes and folds
    for(c in 1:numClasses){
      for(i in 1:numFolds){
        sum <- sum+res[[i]][[c]][[measure+4]] #add this measure for this class and fold
      }
    }
    macroOverallAverage[1,measure] <- sum/(numFolds*numClasses) #get the overall macro average for this measure
  }
  return(macroOverallAverage)
}
