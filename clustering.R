clustering <-function(input){
  require(cluster)
  require(mclust)
  require(fpc)

  #get everything in the right format
  input$topic <- as.numeric(factor(input$topic))
  input$title <- as.character(input$title)
  input$text <- as.character(input$text)
  #create a dtm (use unigrams)
  input.matrix <- convertToDtm(cbind(input$title,input$text),1)
  #turn it into a matrix
  input.matrix <- as.matrix(input.matrix,stringsAsFactors = FALSE)
  rownames(input.matrix) <- 1:nrow(input.matrix)

  #normalise matrix
  norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
  input.matrix <- norm_eucl(input.matrix)
  dist.matrix <- dist(input.matrix, method = "euclidean") # distance matrix

  # K-means
  kmean <- kmeans(input.matrix, 10) #do kmeans clustering for 10 clusters
  clusplot(input.matrix, kmean$cluster, color=TRUE, shade=TRUE, labels=FALSE, lines=0) #plot the kmeans over principle components

  # Hierarchical Agglomerative
  HA<- hclust(dist.matrix, method="ward.D2") #do hierarchical clustering
  plot(HA,labels = FALSE) # display dendogram
  HAcut <- cutree(HA, k=10) # cut tree into 10 clusters
  # draw dendogram with red borders around the 10 clusters
  rect.hclust(HA, k=10, border="red")

  #Expectation maximisation
  EM <- Mclust(input.matrix,G=10) # do expectation maximisation and plot
  plot(prcomp(input.matrix)$x, col=EM$cl,pch=3,main ="Expectation Maximisation" )

  #calculate analytics
  kmeanRand <- cluster.stats(dist.matrix, kmean$cluster, input$topic, compareonly = TRUE)
  print(kmeanRand)
  HARand <- cluster.stats(dist.matrix, HAcut, input$topic, compareonly = TRUE)
  print(HARand)
  EMRand <- cluster.stats(dist.matrix, EM$classification, input$topic, compareonly = TRUE)
  print(EMRand)
}