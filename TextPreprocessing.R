preprocess<-function(input.filename){

  options(stringsAsFactors = FALSE) #a default option that we need to change

  #-----------------------------------#
  #  INPUT DATA                       #
  #-----------------------------------#
  print("Reading in data")
  #The 10 most populus classes, and the ones we'll use for evaluation
  populus = c("topic.earn","topic.acquisitions",  "topic.money-fx",  "topic.grain",	"topic.crude",	"topic.trade",	"topic.interest",	"topic.ship",	"topic.wheat",	"topic.corn")  #make sure we don't use factors for strings as default

  #get in the data
  input.raw <- read.csv(file=input.filename,header=T,sep=",")

  #sample for testing purposes
  #rt.raw <- rt.raw[sample(1:nrow(rt.raw),2000,replace=FALSE),]

  #this will hold everything we're outputting
  output.df = NULL

  #-----------------------------------#
  #  ORGANSE TOPIC COLUMNS AND ROWS   #
  #-----------------------------------#

  #find the columns that identify the topics
  topicColumns <-grep("topic",attributes(rt.raw)$names,ignore.case = TRUE, value = FALSE)

  for(i in 1:nrow(input.raw)){
    #Find the number of topics associated with this document
    numTopics <- sum(input.raw[i,topicColumns])
    #if this document has topics associated and contains text (otherwise this document will get dropped)
    if (numTopics > 0 && input.raw$doc.text[i]!= ""){
      for(j in topicColumns){
        if(input.raw[i,j] == 1){
          #take each row and create a new document for each topic, and use the actual name of the topic
          oldrow<-input.raw[i,]
          newrow <- data.frame(attributes(oldrow[j])$names,oldrow$doc.title,oldrow$doc.text)
          #add this row
          output.df <-rbind(output.df,newrow)
        }
      }
    }
  }

  names(output.df)<- list("topic","title","text")

  #choose the documents that have the 10 most popular topics
  output.df <- subset(output.df, subset = topic %in% populus)
  #shuffle up the instances for bias free k fold
  output.df <- output.df[sample(1:nrow(rt.df),size=nrow(rt.df),replace=FALSE),]
  #we want the topic to be a factor
  output.df$topic <- as.factor(output.df$topic)
  #return a dataframe with the topic, title and text for the correct documents
  return(output.df)
}
