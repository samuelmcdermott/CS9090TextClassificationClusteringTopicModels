preprocess<-function(){
  require(tm)
  require(slam)
  require(NLP)
  require(openNLP)
  require(openNLPdata)
  require(rJava)
  require(SnowballC)
  
  #make sure we don't use factors for strings as default
  #options(stringsAsFactors = FALSE)
  rt.raw <- read.csv(file="reuters.csv",header=T,sep=",")
  rt.raw <- rt.raw[sample(1:nrow(rt.raw),2000,replace=FALSE),]
  rt.df = NULL
  #cleaning and preprocessing
  topicColumns <-grep("topic",attributes(rt.raw)$names,ignore.case = TRUE, value = FALSE)
  for(i in 1:nrow(rt.raw)){
    print(i)
    numTopics <- sum(rt.raw[i,topicColumns])
    if (numTopics > 0 && rt.raw$doc.text[i]!= ""){
      for(j in topicColumns){
        if(rt.raw[i,j] == 1){
          oldrow<-rt.raw[i,]
          newrow <- data.frame(attributes(oldrow[j])$names,oldrow$doc.title,oldrow$doc.text)
          rt.df <-rbind(rt.df,newrow)
        }
      }
    }
  }
  #shuffle up the instances for bias free k fold
  rt.df <- rt.df[sample(1:nrow(rt.df),size=nrow(rt.df),replace=FALSE),]
  names(rt.df)<- list("topic","title","text")
 rt.df$topic <- as.factor(rt.df$topic)
  return(rt.df)
}
