preprocess<-function(){
  require(tm)
  require(slam)
  require(NLP)
  require(openNLP)
  require(openNLPdata)
  require(rJava)
  require(SnowballC)
  
  
  options(stringsAsFactors = FALSE)
  rt.raw <- read.csv(file="reuters.csv",header=T,sep=",")
  rt.df = NULL
  #cleaning and preprocessing
  topicColumns <-grep("topic",attributes(rt.raw)$names,ignore.case = TRUE, value = FALSE)
  for(i in 1:10){
    print(i)
    numTopics <- sum(rt.raw[i,topicColumns])
    if (numTopics > 0 && rt.raw$doc.text[i]!= ""){
      for(j in topicColumns){
        if(rt.raw[i,j] == 1){
          oldrow<-rt.raw[i,]
          newrow <- data.frame(oldrow$purpose,attributes(oldrow[j])$names,oldrow$doc.title,oldrow$doc.text)
          rt.df <-rbind(rt.df,newrow)
        }
      }
    }
  }
  
  names(rt.df)<- list("purpose","topic","title","text")
  rt.corpus <- Corpus(DataframeSource(data.frame(rt.df[,4])))
    # Step 1: Remove numeric characters [0-9]
    rt.corpus <- tm_map(rt.corpus, removeNumbers)
    
    # Step 2: Make all letters lowercase
    rt.corpus <- tm_map(rt.corpus, tolower)
    
    # Step 3: Remove stopwords (EN) from SMART (coincides with MC_tk list)
    rt.corpus <- tm_map(rt.corpus, removeWords, stopwords("SMART"))
    
    # Step 4: Replace punctuation with space (to handle intraword punct.)
    removePunctuationCustom <- function(x){    
      x <- gsub("[[:punct:]]+", " ", x)
    }
    rt.corpus <- tm_map(rt.corpus, removePunctuationCustom)
    
    # Step 5: Remove excess whitespace
    rt.corpus <- tm_map(rt.corpus, stripWhitespace)
  rt.corpus <- tm_map(rt.corpus, PlainTextDocument)
  return(list(rt.df[1:3],rt.corpus))
}
