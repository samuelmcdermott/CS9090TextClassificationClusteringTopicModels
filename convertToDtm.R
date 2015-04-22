convertToDtm <-function(input,n){
  require(tm)
  require(RWeka)
  corpus <- Corpus(DataframeSource(input))

  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus,removeWords,stopwords("english"))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus,PlainTextDocument)

  ngramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))


  dtm <-DocumentTermMatrix(corpus,control = list(tokenize = ngramTokenizer))
  dtm <- removeSparseTerms(dtm,0.99)
  return(dtm)
}