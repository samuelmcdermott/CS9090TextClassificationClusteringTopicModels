convertToDtm <-function(input,n){
  require(tm)
  require(RWeka)
  #make the input a corpus (dataframesource is so we can use multiple columns)
  corpus <- Corpus(DataframeSource(input))

  corpus <- tm_map(corpus, tolower) #all lowercase
  corpus <- tm_map(corpus,removeWords,stopwords("english")) #remove stopwords
  corpus <- tm_map(corpus, removePunctuation) #remove punctuation
  corpus <- tm_map(corpus, stemDocument) #stem the document
  corpus <- tm_map(corpus, removeNumbers) #remove numbers
  corpus <- tm_map(corpus, stripWhitespace) #remove extra whitespace
  corpus <- tm_map(corpus,PlainTextDocument) #fix formatting cos it breaks somewhere above

  #make the ngram function, where n is inputted by the user
  ngramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n))
  #make corpus into document term matrix, with ngrams
  dtm <-DocumentTermMatrix(corpus,control = list(tokenize = ngramTokenizer))
  #remove the sparse terms, best set between 0.95 and 0.99
  dtm <- removeSparseTerms(dtm,0.99)
  #return this document term matrix
  return(dtm)
}