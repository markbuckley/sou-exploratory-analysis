library(tm)
library(caTools)   # provides base64 decoding
library(SnowballC) # provides stemming


tm_tag_score <- tm_term_score

base64decodeVec <- Vectorize(base64decode, c("z"))

createDTMFromFiles <- function(speechesFile, partiesFile) {
  speeches <- loadSpeechData(speechesFile, partiesFile)
  
  createDTM(speeches)
}

createDTM <- function(speeches) {
  speechTexts <- as.vector(base64decodeVec(as.character(speeches$b64Text), what="c"))
  
  print (length(speechTexts))
  
  corpus <- Corpus(DataframeSource(as.data.frame(speechTexts)))
  
  extraStopwords <- c("will", "can", "may", "must", "made", "one", "let")
  
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords("english"), extraStopwords))
  
  dtm <- DocumentTermMatrix(corpus)
  
  dtm <- removeSparseTerms(dtm,0.9)
  dtm <- dtm[rowSums(as.matrix(dtm))>0,]
  
  #  dtm <- weightTfIdf(dtm)
  dtm

}