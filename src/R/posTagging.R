library(openNLP)
library(NLP)
library(parallel)

source("src/R/loadData.R")


doPosTagAnalysis <- function(speechesFile, partiesFile) {
  speeches <- loadSpeechData(speechesFile, partiesFile)
  
  speechTexts <- as.vector(base64decodeVec(as.character(speeches$b64Text), what="c"))
  
  threenessCounts <- mclapply(X=speechTexts, FUN=getThreenessCountFromText, mc.cores=3)
  
  speeches$threenessCount <- threenessCounts
  
  speeches
}


getThreenessCountFromText <- function(text) {
  posTags <- getPosTagList(text)
  posTagListCountThreeness(posTags)
}


getPosTagList <- function(s) {
  
  s <- as.String(s)

  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))

  pos_tag_annotator <- Maxent_POS_Tag_Annotator()

  posTaggedSentence <- annotate(s, pos_tag_annotator, a2)
  print(posTaggedSentence)
  df <- as.data.frame(posTaggedSentence)
  
  as.vector(unlist(df[ df$type == "word", "features"]))
}


posTagListCountThreeness <- function(posList) {
  
  # remove commas
  posTags <- posList [ ! posList == "," ]
  
  n <- length(posTags)
  k <- 4
#  candidates <- split(posTags, rep(1:ceiling(n/k), each=k)[1:n])
 
  candidates <- splitWithOverlap(posTags, k, k-1)
  length(which(isThreeness(candidates)))
}

isThreeness <- Vectorize(function(candidate) {
  if (length(candidate) == 4) {
    if (candidate[3] == "CC") {
      candidate[1] == candidate[2] & candidate[2] == candidate[4]
    } else {
      FALSE
    }
  } else {
    FALSE
  }
})

# from http://stackoverflow.com/questions/8872376/split-vector-with-overlapping-samples-in-r
splitWithOverlap <- function(vec, seg.length, overlap) {
  starts = seq(1, length(vec), by=seg.length-overlap)
  ends   = starts + seg.length - 1
  ends[ends > length(vec)] = length(vec)
  
  lapply(1:length(starts), function(i) vec[starts[i]:ends[i]])
}