library(openNLP)
library(NLP)
library(parallel)
library(ggplot2)

source("src/R/loadData.R")
source("src/R/dtm.R")

doPosTagAnalysis <- function(speechesFile, partiesFile, parallel=F) {
  speeches <- loadSpeechData(speechesFile, partiesFile)
 
#  speeches <- speeches[1:6, ]
 
  speechTexts <- as.vector(base64decodeVec(as.character(speeches$b64Text), what="c"))
  
  print("finished reading in data")

  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()

  if (parallel) {
    cores <- 6
    l <- length(speechTexts)
    splits <- split(speechTexts, rep(1:cores, each=ceiling(l/cores))[1:l])

    threeCountVec <- Vectorize(getThreenessCountFromText, "text")

    threenessCounts <- mclapply(X=splits, FUN=function(x) {
        threeCountVec(x, sent_token_annotator, word_token_annotator, pos_tag_annotator)
      }, mc.cores=cores)
 
    threenessCounts
 
  } else {
    threenessCounts <- lapply(X=speechTexts, FUN=function(x) {
      getThreenessCountFromText(x, sent_token_annotator, word_token_annotator, pos_tag_annotator)
    })

    threenessCounts
  }

#  speeches$threenessCount <- threenessCounts
  
#  speeches
}


getThreenessCountFromText <- function(text, sent_token_annotator, word_token_annotator, pos_tag_annotator) {
  print("*")
  posTags <- getPosTagList(text, sent_token_annotator, word_token_annotator, pos_tag_annotator)
  posTagListCountThreeness(posTags)
}


getPosTagList <- function(s, sent_token_annotator, word_token_annotator, pos_tag_annotator) {
  
  s <- as.String(s)

#  sent_token_annotator <- Maxent_Sent_Token_Annotator()
#  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))

#  pos_tag_annotator <- Maxent_POS_Tag_Annotator()

  posTaggedSentence <- annotate(s, pos_tag_annotator, a2)
#  print(posTaggedSentence)
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

#speeches <- loadSpeechData("data/speeches.csv", "data/parties.csv")
#threeness <- read.csv("results//threeCounts.csv")
plotThreenessData <- function(speeches, threeness) {
  colnames(threeness) <- c("index", "count")
  data <- as.data.frame(cbind(speeches, threeness))
  
  data$decade <- data$subsequentYear - (data$subsequentYear %% 10)
  
  print(colnames(data))
  
  #d <- ddply(data, "decade", summarise, count = mean(count))
  
  g <- ggplot(data) +
    geom_line(aes(subsequentYear, count))
  g
}
