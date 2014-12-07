library(topicmodels)
library(ggplot2)


source("src/R/loadData.R")
source("src/R/dtm.R")


doTopicModelling <- function(speeches, corpus, topics=5, minTfIdf=0, termsFilename) {
  
  dtm <- createDtmFromCorpus(corpus, minTfIdf)
  print(paste0("doc-term matrix containing ", dtm$ncol, " terms."))  
  
  lda.model <- doLDA(dtm, topics, termsFilename)
  
  # assign a distribution over topics to the speeches
  speech.topics <- posterior(lda.model, dtm)
  # and find which topic is most likely for each speech
  majorityTopic <- apply(speech.topics$topics, 1, which.max)

  majorityTopicValue <- apply(speech.topics$topics, 1, max)
  
  # join the chosen topic onto the speech
  # only project the name, year and topic
  spWithTopics <- cbind(speeches, majorityTopic)[, c(1, 7, 9)]
  
  plotTopics(spWithTopics)
    
  #print(nrow(as.data.frame(speech.topics)))
  #print(length(majorityTopicValue))
  
  topicScores <- cbind(as.data.frame(speech.topics$topics), majorityTopicValue)
  colnames(topicScores)[topics+1] <- "majorityTopicScore"
  
  topicScores
}



doLDA <- function(dtm, topics=5, termsFilename) {
  lda.model = LDA(dtm, control = list(alpha = 0.1), k=topics)
  
  write.csv(terms(lda.model,20), termsFilename)
  
  lda.model
}


plotTopics <- function(spWithTopics) {
  spWithTopics$decade <- spWithTopics$subsequentYear - (spWithTopics$subsequentYear %% 10)
  
  #print(colnames(spWithTopics))
  
  g <- ggplot(spWithTopics) +
    geom_bar(aes(factor(decade), fill=factor(majorityTopic)))
  print(g)
}


createDtmFromCorpus <- function(corpus, minTfIdf=0.2) {
  
  dtmRaw <- DocumentTermMatrix(corpus)
  
  dtmTfIdf <- weightTfIdf(dtmRaw)
  keepTheseTerms <- findFreqTerms(dtmTfIdf, minTfIdf)
  
  dtm <- DocumentTermMatrix(corpus, control=list(dictionary=keepTheseTerms))
  dtm <- removeSparseTerms(dtm,0.7)
  dtm <- dtm[rowSums(as.matrix(dtm))>0,]
  
  dtm
}
