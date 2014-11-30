library(topicmodels)
library(ggplot2)


source("src/R/loadData.R")
source("src/R/dtm.R")


doTopicModelling <- function(speeches, corpus, topics=5) {
  
  dtm <- createDtmFromCorpus(corpus)
  
  lda.model <- doLDA(dtm, topics)
  
  # assign a distribution over topics to the speeches
  speech.topics <- posterior(lda.model, dtm)
  # and find which topic is most likely for each speech
  majorityTopic <- apply(speech.topics$topics, 1, which.max)

  # join the chosen topic onto the speech
  # only project the name, year and topic
  spWithTopics <- cbind(speeches, majorityTopic)[, c(1, 7, 10)]
  
  plotTopics(spWithTopics)
    
  speech.topics
  }


doLDA <- function(dtm, topics=5) {
  lda.model = LDA(dtm, control = list(alpha = 0.1), k=topics)
  print (terms(lda.model,20))
  
  lda.model
}


plotTopics <- function(spWithTopics) {
  spWithTopics$decade <- spWithTopics$subsequentYear - (spWithTopics$subsequentYear %% 10)
  
  print(colnames(spWithTopics))
  
  g <- ggplot(spWithTopics) +
    geom_bar(aes(factor(decade), fill=factor(majorityTopic)))
  print(g)
}


createDtmFromCorpus <- function(corpus) {
  
  dtmRaw <- DocumentTermMatrix(corpus)
  
  dtmTfIdf <- weightTfIdf(dtmRaw)
  keepTheseTerms <- findFreqTerms(dtmTfIdf, 0, 0.3)
  
  dtm <- DocumentTermMatrix(corpus, control=list(dictionary=keepTheseTerms))
  dtm <- removeSparseTerms(dtm,0.7)
  dtm <- dtm[rowSums(as.matrix(dtm))>0,]
  
  dtm
}
