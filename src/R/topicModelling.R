library(topicmodels)


source("src/R/loadData.R")
source("src/R/dtm.R")


doTopicModelling <- function(dtm) {
  doLDA(dtm)
  }


doLDA <- function(dtm) {
  lda.model = LDA(dtm, 5)
  print (terms(lda.model,20))
  
  lda.model
}



