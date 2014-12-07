library(tm)
library(tm.plugin.sentiment)
library(plyr)
library(ggplot2)
library(RColorBrewer)

source("src/R/loadData.R")
source("src/R/dtm.R")

doSentimentAnalysis <- function(speeches) {
  
  dtm <- createDTM(speeches)
  doSentimentAnalysisDTM(speeches, dtm)
}

doSentimentAnalysis <- function(speeches, dtm) {
  
  pol <- polarity(dtm)
  
  plotSentimentResults(speeches, pol)
}


plotSentimentResults <- function(speeches, pol, minYear=1950) {
  
  speeches$polarity <- pol
  
  speeches <- speeches[speeches$subsequentYear>minYear,]
  speeches <- droplevels(speeches)
  
  print(paste0("Average sentiment polarity: ", mean(pol)))
  
  #with(speeches, boxplot(polarity~party))
  g <- ggplot(speeches) + 
    geom_boxplot(aes(party, polarity)) +
    xlab("")
  print(g)
  
  with(speeches, t.test(polarity~party))
}

plotGdpVsPolarity <- function(speeches, gdpGrowth, minYear=0) {
  speechesColumns <- speeches[, c("name", "party", "subsequentYear", "polarity")]
  
  data <- merge(speechesColumns, gdpGrowth, by.x="subsequentYear", by.y="yearEnding")
  
  with(data[data$subsequentYear>minYear, ], plot(gdpGrowth~polarity))
}

plotApprovalVsPolarity <- function(speeches, approval, minYear=0) {
  speechesColumns <- speeches[, c("name", "party", "subsequentYear", "polarity")]
  
  approvalByYear <- getAvgApprovalByYear(approval)
  
  data <- merge(speechesColumns, approvalByYear, by.x="subsequentYear", by.y="year")
  
  data <- data[data$subsequentYear>minYear, ]
  
  print(paste0("Correlation of speech polarity and approval: ", with(data, cor(polarity, meanApproval))))
  
  with(data, plot(meanApproval~polarity))
}

getAvgApprovalByYear <- function(approval) {
  approval$year <- format(approval$end, "%Y")
  
  ddply(approval, c("name", "year"), summarise, meanApproval = mean(approve))
}

computeIndexedApprovalData <- function(speeches, approval, days) {
  sp <- speeches[,c("index", "date", "polarity")]
  app <- approval[, c("end", "approve")]
  
  # which speech precedes each approval rating?
  app$precedingSpeechIndex <- findPrecedingSpeechIndex(speeches, approval$end)
  
  data <- merge(sp,app, by.x="index", by.y="precedingSpeechIndex")
  
  colnames(data) <- c("speechIndex", "speechDate", "polarity", "approvalDate", "approval")
  
  data$daySinceSpeech <- as.numeric(data$approvalDate - data$speechDate)
  
  data <- data[data$daySinceSpeech<=100,]
  
  ddply(data, "speechIndex", transform, rebasedApproval = rebaseApproval(approvalDate, approval))
}


plotIndexedApprovalByDay <- function(speeches, approval, days=100, minSpeechIndex=0) {
  
  data <- computeIndexedApprovalData(speeches, approval, days)
  colnames(data
           )
  data <- data[ data$speechIndex > minSpeechIndex, ]
  
  midpoint <- mean(c(max(data$polarity), min(data$polarity)))
  print(midpoint)
  
  g <- ggplot(data) +
    geom_hline(yintercept=100) +
    geom_line(aes(daySinceSpeech, rebasedApproval, group=speechIndex, colour=polarity)) +
    scale_colour_gradient2(midpoint=midpoint, low="red", high="blue") +
    #scale_y_continuous(limits=c(0, 5000))
    coord_cartesian(ylim = c(50, 150)) +
    ylim(50,150) +
    xlab("Days after speech") +
    ylab("Approval (day of speech = 100)")
  g
}

getLatestApproval <- function(approvalDates, approvalRatings) {
  approvalRatings[approvalDates == max(approvalDates)]
}

checkApprovalCorrelationAfterTime <- function(speeches, approval, days=100) {
  data <- computeIndexedApprovalData(speeches, approval, days)
  
  data <- data[ data$daySinceSpeech > 90, ]
  
  latestApprovalRatings <- ddply(data, c("speechIndex", "polarity"), summarise, latestApproval = getLatestApproval(approvalDate, rebasedApproval))
  
  correl <- with(latestApprovalRatings, cor(polarity, latestApproval))
  print(paste0("Correlation of speech polarity and 90-100 day rebased approval rating: ", correl))
  
  latestApprovalRatings
}

findPrecedingSpeechIndex <- Vectorize(function(speeches, measurementDate) {
  
  latestEarlierDate <- max(speeches[speeches$date<measurementDate,"date"])
  speeches[speeches$date == latestEarlierDate, "index"]
},
                                      "measurementDate")


rebaseApproval <- function(approvalDate, approval) {
  day1 <- min(approvalDate)
  factor <- 100 / approval[approvalDate == day1]

  approval * factor[1] # just in case there are duplicate day1s
}