
loadSpeechData <- function(speechesFilename, partiesFilename) {
  speeches <- read.csv(speechesFilename, header=F)
  colnames(speeches) <- c("title", "name", "dateStr", "b64Text")
  
  speeches$index <- c(1:nrow(speeches))
  
  # a table of name, party
  parties <- read.csv(partiesFilename)
  
  # parse the dates properly
  speeches$date <- as.Date(speeches$dateStr, format="%B %d, %Y")
  speeches$subsequentYear <- subsequentYear(speeches$date)
    
  speeches <- merge(speeches, parties, all.x=T)
}

loadApprovalData <- function(filename) {
  approval <- read.csv(filename)
  
  approval$start <- as.Date(approval$start, format="%m/%d/%Y")
  approval$end <- as.Date(approval$end, format="%m/%d/%Y")
  
  approval
}

loadGdpGrowthData <- function(filename) {
  gdp <- read.csv(filename)
  
  gdp$yearEnding <- format(as.Date(gdp$yearEnding, format="%b %d, %Y"), "%Y")
  
  gdp
}

# given a date, which is either december of january, what is the subsequent year
subsequentYear <- Vectorize(function(date) {
  if (as.numeric(format(date, "%m")) > 6) {
    as.numeric(format(date, "%Y")) + 1
  } else {
    as.numeric(format(date, "%Y"))
  }
})