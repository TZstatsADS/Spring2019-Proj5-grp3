get.dtm <- function(text.data, unique.only = F){
  corpus = VCorpus(VectorSource(text.data))
  dtm = DocumentTermMatrix(corpus, control = list(
    tolower = TRUE,
    removeNumbers = TRUE,#we aren't concerned with informativeness, so few clearning options
    stopwords = TRUE,
    removeNumbers = TRUE, 
    removePunctuation = TRUE,
    stripWhitespace = TRUE))
  if (unique.only == T){
    non_zero_entries = unique(DTM$i) #omits zero entries
    dtm = dtm[non_zero_entries,]
  }
  return(dtm)
}