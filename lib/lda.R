words = c("guest", "guests", "room", "rooms", "bed", "beds", "bedroom", "bedrooms", "night", "nights", "stay", "place", "enterance", "apartment")
getCorpus = function(text) {
  corpus = VCorpus(VectorSource(text)) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(stripWhitespace) %>%
    tm_map(removeWords, stopwords("en")) %>%
    tm_map(removeWords, stopwords("SMART")) %>%
    tm_map(removeWords, words)
  return(corpus)
}

top_terms_by_topic_LDA = function(text, plot = T, number_of_topics = 4) {    
  corpus = getCorpus(text)
  DTM = DocumentTermMatrix(corpus) 
  
  unique_indexes = unique(DTM$i) 
  DTM = DTM[unique_indexes,] 
  
  lda = LDA(DTM, k = number_of_topics, method = "Gibbs", control = list(iter = 2000,  thin = 400, nstart = 5, best = T, seed = list(1234, 423, 2211, 1122, 3345)))
  topics = tidy(lda, matrix = "beta")
  
  top_terms = topics %>% 
    group_by(topic) %>% 
    top_n(10, beta) %>% 
    ungroup() %>% 
    arrange(topic, -beta)
  
  if(plot == T){
    top_terms %>% 
      mutate(term = reorder(term, beta)) %>% 
      ggplot(aes(term, beta, fill = factor(topic))) + 
      geom_col(show.legend = FALSE) + 
      facet_wrap(~ topic, scales = "free") + 
      labs(x = NULL, y = "Topic concentration (beta)") + 
      coord_flip()
  }else{ 
    return(top_terms)
  }
}