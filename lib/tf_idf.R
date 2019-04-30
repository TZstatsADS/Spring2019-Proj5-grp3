top_terms_by_topic_tfidf = function(text_df, text_column, group_column, plot = T) {
  group_column = enquo(group_column)
  text_column = enquo(text_column)
  
  words = text_df %>%
    unnest_tokens(word, !!text_column) %>%
    count(!!group_column, word) %>% 
    ungroup()
  
  total_words = words %>% 
    group_by(!!group_column) %>% 
    summarize(total = sum(n))
  
  words = left_join(words, total_words)
  
  tf_idf = words %>%
    bind_tf_idf(word, !!group_column, n) %>%
    select(-total) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))))
  
  if(plot == T){
    group_name <- quo_name(group_column)
    
    tf_idf %>% 
      group_by(!!group_column) %>% 
      top_n(10) %>% 
      ungroup %>%
      ggplot(aes(word, tf_idf, fill = as.factor(group_name))) +
      geom_col(show.legend = F) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(reformulate(group_name), scales = "free") +
      coord_flip()
  }else{
    return(tf_idf)
  }
}