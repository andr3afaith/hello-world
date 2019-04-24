#------------------------------------------------------------------------------#
# ANN COULTER 4/8 practice sentiment analysis
#------------------------------------------------------------------------------#

##Import AOC
Coulter <- rio::import("/Volumes/ADATA UFD/Andrea_data_folder/Coulter.csv")

#Define characters
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
#
#
C_tweet_words <- Coulter %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

C_words <- C_tweet_words %>%
  count(word, source) %>%
  filter(sum(n) >= 5) %>%
  spread(source, n, fill = 0) %>%
  ungroup()

#Don't count as separate devices#
C_words <- C_tweet_words %>%
  count(word) %>%
  filter(sum(n) >= 5) %>%
  ungroup()

#Create a sentiment analysis table called nrc
nrc <- sentiments %>%
  filter(lexicon == "nrc") %>%
  dplyr::select(word, sentiment)

#------------------------------------------------------------------------------#  
#Count the number of words by screen_name:
#------------------------------------------------------------------------------#  
C_screen_name <- C_tweet_words %>%
  group_by(screen_name) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  distinct(user_id, screen_name, total_words)

#------------------------------------------------------------------------------#
# Create a Table of Words and Sentiments
#------------------------------------------------------------------------------#

C_sentiment_words <- C_tweet_words %>%
  count(word) %>%
  inner_join(nrc, by = "word") %>% 
  select(word, sentiment, n) %>% 
  arrange(desc(n))

#------------------------------------------------------------------------------#
#                 Summarizing Sentiments Based on AOC Screen Name
#------------------------------------------------------------------------------#

#don't forget to change inner_join(new_screen_name_chart)

C_total_sentiment <- C_tweet_words %>%
  inner_join(nrc, by = "word") %>%
  count(sentiment, user_id) %>%
  ungroup() %>%
  complete(sentiment, user_id, fill = list(n = 0)) %>%
  inner_join(C_screen_name) %>%
  group_by(screen_name, sentiment, total_words) %>%
  summarize(words = sum(n)) %>%
  arrange(desc(words)) %>% 
  ungroup()

#visualize 
ggplot(C_total_sentiment,
       aes(x=words, y=fct_reorder(sentiment, words, desc=TRUE))) +
  geom_point() +
  labs(x="Count of Words in Twitter Feed", y="Words", 
       title = "Sentiment In Coulter Twitter 2018-2019",
       subtitle = "Chart: 4-8-19",
       caption = "Data analysis by Andrea Johnson") +
  theme_wsj() +
  geom_text(aes(label=words), hjust=-.4) 