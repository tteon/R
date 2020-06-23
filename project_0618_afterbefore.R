library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)

# frequency

# before_trump -> 0801~1231
# after_trump -> 0101~0618 

a_trump <- read_csv("after_trump.csv")
b_trump <- read_csv('before_trump.csv')
head(a_trump)
head(b_trump)

# combining for comparison

tweets <- bind_rows(a_trump %>% 
                      mutate(person = "a_trump"),
                    b_trump %>% 
                      mutate(person = "b_trump"))
tweets

library(tidytext)
library(stringr)

# preprocessing

remove_reg <- "&amp;|&lt;|&gt;"
tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))
tidy_tweets

# catching the frequency in text

frequency <- tidy_tweets %>% 
  group_by(person) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_tweets %>% 
              group_by(person) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency

# arrange and observe that result
library(tidyr)

frequency <- frequency %>% 
  select(person, word, freq) %>% 
  spread(person, freq) %>%
  arrange(a_trump, b_trump)

frequency

# frequency visualization by x-axis,y-axis
# b_trump , before / a_trump , after

library(scales)
ggplot(frequency, aes(a_trump, b_trump)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

a_trump
b_trump
#
'''
tidy_tweets <- tidy_tweets %>%
  filter(timestamp >= as.Date("2016-01-01"),
         timestamp < as.Date("2017-01-01"))
'''
#version 1 a_trump
#Notice the word 'coronavirus'
word_ratios_a <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, person) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(person, n, fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(a_trump / b_trump)) %>%
  arrange(desc(logratio))

word_ratios_a

#version 2 b_trump
#Notice the word 'turkey'
word_ratios_b <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, person) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(person, n, fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log( b_trump / a_trump )) %>%
  arrange(desc(logratio))

word_ratios_b

# Visualization
# red - before , blue - after 

word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio (After/Before)") +
  scale_fill_discrete(name = "", labels = c("a_trump", "b_trump"))

### ok 

# sentiments datsets 

library(tidytext)

# Sentiment 

sentiments

# -5~5 , based on unigrams
get_sentiments("afinn")
# pos / neg , binary  
get_sentiments("bing")
# have much more representation for expression , binary
get_sentiments("nrc")

library(dplyr)
library(stringr)

#

nrcjoy <- get_sentiments('nrc') %>%
  filter(sentiment == 'joy')

nrc$sentiment

tidy_tweets %>%
  filter(person == 'a_trump') %>%
  inner_join(nrcjoy) %>%
  count(word, sort = T)

aa_trump <- tidy_tweets %>%
  filter(person == 'a_trump')



#

tidy_tweets %>%
  filter(person == 'b_trump') %>%
  inner_join(nrcjoy) %>%
  count(word, sort = T)

bb_trump <- tidy_tweets %>%
  filter(person == 'b_trump')

bb_trump

# pos / neg / top10 / words

bing_word_counts_a <- aa_trump %>%
  inner_join(get_sentiments('bing')) %>%
  count(word, sentiment, sort = T) %>%
  ungroup()

bing_word_counts_a

#

bing_word_counts_b <- bb_trump %>%
  inner_join(get_sentiments('bing')) %>%
  count(word, sentiment, sort = T) %>%
  ungroup()

bing_word_counts_b

#

bing_word_counts_a %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#
bing_word_counts_b %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


# word cloud

library(wordcloud)


aa_trump %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

bb_trump %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


#
library(reshape2)

aa_trump %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

bb_trump %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

# tf-idf

library(dplyr)
library(tidytext)

a_trump

a_trump_words <- a_trump %>%
  unnest_tokens(word, text) %>%
  count(created_at, word, sort = TRUE) %>%
  mutate(month = lubridate::month(mdy_hms(a_trump_words$created_at)))

a_trump_words

a_trump_total_words <- a_trump_words %>% 
  group_by(month) %>% 
  summarize(total = sum(n))

a_trump_total_words

a_trump_words <- left_join(a_trump_words , a_trump_total_words)

#

b_trump$created_at
tail(b_trump)

b_trump_words <- b_trump %>%
  unnest_tokens(word, text) %>%
  count(created_at, word, sort = TRUE) %>%
  mutate(month = lubridate::month(mdy_hms(b_trump_words$created_at)))

b_trump_words

b_trump_total_words <- b_trump_words %>% 
  group_by(month) %>% 
  summarize(total = sum(n))

b_trump_total_words

b_trump_words <- left_join(b_trump_words , b_trump_total_words)

b_trump_words

# Error code
library(ggplot2)

ggplot(a_trump_words, aes(n/total, fill = n)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~month, scales = "free_y")

# tfidf

a_trump_words

a_trump_words <- a_trump_words %>%
  bind_tf_idf(word, month , n)

a_trump_words

#
b_trump_words

b_trump_words <- b_trump_words %>%
  bind_tf_idf(word, month , n)

b_trump_words

# tfidf , desc , -select total, created_at

a_trump_words <- a_trump_words %>%
  select(-total,-created_at) %>%
  arrange(desc(tf_idf))

str(a_trump_words)

#

b_trump_words <- b_trump_words %>%
  select(-total,-created_at) %>%
  arrange(desc(tf_idf))

str(b_trump_words)
b_trump_words
#

?lubridate
?POSIXct

# month categorical
a_trump_words %>%
  mutate(month = lubridate::month(mdy_hms(a_trump_words$created_at)))

str(a_trump_words)

# ERROR CODE
a_trump_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(month) %>% 
  top_n(5) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = word)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~month, ncol = 2, scales = "free") +
  coord_flip()

b_trump_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(month) %>% 
  top_n(5) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = word)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~month, ncol = 2, scales = "free") +
  coord_flip()

# tf-idf ploting

a_trump_words

plot_a_trump <- a_trump_words %>%
  bind_tf_idf(word, month, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = fct_reorder(word, tf_idf))

plot_a_trump %>% 
  group_by(month) %>% 
  top_n(5, tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = n)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~month, ncol = 2, scales = "free") +
  coord_flip()

plot_a_trump

#

b_trump_words

plot_b_trump <- b_trump_words %>%
  bind_tf_idf(word, month, n) %>%
  arrange(desc(tf_idf)) 

plot_b_trump %>% 
  group_by(month) %>% 
  top_n(5, tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = n)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~month, ncol = 2, scales = "free") +
  coord_flip()

plot_b_trump


# tunning

mystopwords <- data_frame(word = c("jeanne" , "iraqis", "steven", "dhs", "verily", "thom" ,"richer", "kay", "farce", "jeb", "steinbrenner", "mutiny", "mack", "cindy", "chera" ," clark", "3.8b", "57", "reschenthaler", "marieleff", "churchill",
                                   "zelensky", "kirk", " jentezen", "sammy" ," aqap", "jodey", "fdny", "34", "vixmzwgym2", "todqiaiq54", "negnlaswmt", "e08ngbcw3d","185","juaquin","joaquin","qs","ltc"))

# stopwords
a_trump_words <- anti_join(a_trump_words, mystopwords, by = "word")
b_trump_words <- anti_join(b_trump_words, mystopwords, by = "word")
# failure to delete unnessary words
plot_b_trump %>% 
  group_by(month) %>% 
  top_n(5, tf_idf) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = n)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~month, ncol = 2, scales = "free") +
  coord_flip()

plot_a_trump

# n-gram

a_trump

a_trump_bigrams <- a_trump %>%
  mutate(month = lubridate::month(mdy_hms(a_trump$created_at))) %>%
  unnest_tokens(bigram, text, token = 'ngrams', n=2)

a_trump_bigrams <- a_trump_bigrams %>%
  select(-created_at)

a_trump_bigrams

a_trump_bigrams %>%
  count(bigram, month ,sort = T)

# n-gram , before version

b_trump 

b_trump_bigrams <- b_trump %>%
  mutate(month = lubridate::month(mdy_hms(b_trump$created_at))) %>%
  unnest_tokens(bigram, text, token = 'ngrams', n=2)
  

b_trump_bigrams <- b_trump_bigrams %>%
  select(-created_at)

b_trump_bigrams

b_trump_bigrams %>%
  count(bigram, month ,sort = T)

# separate

library(tidyr)

a_trump_bigrams_separated <- a_trump_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
a_trump_bigrams_separated
# preprocessing first challenge
a_trump_bigrams_filtered <- a_trump_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# checking the result
a_trump_bigram_counts <- a_trump_bigrams_filtered %>%
  count(word1, word2, sort = T)

a_trump_bigram_counts

#

b_trump_bigrams_separated <- b_trump_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
b_trump_bigrams_separated
# preprocessing first challenge
b_trump_bigrams_filtered <- b_trump_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# checking the result
b_trump_bigram_counts <- b_trump_bigrams_filtered %>%
  count(word1, word2, sort = T)

b_trump_bigram_counts

# preprocessing second challenge
# add the stopwords dictionary
mystopwords <- data_frame(word = c("https","t.co","rt"))

b_trump_bigrams_filtered <- b_trump_bigrams_separated %>%
  filter(!word1 %in% mystopwords$word) %>%
  filter(!word2 %in% mystopwords$word)

b_trump_bigram_counts <- b_trump_bigrams_filtered %>%
  count(word1, word2, sort = T)

b_trump_bigram_counts



# using unite function 

a_trump_bigrams_united <- a_trump_bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

a_trump_bigrams_united

#

b_trump_bigrams_united <- b_trump_bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

b_trump_bigrams_united

# trigram analytics

a_trump %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word,
         !word1 %in% mystopwords$word,
         !word2 %in% mystopwords$word,
         !word3 %in% mystopwords$word) %>%
  count(word1, word2, word3, sort = TRUE)

b_trump %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word,
         !word1 %in% mystopwords$word,
         !word2 %in% mystopwords$word,
         !word3 %in% mystopwords$word) %>%
  count(word1, word2, word3, sort = TRUE)

# bigram analytics
a_trump_bigrams_filtered

a_trump_bigrams_filtered %>%
  filter(word2 == "coronavirus") %>%
  count(month, word1, sort = T)

a_trump_bigram_tf_idf <- a_trump_bigrams_united %>%
  count(month, bigram) %>%
  bind_tf_idf(bigram, month, n) %>%
  arrange(desc(tf_idf))

a_trump_bigram_tf_idf

a_trump_bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  group_by(month) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, fill=n)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ month, ncol = 2, scales = "free") +
  coord_flip() +
  labs(y = "tf-idf of bigram",
       x = "")

#
b_trump_bigrams_filtered

b_trump_bigrams_filtered %>%
  filter(word2 == "coronavirus") %>%
  count(month, word1, sort = T)

b_trump_bigram_tf_idf <- b_trump_bigrams_united %>%
  count(month, bigram) %>%
  bind_tf_idf(bigram, month, n) %>%
  arrange(desc(tf_idf))

b_trump_bigram_tf_idf

b_trump_bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  group_by(month) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, fill=n)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ month, ncol = 2, scales = "free") +
  coord_flip() +
  labs(y = "tf-idf of bigram",
       x = "")

# bigram _ sentiment

a_trump_bigrams_separated %>%
  filter(word1 == 'not') %>%
  count(word1, word2, sort = T)

# inspect neg words by using 'AFINN' dictionary

AFINN <- get_sentiments("afinn")

AFINN

a_trump_not_words <- a_trump_bigrams_separated %>%
  filter(word1 == 'not') %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = T) %>%
  ungroup()

a_trump_not_words
 
# 

a_trump_not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment value * number of occurrences") +
  coord_flip()

#

b_trump_not_words <- b_trump_bigrams_separated %>%
  filter(word1 == 'not') %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = T) %>%
  ungroup()

b_trump_not_words

# 

b_trump_not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment value * number of occurrences") +
  coord_flip()

# graph

library(igraph)

a_trump_bigram_counts

a_trump_bigram_graph <- a_trump_bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

a_trump_bigram_graph

#

b_trump_bigram_counts

b_trump_bigram_graph <- b_trump_bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

b_trump_bigram_graph


#
library(ggraph)
set.seed(2020)

ggraph(a_trump_bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

ggraph(a_trump_bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 2, hjust = 2)

#

ggraph(b_trump_bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

ggraph(b_trump_bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 2, hjust = 2)


# plus aesthetic factor
# using end cap option

set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(a_trump_bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#

ggraph(b_trump_bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# specific month and then what happen these words?

a_trump_month_words <- a_trump %>%
  mutate(month = lubridate::month(mdy_hms(a_trump$created_at))) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  select(-created_at)

a_trump_month_words

#

b_trump_month_words <- b_trump %>%
  mutate(month = lubridate::month(mdy_hms(b_trump$created_at))) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  select(-created_at)

b_trump_month_words

#
library(widyr)

a_trump_word_pairs <- a_trump_month_words %>%
  pairwise_count(word, month, sort =T )

str(a_trump_word_pairs)
head(a_trump_word_pairs)
tail(a_trump_word_pairs)

a_trump_word_pairs %>%
  filter(item1 == "trump")

a_trump_word_pairs %>%
  filter(item1 == "coronavirus")

a_trump_word_pairs %>%
  filter(item1 == "turkey")

a_trump_word_pairs %>%
  filter(item1 == "obama")

a_trump_word_pairs %>%
  filter(item1 == "biden")

a_trump_word_pairs %>%
  filter(item1 == "wuhan")

#
b_trump_word_pairs <- b_trump_month_words %>%
  pairwise_count(word, month, sort =T )

str(a_trump_word_pairs)
head(a_trump_word_pairs)
tail(a_trump_word_pairs)

b_trump_word_pairs %>%
  filter(item1 == "trump")

b_trump_word_pairs %>%
  filter(item1 == "coronavirus")

b_trump_word_pairs %>%
  filter(item1 == "turkey")

b_trump_word_pairs %>%
  filter(item1 == "obama")

b_trump_word_pairs %>%
  filter(item1 == "biden")

b_trump_word_pairs %>%
  filter(item1 == "wuhan")

# correlation inspect
a_trump_month_words
tail(a_trump_month_words)

a_trump_word_cors <- a_trump_month_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, month, sort = T)

a_trump_word_cors


a_trump_section_words <- a_trump %>%
  mutate(month = lubridate::month(mdy_hms(a_trump$created_at))) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  select(-created_at)

a_trump_word_pairs <- a_trump_section_words %>%
  pairwise_count(word, month, sort = T)

a_trump_word_cors <- a_trump_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, month, sort = T)

a_trump_word_cors %>%
  filter(item2 == 'coronavirus')

a_trump_word_cors %>%
  filter(item1 %in% c("covid")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

#
b_trump_month_words
tail(a_trump_month_words)

b_trump_word_cors <- b_trump_month_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, month, sort = T)

b_trump_word_cors


b_trump_section_words <- b_trump %>%
  mutate(month = lubridate::month(mdy_hms(b_trump$created_at))) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  select(-created_at)

b_trump_word_pairs <- b_trump_section_words %>%
  pairwise_count(word, month, sort = T)

b_trump_word_cors <- b_trump_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, month, sort = T)

b_trump_word_cors %>%
  filter(item2 == 'coronavirus')

b_trump_word_cors %>%
  filter(item1 %in% c("turkey","syria","oil")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

























