library(jsonlite)
metadata <- fromJSON("https://data.nasa.gov/data.json")
names(metadata$dataset)

class(metadata$dataset$title)
class(metadata$dataset$description)

# keywords are stored by vector of list format
class(metadata$dataset$keyword)

library(dplyr)

str(metadata$dataset)

str(metadata$dataset$editor)
a = seq_len(27099)
a

nasa_title <- data_frame(title = metadata$dataset$title, id=a)
nasa_title
nasa_title <- NULL

nasa_desc <- data_frame(id=a, desc = metadata$dataset$description)

nasa_desc %>%
  select(desc) %>%
  sample_n(5)

library(tidyr)

nasa_keyword <- data_frame(id = a , keyword = metadata$dataset$keyword) %>%
  unnest(keyword)

nasa_keyword


library(tidytext)
nasa_title <- nasa_title %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words)

nasa_desc <- nasa_desc %>%
  unnest_tokens(word, desc) %>%
  anti_join(stop_words)

nasa_title
nasa_desc

#

nasa_title %>%
  count(word, sort =T )
# desc -> not descend , yes description
nasa_desc %>%
  count(word, sort =T )

# tunning
my_stopwords <- data_frame(word = c(as.character(1:5), "ii", "1", "2" ,"data", "v1.0"))


nasa_title <- nasa_title %>% anti_join(my_stopwords)
nasa_desc <- nasa_desc %>% anti_join(my_stopwords)

nasa_keyword %>%
  group_by(keyword) %>%
  count(sort = T)

# co-occurance

library(widyr)

title_word_pairs <- nasa_title %>%
  pairwise_count(word, id, sort= T , upper =F)
title_word_pairs

desc_word_pairs <- nasa_desc %>%
  pairwise_count(word, id, sort= T , upper =F)
desc_word_pairs


library(ggplot2)
library(igraph)
library(ggraph)

# title field
set.seed(1234)
title_word_pairs %>%
  filter(n >= 250) %>%
  graph_from_data_frame() %>%
  ggraph(layout = 'fr') +
  geom_edge_link(aes(edge_alpha = n , edge_width = n), edge_colour = 'cyan4') +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = T,
                 point.padding = unit(0.2, 'lines')) +
  theme_void()

# desc field
set.seed(1234)
desc_word_pairs %>%
  filter(n >= 5000) %>%
  graph_from_data_frame() %>%
  ggraph(layout = 'fr') +
  geom_edge_link(aes(edge_alpha = n , edge_width = n), edge_colour = 'darkred') +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = T,
                 point.padding = unit(0.2, 'lines')) +
  theme_void()

# keyword connection 

keyword_pairs <- nasa_keyword %>%
  pairwise_count(keyword, id, sort = T, upper = F)

keyword_pairs

set.seed(1234)
keyword_pairs %>%
  filter(n >= 700) %>%
  graph_from_data_frame() %>%
  ggraph(layout = 'fr') +
  geom_edge_link(aes(edge_alpha = n , edge_width = n), edge_colour = 'royalblue') +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = T,
                 point.padding = unit(0.2, 'lines')) +
  theme_void()

# correlation

keyword_cors <- nasa_keyword %>%
  group_by(keyword) %>%
  filter(n() >= 50) %>%
  pairwise_cor(keyword, id, sort = T, upper = F)

keyword_cors
set.seed(1234)
keyword_cors %>%
  filter(correlation > .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = 'fr') +
  geom_edge_link(aes(edge_alpha = correlation , edge_width = correlation), edge_colour = 'royalblue') +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = T,
                 point.padding = unit(0.2, 'lines')) +
  theme_void()


set.seed(1234)
keyword_cors %>%
  filter(correlation > .8) %>%
  graph_from_data_frame() %>%
  ggraph(layout = 'fr') +
  geom_edge_link(aes(edge_alpha = correlation , edge_width = correlation), edge_colour = 'royalblue') +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = T,
                 point.padding = unit(0.2, 'lines')) +
  theme_void()

# tf-idf

desc_tf_idf <- nasa_desc %>%
  count(id, word, sort = T) %>%
  ungroup() %>%
  bind_tf_idf(word, id, n)

desc_tf_idf %>%
  arrange(-tf_idf) %>%
  select(-id)

# description field connect the keyword field

desc_tf_idf <- full_join(desc_tf_idf, nasa_keyword, by = 'id')
desc_tf_idf %>%
  filter(!near(tf, 1)) %>% # remember !near function 
  filter(keyword %in% c("nucleic acid extraction", "forest science",
                        "knowledge", "sharing",
                        "atmospheric science", "climate", "cloud")) %>%
  arrange(desc(tf_idf)) %>%
  group_by(keyword) %>%
  distinct(word, keyword, .keep_all = T) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% # rev function ?rev reverse
  ggplot(aes(word, tf_idf, fill = keyword)) +
  geom_col(aes(word, tf_idf, fill = keyword)) +
  geom_col(show.legend = F) +
  facet_wrap(~keyword, ncol = 3, scales = 'free') +
  coord_flip() +
  labs(title = 'Hightes tf-idf words in NASA metadata description fields',
       caption = 'NASA metadata from https://data.nasa.gov/data.json',
       x = NULL, y= "tf-idf")
  
my_stop_words <- bind_rows(stop_words, data_frame(word = c("1.1", "0", "1a" , "1.5", "1.0", "20a", "16s" ," ocko", "nbsp", "gt", "lt", "nbsp" ,"93", "td" ,"li","em", lexicon = rep("custom", 30))))

word_counts <- nasa_desc %>%
  anti_join(my_stop_words) %>%
  count(id, word, sort =T) %>%
  ungroup()

word_counts

desc_dtm <- word_counts %>%
  cast_dtm(id, word, n)

desc_dtm

library(topicmodels)

desc_lda <- LDA(desc_dtm, k = 24, control = list(seed = 1234))

desc_lda

tidy_lda <- tidy(desc_lda)

tidy_lda

# beta means the target words of probaility about belong at the speicfic group from target topics

top_terms <- tidy_lda %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms


# 

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%
  arrange(desc(beta)) %>%
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"),
                       levels = rev(paste(term, topic, sep = "__")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend =F) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top 10 terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 4, scales = 'free')


#
top_terms_2 <- tidy_lda %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_2

top_terms_2 %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%
  arrange(desc(beta)) %>%
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"),
                       levels = rev(paste(term, topic, sep = "__")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend =F) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top 10 terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 4, scales = 'free')

# measure by gamma matrix

lda_gamma <- tidy(desc_lda, matrix = "gamma")

lda_gamma

ggplot(lda_gamma, aes(gamma)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = 'Distribution of probabilities for all topics',
       y = 'Number of documents' , x = expression(gamma))

ggplot(lda_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = F) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = 'Distribution of probability for each topic',
       y = 'Number of documents' , x = expression(gamma))

# close to gamma matrix "1" means that document belong the topic
# contrary to gamma matrix "0" means that document not belong the topic


# format incompatiable error 
nasa_keyword
lda_gamma
class(lda_gamma$document)
class(nasa_keyword$id)

lda_gamma$document <- as.numeric(lda_gamma$document)
lda_gamma <- full_join(lda_gamma, nasa_keyword, by = c('document' = 'id'))

lda_gamma

top_keywords <- lda_gamma %>%
  filter(gamma > 0.9) %>%
  count(topic, keyword, sort = T)

top_keywords %>%
  group_by(topic) %>%
  top_n(5, n) %>%
  group_by(topic, keyword) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(keyword = factor(paste(keyword, topic, sep = "__"),
                          levels = rev(paste(keyword, topic, sep = "__")))) %>%
  ggplot(aes(keyword, n , fill = as.factor(topic))) +
  geom_col(show.legend = F) +
  labs(title = "Top keywords for each LDA topic",
       x = NULL, y = "Number of documents") +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x))+
  facet_wrap(~ topic, ncol = 3, scales = "free")




































































































































