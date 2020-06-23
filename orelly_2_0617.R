library(dplyr)
library(tidytext)
library(janeaustenr)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams

austen_bigrams %>%
  count(bigram, sort =T )

library(tidyr)

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c('word1', 'word2'), sep = ' ')

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = T )

bigram_counts

library(igraph)

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2016)

a <- grid::arrow(type = 'closed', length = unit(.15, 'inches'))

ggraph(bigram_graph, layout = 'fr') +
  geom_edge_link(aes(edge_alpha = n), show.legend = F,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = 'lightblue', size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# edge_alpha , frequency aspect
# grid::arrow() , representation to direction
# end_cap
# HMM visualization

library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)
# count function
count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = 'ngrams' , n = 2) %>%
    separate(bigram, c('word1', 'word2'), sep = ' ') %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = T)
}
# visualize function
visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

library(gutenbergr)
kjv <- gutenberg_download(10)

library(stringr)

kjv_bigrams <- kjv %>%
  count_bigrams()

# tunning
kjv_bigrams %>%
  filter(n > 40,
         !str_detect(word1, '\\d'),
         !str_detect(word2, '\\d')) %>%
  visualize_bigrams()

# widyr package , wide matrix , ngram's problem , overall checking

austen_section_words <- austen_books() %>%
  filter(book == 'Pride & Prejudice') %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

austen_section_words

library(widyr)

# count the co-occurance words at each sentences
word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = T) # pairwise_ 는 word 변수의 각 단어 쌍에 대해 하나의 행을 생성한다는 것을 의미.

word_pairs

word_pairs %>%
  filter(item1 == 'darcy')

# inspecting these pair units correlation 
# 이진 상관에 대한 측정 지표 "phi coefficient"

word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = T)

# specific word 'pounds'
word_cors %>%
  filter(item1 == 'pounds')
# bar ver.
word_cors %>%
  filter(item1 %in% c('elizabeth', 'pounds', 'married', 'pride')) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~ item1, scales = 'free') +
  coord_flip()

set.seed(2016)
# graph ver.
word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# Chapter-5
# tidy process , document-term
# row - document , column - term , value - occurance count
# dtm objection tidy

library(tm)

data('AssociatedPress', package = 'topicmodels')

AssociatedPress

terms <- Terms(AssociatedPress)

head(terms)

library(dplyr)
library(tidytext)

ap_td <- tidy(AssociatedPress)
ap_td

ap_sentiments <- ap_td %>%
  inner_join(get_sentiments('bing'), by = c(term = 'word'))

ap_sentiments

library(ggplot2)

ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == 'negative', -n, n)) %>%
  mutate(term = reorder(term,n)) %>%
  ggplot(aes(term, n, fill= sentiment)) +
  geom_bar(stat = 'identity') +
  ylab('Contribution to sentiment') +
  coord_flip()

# dfm objective 

library(methods)

data('data_corpus_inaugural', package = 'quanteda')
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = F)

inaug_dfm

#
inaug_td <- tidy(inaug_dfm)

inaug_td

# 
inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

inaug_tf_idf

library(tidyr)
library(ggplot2)
year_term_counts <- inaug_td %>%
  extract(document, 'year', '(\\d+)', convert = T) %>% 
  complete(year, term, fill = list(count = 0)) %>% # including function
  group_by(year) %>%
  mutate(year_total = sum(count))

year_term_counts <- inaug_td %>%
  extract(document, "year", "(\\d+)", convert = TRUE) %>%
  complete(year, term, fill = list(count = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(count))

year_term_counts %>%
  filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>%
  ggplot(aes(year, count / year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("% frequency of word in inaugural address")

# textdata cast at matrix

ap_td
ap_td %>%
  cast_dtm(document, term, count)

ap_td %>%
  cast_dfm(document, term, count)

library(Matrix)

m <- ap_td %>%
  cast_sparse(document, term, count)

class(m)

dim(m)

library(janeaustenr)

austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word , n)

austen_dtm

# Corpus -> META data

data('acq')

acq

acq[[1]]

acq_td <- tidy(acq)

acq_td

acq_tokens <- acq_td %>%
  select(-places) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = 'word')

# the most frequency occur words
acq_tokens %>%
  count(word, sort = T)

# tf-idf
acq_tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))

# Chapter-6 Topic modeling

# 모든 문서는 토픽들의 혼합체이다.
# 모든 토픽은 단어들의 혼합체이다.

library(topicmodels)

data("AssociatedPress")

AssociatedPress

ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed= 1234))

ap_lda

library(tidytext)

ap_topics <- tidy(ap_lda , matrix = 'beta')

ap_topics

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.lenged = F) +
  facet_wrap(~ topic, scales = 'free') +
  coord_flip()

library(tidyr)

beta_spread <- ap_topics %>%
  mutate(topic = paste0('topic', topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 | .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()

# document-topic probability

ap_documents <- tidy(ap_lda, matrix = 'gamma')

ap_documents

tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))

# example , library thief

titles <- c('Twenty Thousand Leagues under the Sea',
            'The War of the Worlds',
            'Pride and Prejudice',
            'Great Expectations')

library(gutenbergr)

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = 'title')

library(stringr)

# 각기 1개 장을 대표하는 문서들로 나눈다.

reg <- regex("^chapter ", ignore_case = T)
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, reg))) %>%
  ungroup() %>%
  filter(chapter >0) %>%
  unite(document, title, chapter)

# 단어들로 분리한다.

by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# 문서-단어 카운트를 알아낸다.

word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = T) %>%
  ungroup()

word_counts

# 각 장의 LDA
# casting
chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm

chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed= 1234))

chapters_lda

chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = F) +
  facet_wrap(~ topic, scales = 'free') +
  coord_flip()

top_terms$topic
str(top_terms)

# classify each document
# 토픽별 문서당 확률인 감마를 검토함으로써 발견.

chapters_gamma <- tidy(chapters_lda, matrix = 'gamma')

chapters_gamma

chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)
chapters_gamma

# 그림을 그리기 전 토픽1, 토픽2 순서에 따라 제목의 순서를 바꾼다.

chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)

chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()


# validation procedure ,evaluation

chapter_classifications

book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)
?transmute

book_topics

chapter_classifications %>%
  inner_join(book_topics, by = 'topic') %>%
  filter(title != consensus)

# 단어별 할당; augment
# tidy() 가 모델의 통계 구성 요소를 탐색하는 반면에, augment()는 모델을 사용해 원본 데이터의 각 관측에 정보를 추가한다.

assignments <- augment(chapters_lda, data = chapters_dtm)
assignments

assignments <- assignments %>%
  separate(document, c('title', 'chapter'), sep = '_', convert = T) %>%
  inner_join(book_topics, by = c('.topic' = 'topic'))

library(scales)

assignments %>%
  count(title, consensus, wt = count) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = 'red', label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = 'Book words were assigned to',
       y = 'Book words came from',
       fill = '% of assignments')


wrong_words <- assignments %>%
  filter(title != consensus)

wrong_words

wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))

word_counts %>%
  filter(word == 'flopson')

# mallet
install.packages('mallet')
library(mallet)

# 1개 장당 1개 문자열을 사용해 벡터를 생성한다.
collapsed <- by_chapter_word %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_replace(word, "'", "")) %>%
  group_by(document) %>%
  summarize(text = paste(word, collapse = " "))

# create an empty file of "stopwords"
file.create(empty_file <- tempfile())
docs <- mallet.import(collapsed$document, collapsed$text, empty_file)

mallet_model <- MalletLDA(num.topics = 4)
mallet_model$loadDocuments(docs)
mallet_model$train(100)


# word-topic pairs
tidy(mallet_model)


# document-topic pairs
tidy(mallet_model, matrix = "gamma")

# column needs to be named "term" for "augment"
term_counts <- rename(word_counts, term = word)
augment(mallet_model, term_counts)























































