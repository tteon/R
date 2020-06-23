
text <- c("Because I could not stop for Death -",
          "HE kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")
text

library(dplyr)
text_df <- data_frame(line = 1:4, text = text)
text_df

library(tidytext)

# 1 argument word - column name , 텍스트의 중첩이 해제될 때 생성될 출력 열 이름을 지정하고 
# 2 argument text - column name , 텍스트가 들어오는 입력 열을 지정한다.

text_df %>%
  unnest_tokens(word, text)


# 정돈하기

install.packages("janeaustenr")
library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>% group_by(book) %>% mutate(linenumber = row_number(), chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = T)))) %>% ungroup()
library(tidytext)
tidy_books <- original_books %>% unnest_tokens(word , text)
tidy_books

data(stop_words)
tidy_books <- tidy_books %>% anti_join(stop_words)

tidy_books %>% count(word, sort = T)

library(ggplot2)
# visualization
tidy_books %>%
  count(word, sort =T) %>%
  filter(n > 600) %>%
  mutate(word = reorder (word , n )) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab('counts') +
  ylab('the most occur in this novel') +
  coord_flip()

install.packages('gutenbergr')
library(gutenbergr)

hgwells <- gutenberg_download(c(35, 36, 5230, 159))

tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_hgwells %>%
  count(word, sort =T)

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))

tidy_bronte <- bronte %>% unnest_tokens(word, text) %>% anti_join(stop_words)

tidy_bronte %>% count(word, sort = T)

library(tidyr)

frequency <- bind_rows(mutate(tidy_bronte, author = 'Bronte Sister'),
                       mutate(tidy_bronte, author = 'H.G. Wells'),
                       mutate(tidy_bronte, author = 'Jane Austen')) %>%
  mutate(word = str_extract(word, "[a-z]+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, 'Bronte Sister' : 'H.G. Wells')

frequency <- bind_rows(mutate(tidy_bronte, author = "Bronte Sisters"),
                       mutate(tidy_hgwells, author = "H.G. Wells"), 
                       mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Bronte Sisters`:`H.G. Wells`)

library(scales)

ggplot(frequency, aes(x= proportion, y = 'Jane Austen', color = abs('Jane Austen' - proportion))) +
  geom_abline(color = "gray40", lty= 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = T, vjust= 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),low ='darkslategray4', high = 'gray75') +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position='nono') +
  labs(y = 'Jane Austen', x =NULL)
  
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.2, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

# 상관분석

cor.test(data = frequency[frequency$author == "Bronte Sisters",],
         ~ proportion + `Jane Austen`)


cor.test(data = frequency[frequency$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)


# chapter 2

library(tidytext)

sentiments

# AFINN , Bing , NRC
# based on unigrams 
# AFINN - allocate to score -5 ~ 5 at each word , + -> pos - > neg
# Bing - binary format
# NRC - has a spectrum of [positive negative anger anticipation disgust fear joy sadness surprise trust]
?get_sentiments
get_sentiments('afinn')
get_sentiments('bing')
get_sentiments('nrc')
get_sentiments('loughran')

# using inner join for sentiment analysis 

library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(), chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = T)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

?unnest_tokens
tidy_books

nrcjoy <- get_sentiments('nrc') %>%
  filter(sentiment == 'joy')
get_sentiments('nrc')

tidy_books %>%
  filter(book == 'Emma') %>%
  inner_join(nrcjoy) %>%
  count(word, sort = T)

library(tidyr)

# %/% 연산자는 정수 나눗셈과 같으므로 이 %/% 연산자를 사용하면 80줄을 한 단원으로 삼아 긍정 정서와 부정 정수를 계수해 볼 수 있다.

jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

library(ggplot2)

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = F) +
  facet_wrap(~book, ncol = 2, scales = 'free_x')


# sentiment dicitonaries comparison 

pride_prejudice <- tidy_books %>%
  filter(book == 'Pride & Prejudice')

pride_prejudice

afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(pride_prejudice %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          pride_prejudice %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = F) +
  facet_wrap(~method, ncol = 1, scales = 'free_y')

# check the distribution about these sentiment dictionary

get_sentiments('nrc') %>%
  filter(sentiment %in% c('positive', 'negative')) %>%
  count(sentiment)

get_sentiments('bing') %>%
  count(sentiment)

# find out the most frequency of pos | neg words

bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments('bing')) %>%
  count(word, sentiment, sort =T ) %>%
  ungroup()
bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = F) +
  facet_wrap(~sentiment, scales = 'free_y') +
  labs(y = 'Constribution to sentiment', x = NULL) +
  coord_flip()
# tunning dic

custom_stop_words <- bind_rows(data_frame(word = c('miss'),
                                          lexicon = c('custom')),
                               stop_words)
custom_stop_words

nrc_word_counts <- tidy_books %>%
  inner_join(get_sentiments('nrc')) %>%
  count(word, sentiment, sort = T) %>%
  ungroup()

nrc_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = F) +
  facet_wrap(~sentiment, scales = 'free_y') +
  labs(y = 'Constribution to sentiment', x = NULL) +
  coord_flip()

# word cloud

library(wordcloud)

tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n , max.words = 100))

library(reshape2)

tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

?acast

# 단어 보기

PandP_sentences <- data_frame(text = prideprejudice) %>%
  unnest_tokens(sentence, text , token = 'sentences')

str(PandP_sentences)
PandP_sentences$sentence[2]

austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = 'regex', pattern = 'Chapter|CHAPTER [\\dIVXLC]') %>%
  ungroup()

austen_chapters %>%
  group_by(book) %>%
  summarise(chapters = n())

bingnegative <- get_sentiments('bing') %>%
  filter(sentiment == 'negative')

str(bingnegative)
bingnegative$word[450:500]

wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n())

tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c('book', 'chapter')) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  top_n(1) %>%
  ungroup()


# Chapter-3
# tf-idf -> heuristic quantity.

# word frequency
library(dplyr)
library(janeaustenr)
library(tidytext)

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort =T) %>%
  ungroup()

total_words <- book_words %>%
  group_by(book) %>%
  summarize(total = sum(n))

?summarize

book_words <- left_join(book_words, total_words)
book_words

library(ggplot2)
ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(Show.legend = F) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = 'free_y')

# 지프의 법칙 -> 단어가 나타나는 빈도는 순위에 반비례한다고 한다.
freq_by_rank <- book_words %>%
  group_by(book) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

rank_subset <- freq_by_rank %>%
  filter(rank <500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

# bind_tf_idf function
book_words <- book_words %>%
  bind_tf_idf(word, book, n)
book_words

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

book_words %>% arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(book) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=book)) +
  geom_col(show.legend = F) +
  labs(x = NULL, y = 'tf-idf') +
  facet_wrap(~book, ncol = 2 , scales = 'free') +
  coord_flip()


# 물리학 텍스트의 말뭉치

library(gutenbergr)
physics <- gutenberg_download(c(37729, 14725, 13476, 5001),
                              meta_fields = 'author')

physics_words <- physics %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = T) %>%
  ungroup()

physics_words

plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo", "Huygens, Christiaan", "Tesla, Nikola", "Einstein, Albert")))

plot_physics %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill= author)) +
  geom_col(show.legend = F) +
  labs(x = NULL, y= "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = 'free') +
  coord_flip()

# customize stopwords dictionary

mystopwords <- data_frame(word = c('eq', 'co', 'rc', 'ac', 'ak', 'bn', 'fig', 'file', 'cg', 'cb', 'cm'))
physics_words<- anti_join(physics_words, mystopwords, by = 'word')
plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup %>%
  mutate(author = factor(author, levels = c('Galilei, Galileo',
                                            'Huygens, Christiaan' ,
                                            'Tesla, Nikola')))

ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = F) +
  labs(x = NULL, y = 'tf-idf') +
  facet_wrap(~author, ncol = 2, scales= 'free') +
  coord_flip()

# chapter-4

library(dplyr)
library(tidytext)
library(janeaustenr)

# tokenize

austen_bigrams <- austen_books() %>% unnest_tokens(bigram, text, token = 'ngrams', n = 2 )

austen_bigrams

# n-gram selection and screening

austen_bigrams %>% count(bigram, sort =T)

library(tidyr)

# separate
bigrams_separated <- austen_bigrams %>% separate(bigram, c('word1', 'word2'), sep = ' ')
bigrams_separated
# Remove the stopword at df by using function of filter & %IN%
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = T)

bigram_counts

bigram_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = ' ')

bigram_united2 <- bigrams_filtered %>%
  unite(bigram2, word1, word2, sep = '.') # unite(columnname, feature1 , feature2 , sep)

bigram_united2

# trigram

trigram_counts <- austen_books() %>%
  unnest_tokens(trigram, text, token = 'ngrams', n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = ' ') %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = T)

trigram_united <- trigram_counts %>%
  unite(trigram, word1 , word2 , word3 , sep = 't')
trigram_united

# bigram analytics , focus on 'street' word

bigrams_filtered %>%
  filter(word2 == 'street') %>%
  count(book, word1, sort = T)

bigram_tf_idf <- bigram_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book , n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

# useful for mammoth text dateset -> bigram anayltics , context understand

# 정서분석 시 바이그램을 사용해 문맥 제공하기

bigrams_separated %>%
  filter(word1 == 'not') %>%
  count(word1, word2, sort = T)

AFINN <- get_sentiments('afinn')

AFINN

not_words <- bigrams_separated %>%
  filter(word1 == 'not') %>%
  inner_join(AFINN, by = c(word2 = 'word')) %>%
  count(word2, value, sort = T ) %>%
  ungroup()

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = F) +
  xlab('Words preceded by \"not\"') +
  ylab('Sentiment value * number of occurrences') +
  coord_flip()

not_words

# review neg words
negation_words <- c('not', 'no', 'never', 'without')

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = 'word')) %>%
  count(word1, word2, value, sort = T) %>%
  ungroup()

negated_words

negated_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word3 = reorder(word3, contribution)) %>%
  ggplot(aes(word3, n * value, fill = n * value > 0)) +
  geom_col(show.legend = F) +
  xlab('Words preceded by a lots words') +
  ylab('Sentiment Value * number of occurrences') +
  coord_flip()
 

# ggraph , visualization the bigram result

install.packages('igraph')
library(igraph)

bigram_counts

# filtering relatively frequency than normal words
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

install.packages('ggraph')
library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = 'fr') +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
























































































