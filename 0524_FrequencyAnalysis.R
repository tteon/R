install.packages('quanteda')
library(quanteda)

data_corpus_inaugural
summary(data_corpus_inaugural)
class(data_corpus_inaugural)

library(tidytext)
library(tibble)
library(dplyr)

tidy(data_corpus_inaugural) %>%
  filter(Year > 1990) 

# trimws -> blank deleting function
us.president.aaddress <- tidy(data_corpus_inaugural) %>%
  filter(Year > 1990) %>%
  group_by(President, FirstName) %>%
  summarise_all(list(~trimws(paste(., collapse=' ')))) %>%
  arrange(Year) %>%
  ungroup()

us.president.aaddress

library(tm)
?DataframeSource
# revise the format for fitting the function of DataframeSource is need to ( text, doc_id ) argument
us.president.aaddress <- us.president.aaddress %>%
  select(text, everything()) %>% 
  add_column(doc_id=1:nrow(.), .before=1)
us.president.aaddress
address.corpus <- VCorpus(DataframeSource(us.president.aaddress))
address.corpus
lapply(address.corpus[1], content)


address.corpus <- tm_map(address.corpus, content_transformer(tolower))
lapply(address.corpus[1], content)

# customizing stopwords dictionary
mystopwords <- c(stopwords('english'), c('can', 'must', 'will'))
# preprocessing flow
address.corpus <- tm_map(address.corpus, removeWords, mystopwords)
address.corpus <- tm_map(address.corpus, removePunctuation)
address.corpus <- tm_map(address.corpus, removeNumbers)
address.corpus <- tm_map(address.corpus, stripWhitespace)
address.corpus <- tm_map(address.corpus, content_transformer(trimws))
# stemming 
address.corpus <- tm_map(address.corpus, content_transformer(gsub),
                         pattern='america|americas|america|americans',
                         replacement='america')
lapply(address.corpus[1], content)

address.dtm <- DocumentTermMatrix(address.corpus)
inspect(address.dtm)

# For calculating , transformation procedure corpus to matrix 
termfreq <- colSums(as.matrix(address.dtm))
length(termfreq)

termfreq[head(order(termfreq, decreasing=T))]
termfreq[tail(order(termfreq, decreasing=T))]

# filtering with Criterion by flashing freqency 
findFreqTerms(address.dtm, lowfreq=40)

library(ggplot2)
# Need to use 'ggplot2' of condition -> dataframe format
termfreq.df <- data.frame(word=names(termfreq), frequency=termfreq)
termfreq.df
# visualization
ggplot(subset(termfreq.df, frequency >=40), 
       aes(x=word, y=frequency, fill=word)) +
  geom_col(color='dimgray') +
  labs(x=NULL , y='Term Frequency (Count) ')

# visualization extended version
ggplot(subset(termfreq.df, frequency >=40), 
       aes(x=reorder(word, frequency), y=frequency, fill=word)) +
  geom_col(color='dimgray', width=0.6 , show.legend=FALSE) +
  geom_text(aes(label=frequency), size=3.5, color='black', hjust=-0.3) +
  labs(x=NULL , y='Term Frequency (Count)') +
  coord_flip()
  
set.seed(123)
install.packages('wordcloud')
library(wordcloud)
library(RColorBrewer)
head(termfreq)
wordcloud(words=names(termfreq), freq=termfreq,
          scale=c(4, 0.5), min.freq = 5,
          rot.per=0.1, random.order=F,
          colors=brewer.pal(6, 'Dark2'),
          random.color=F)

inspect(address.dtm)
rownames(address.dtm) <- c('Clinton', 'Bush', 'Obama', 'Trump')
Docs(address.dtm)

address.tf <- tidy(address.dtm)
address.tf <- address.tf %>%
  mutate(document=factor(document,
                         levels=c('Clinton', 'Bush', 'Obama', 'Trump'))) %>%
                         arrange(desc(count)) %>%
                           group_by(document) %>%
                           top_n(n=10, wt=count) %>%
                           ungroup()
address.tf

ggplot(address.tf, aes(x=term, y=count, fill=document)) +
  geom_col(show.legend = F) +
  facet_wrap(~document, ncol=2, scales='free') +
  labs(x=NULL, y='Term Frequency (count)')+
  coord_flip()

# rearrange to frequency
ggplot(address.tf, aes(reorder_within(x=term, by=count, within=document), y=count, fill=document)) +
  geom_col(show.legend = F) +
  facet_wrap(~document, ncol=2, scales='free') +
  scale_x_reordered() +
  labs(x=NULL, y='Term Frequency (count)')+
  coord_flip()

address.dtm2 <- DocumentTermMatrix(address.corpus,
                                   control=list(weighting=weightTfIdf))
rownames(address.dtm2) <- c('Clinton', 'Bush', 'Obama', 'Trump')
Docs(address.dtm2)
inspect(address.dtm2)

# 'count' column name modify -> 'tf-idf' 
address.tfidf <- tidy(address.dtm2) %>%
  mutate(tf_idf=count, count=NULL)

address.tfidf
address.tfidf <- address.tfidf %>%
  mutate(document=factor(document,
                         levels=c('Clinton', 'Bush', 'Obama', 'Trump'))) %>%
  arrange(desc(tf_idf)) %>%
  group_by(document) %>%
  top_n(n=10, wt=tf_idf) %>%
  ungroup()
address.tfidf

ggplot(address.tfidf, aes(reorder_within(x=term, by=tf_idf, within=document), y=tf_idf, fill=document)) +
  geom_col(show.legend = F) +
  facet_wrap(~document, ncol=2, scales='free') +
  scale_x_reordered() +
  labs(x=NULL, y='Term Frequency (tf_idf)')+
  coord_flip()

uspresident.address <- us.president.aaddress

address.words <- uspresident.address %>%
  unnest_tokens(word, text)
# preprocessing procedure
address.words <- address.words %>%
  anti_join(stop_words, by='word') %>%
  filter(!grepl(pattern='\\d+', word)) %>%
  mutate(word=gsub(pattern="'", replacement='', word)) %>%
  mutate(gsub(pattern='america|americas|america|americans',replacement='america', word)) %>%
  count(President, word, sort=T, name='count') %>%
  ungroup()

address.words
# preprocess ~ visualzaion 
address.words %>%
  group_by(word) %>%
  summarise(count=sum(count)) %>%
  arrange(desc(count)) %>%
  top_n(n=10, wt=count) %>%
  ggplot(aes(reorder(word, -count), count)) +
  geom_col(color='dimgray', fill='salmon', width=0.6, show.legend = F) +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  geom_text(aes(label=count), size=3.5 , color='black', vjust=-0.3) +
  labs(x=NULL, y='Term Frequency (count)')

# bind_tf_idf function 
address.words <- address.words %>%
  bind_tf_idf(term=word, document=President, n=count)
address.words

address.words %>%
  arrange(desc(tf_idf))
address.words %>%
  arrange(tf_idf)


address.words %>%
  arrange(desc(tf_idf)) %>%
  mutate(President=factor(President,levels=c('Clinton', 'Bush', 'Obama', 'Trump'))) %>%
  group_by(President) %>%
  top_n(8, wt=tf_idf) %>%
  ggplot(aes(reorder_within(word, tf_idf, President), tf_idf, fill=President)) +
  geom_col(show.legend = F) +
  facet_wrap(~President, ncol=2, scales='free') +
  scale_x_reordered() +
  labs(x=NULL, y='Term Frequency-Inverse Document Frequency') +
  coord_flip()
  
address.words %>%
  arrange(desc(tf)) %>%
  mutate(President=factor(President,levels=c('Clinton', 'Bush', 'Obama', 'Trump'))) %>%
  group_by(President) %>%
  top_n(8, wt=tf) %>%
  ggplot(aes(reorder_within(word, tf, President), tf, fill=President)) +
  geom_col(show.legend = F) +
  facet_wrap(~President, ncol=2, scales='free') +
  scale_x_reordered() +
  labs(x=NULL, y='Term Frequency (proportion)') +
  coord_flip()









