library(tidytext)
install.packages('textdata')
library(textdata)
sentiments

get_sentiments(lexicon='bing')
unique(get_sentiments('bing')$sentiment)

get_sentiments(lexicon='afinn')
unique(get_sentiments('afinn')$value)
summary(get_sentiments('afinn')$value)

get_sentiments(lexicon='nrc')
unique(get_sentiments('nrc')$sentiment)

get_sentiments(lexicon='loughran')
unique(get_sentiments('loughran')$sentiment)

library(dplyr)
library(tibble)
library(purrr)
library(readr)
library(lubridate)
url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/00438/Health-News-Tweets.zip'
local.copy <- tempfile()
download.file(url, local.copy, mode='wb')
# locale modify -> eng's format
Sys.setlocale('LC_TIME', 'English')
# load the datafile
health.twitter <-
  map(unzip(zipfile=local.copy,
            files=c('Health-Tweets/bbchealth.txt',
                    'Health-Tweets/cnnhelath.txt',
                    'Health-Tweets/foxnewshealth.txt',
                    'Health-Tweets/NBChelath.txt')),
      read_delim, delim='|', quote='',
      col_types=list(col_character(), col_character(), col_character()),
      col_names=c('id', 'datetime', 'tweet')) %>%
  map2(c('bbc', 'cnn', 'foxnews', 'nbc'), ~ cbind(.x, source=.y)) %>%
  reduce(bind_rows) %>%
  as_tibble() %>%
  mutate(datetime=ymd_hms(strptime(datetime, '%a %b %d %H:%M:%S +0000 %Y')))
# %a day %b month %d  %H %M %S +0000 %Y year
health.twitter
# removing above temp data
unlink(local.copy)
Sys.setlocale()

health.twitter %>%
  count(source)


# data preprocess
library(stringr)
health.words <- health.twitter %>%
  select(-id) %>%
  mutate(tweet=str_replace_all(tweet, pattern='(f|ht)tp\\S+s*', replacement='')) %>%
  mutate(tweet=str_replace_all(tweet, pattern='\\d+', replacement='')) %>%
  mutate(tweet=str_replace_all(tweet, pattern='\\bRT', replacement='')) %>%
  mutate(tweet=str_replace_all(tweet, pattern='@\\S+', replacement='')) %>%
  mutate(tweet=str_replace_all(tweet, pattern='&amp', replacement='')) %>%
  unnest_tokens(word, tweet)

# by ->selecting joint column 
health.words %>%
  inner_join(get_sentiments('bing'), by='word')

health.words %>%
  inner_join(get_sentiments('bing'), by='word') %>%
  count(word, sentiment, sort=T)

health.words %>%
  inner_join(get_sentiments('bing'), by='word') %>%
  count(word, sentiment, sort=T) %>%
  group_by(sentiment) %>%
  top_n(10 , n) %>%
  ungroup()

health.sentiment <- health.words %>%
  inner_join(get_sentiments('bing'), by='word') %>%
  count(word, sentiment, sort=T) %>%
  group_by(sentiment) %>%
  top_n(10 , n) %>%
  ungroup() %>%
  mutate(nsign=ifelse(sentiment=='negative', -n, n))

health.sentiment

library(ggplot2)
library(scales)
ggplot(health.sentiment, aes(x=reorder(word, nsign), y=nsign, fill=factor(sentiment, levels=c('positive', 'negative')))) +
       geom_col(color='lightslategray', width=0.8) + 
       geom_text(aes(label=n), size=3, color='black', hjust=ifelse(health.sentiment$nsign<0, 1.1, -0.1)) +
       scale_fill_manual(values=c('cornflowerblue','tomato')) +
       scale_y_continuous(breaks=pretty(health.sentiment$nsign),labels=abs(pretty(health.sentiment$nsign))) +
       labs(x=NULL, y='Count') +
       theme(legend.position ='bottom', legend.title=element_blank()) +
       coord_flip()

# filtering the words which is no relationship with sentiment
health.sentiment <- health.words %>%
  inner_join(get_sentiments('bing'), by='word') %>%
  filter(!(word=='patient'|word=='cancer'|word=='virus')) %>%
  count(word, sentiment, sort=T) %>%
  group_by(sentiment) %>%
  top_n(10 , n) %>%
  ungroup() %>%
  mutate(nsign=ifelse(sentiment=='negative', -n, n))

ggplot(health.sentiment, aes(x=reorder(word, n), y=n, fill=factor(sentiment, levels=c('positive', 'negative')))) +
  geom_col(color='lightslategray', width=0.8, show.legend=F) + 
  geom_text(aes(label=n), size=3, color='black', hjust=1.2) +
  scale_fill_manual(values=c('lightsteelblue','lightsalmon')) +
  facet_wrap(~ factor(sentiment, levels=c('positive', 'negative')),ncol=2, scales='free') +
  labs(x=NULL, y='Count') +
  coord_flip()

library(wordcloud)
library(reshape2)
set.seed(123)
windows(width=7, height=7)
health.words %>%
  inner_join(get_sentiments('bing'), by='word') %>%
  filter(!(word=='patient'|word=='cancer'|word=='virus')) %>%
  count(word, sentiment, sort=T) %>%
  ungroup() %>%
  acast(word ~ sentiment, value.var = 'n', fill=0) %>%
  comparison.cloud(colors=c('tomato', 'cornflowerblue'),title.size=2,title.colors=c('red','blue'),title.bg.colors='wheat',scale=c(4, 0.3), max.words=200)

# groupby ; source (broadcast)
health.sentiment <- health.words %>%
  inner_join(get_sentiments('bing'), by='word') %>%
  filter(!(word=='patient'|word=='cancer'|word=='virus')) %>%
  count(word, sentiment, source, sort=T) %>%
  group_by(source, sentiment) %>%
  top_n(10 , n) %>%
  ungroup() 
health.sentiment

windows(width=7, height=9)
ggplot(health.sentiment, aes(reorder_within(x=word, by=n, within=source), y=n, fill=source)) +
  geom_col(show.legend=F) +
  facet_wrap(~ factor(source, labels=c('BBC', 'CNN', 'Fox News', 'NBC')) + sentiment , ncol=2, scales='free') +
  scale_x_reordered() +
  labs(x=NULL, y='Count') +
  coord_flip()

#time-series

health.sentiment <- health.words %>%
  inner_join(get_sentiments('bing'), by='word') %>%
  filter(!(word=='patient'|word=='cancer'|word=='virus')) %>%
  mutate(time=floor_date(x=datetime, unit='month')) %>%
  count(sentiment, time) %>%
  group_by(sentiment) %>%
  slice(2:(n()-1)) %>%
  ungroup()
health.sentiment

Sys.setlocale('LC_TIME', 'English')

windows(width=7.0, height=5.5)
ggplot(health.sentiment, aes(x=time, y=n, fill=sentiment, color=sentiment)) +
  geom_area(position='identity', alpha=0.3) +
  geom_line(size=1.5) +
  scale_fill_manual(labels=c('Negative', 'Positive'), values=c('orangered', 'deepskyblue2')) +
  scale_color_manual(labels=c('Negative', 'Positive'), values=c('orangered', 'deepskyblue2')) +
  scale_x_datetime(date_labels='%b %Y', date_breaks='6 months') +
  labs(x=NULL, y='Count') +
  theme(legend.position='bottom', legend.title=element_blank())

windows(width=9.0, height=9.0)
health.words %>%
  inner_join(get_sentiments('bing'), by='word') %>%
  filter(!(word=='patient'|word=='cancer'|word=='virus')) %>%
  mutate(time=floor_date(x=datetime, unit='month')) %>%
  count(source, sentiment, time) %>%
  group_by(source, sentiment) %>%
  slice(2:(n()-1)) %>%
  ungroup() %>%
  ggplot(aes(x=time, y=n, fill=sentiment, color=sentiment)) +
  geom_area(position='identity', alpha=0.3) +
  geom_line(size=1.5) +
  facet_wrap(~ factor(source, labels=c('BBC', 'CNN', 'Fox News', 'NBC')), nrow=4,scales='free') +
  scale_fill_manual(labels=c('Negative', 'Positive'), values=c('coral', 'cornflowerblue')) +
  scale_color_manual(labels=c('Negative', 'Positive'), values=c('coral', 'cornflowerblue')) +
  scale_x_datetime(date_labels='%b %Y', date_breaks='2 months') +
  labs(x=NULL, y='Count') +
  theme(legend.position='bottom', legend.title=element_blank() ,axis.text.x=element_text(size=8))




















