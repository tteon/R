load('tripadv-planet.rda')
ls()
str(tripadv.review)

library(tidytext) # tokenize package
library(tidyverse) # text preprocessing package

unnest_tokens(tbl=tripadv.review, output=word, input=review, token='words')

# anti_join 
unnest_tokens(tbl=tripadv.review, output=word, input=review, token='words') %>%
  anti_join(stop_words, by='word') %>%
  select(c(id, date, rating, word)) %>%
  as_tibble()

# 
tripadv.review.words <- unnest_tokens(tbl=tripadv.review, output=word, input=review, token='words') %>%
  anti_join(stop_words, by='word') %>%
  select(c(id, date, rating, word)) %>%
  as_tibble()

tripadv.review.words


# 
get_sentiments(lexicon='bing')

tripadv.review.words %>%
  inner_join(get_sentiments('bing'), by = 'word')

# counts each review neg/pos stance
tripadv.review.words %>%
  inner_join(get_sentiments('bing'), by = 'word') %>% 
  count(id, date, rating, sentiment)
  
# add the column which is combined pos , neg counts
tripadv.review.words %>%
  inner_join(get_sentiments('bing'), by = 'word') %>% 
  count(id, date, rating, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment=positive-negative) %>%
  ungroup()

tripadv.sent.review <- tripadv.review.words %>%
  inner_join(get_sentiments('bing'), by = 'word') %>% 
  count(id, date, rating, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment=positive-negative) %>%
  ungroup()

summary(tripadv.sent.review$sentiment)

# visualization association between sentiment factor and rating
ggplot(tripadv.sent.review, aes(x=as.factor(rating), y=sentiment)) +
  geom_boxplot(fill='darkcyan', color='black')

ggplot(tripadv.sent.review, aes(x=as.factor(rating), y=sentiment)) +
  geom_boxplot(fill='darkcyan', color='black') +
  scale_x_discrete(breaks=c(1:5), labels=c("1 star", "2 star", "3 star", "4 star", "5 star")) +
  scale_y_continuous(breaks=seq(-6, 6, by=2)) +
  # define graph subfactors
  labs(x='Rating', y='Sentiment ( # of positives - # of negatives )', title = 'Review of korean restaurant', subtitle = "Distribution of sentiment score by review rating", caption="Source ; tripadvisor") +
  theme(plot.title=element_text(face='bold'),
        axis.text=element_text(face='bold'))

# coord flip  
ggplot(tripadv.sent.review, aes(x=as.factor(rating), y=sentiment)) +
  geom_boxplot(fill='darkcyan', color='black') +
  scale_x_discrete(breaks=c(1:5), labels=c("1 star", "2 star", "3 star", "4 star", "5 star")) +
  scale_y_continuous(breaks=seq(-6, 6, by=2)) +
  labs(x='Rating', y='Sentiment ( # of positives - # of negatives )', title = 'Review of korean restaurant', subtitle = "Distribution of sentiment score by review rating", caption="Source ; tripadvisor") +  # define x , y of style
  theme(plot.title=element_text(face='bold'),
        axis.text=element_text(face='bold')) +
  coord_flip()

# find out words among frequency appearing at each row with neg&pos 

tripadv.sent.words <- tripadv.review.words %>%
  inner_join(get_sentiments('bing'), by='word') %>%
  count(sentiment, word) %>%
  ungroup() %>%
  filter(n>=10) %>%
  mutate(nsign=ifelse(sentiment=='negative', -n, n))

print(tripadv.sent.words, n=Inf)

ggplot(tripadv.sent.words,
       aes(x=reorder(word, nsign), y=nsign),
       fill=factor(sentiment, levels=c('positive', 'negative'))) +
  # just should represent for orginal 'N'value using stat='identity'
  geom_bar(stat='identity', color='lightslategray', width=0.8) +
  # finetunning process
  theme(legend.position='bottom',
        legend.title=element_blank(),
        plot.title=element_text(face='bold'),
        axis.title=element_text(face='bold', size=10)) +
  #  graph title, axis title
  labs(x=NULL, y='Count', title='Review of Planet Hollywood',
       subtitle='Top words contributing to sentiment',
       caption='Soucre ; Tripadvisor') +
  # modify the values of bar
  scale_fill_manual(values=c('cornflowerblue','tomato')) +
  coord_flip()

library(scales)

# another version plus fruquency counts using geom_text
ggplot(tripadv.sent.words,
       aes(x=reorder(word, nsign), y=nsign,
       fill=factor(sentiment, levels=c('positive', 'negative')))) +
  # just should represent for orginal 'N'value using stat='identity'
  geom_bar(stat='identity', color='lightslategray', width=0.8) +
  geom_text(aes(label=n), size=3, color='black', hjust=ifelse(tripadv.sent.words$nsign <0, 1.1 , -0.1)) +
  # finetunning process
  theme(legend.position='bottom',
        legend.title=element_blank(),
        plot.title=element_text(face='bold'),
        axis.title=element_text(face='bold', size=10)) +
  #  graph title, axis title
  labs(x=NULL, y='Count', title='Review of Planet Hollywood',
       subtitle='Top words contributing to sentiment',
       caption='Soucre ; Tripadvisor') +
  # modify the values of bar
  scale_fill_manual(values=c('cornflowerblue','tomato')) +
  #scale_y_continuous(breaks=pretty(tripadv.sent.words$nsign), labels=abs(tripadv.sent.words$nsign)) +
  coord_flip()


# time-series anayltics
# BY month 
library(lubridate)
tripadv.review$ym <- floor_date(x=tripadv.review$date, unit = 'month')
tripadv.review$ym

tripadv.agg <- tripadv.review %>%
  group_by(ym) %>%
  summarise(rating = mean(rating, na.rm=T), n=n()) %>%
  ungroup()
tripadv.agg

# basic version
ggplot(tripadv.agg,
       aes(x=ym, y=rating)) +
  geom_line(color='khaki4', size=1) +
  geom_smooth(color='dodgerblue', size=1)

# tunning 
ggplot(tripadv.agg,
       aes(x=ym, y=rating)) +
  geom_line(color='khaki4', size=1) +
  geom_smooth(color='dodgerblue', size=1) +
  scale_x_date(date_labels = "%Y", date_breaks="2 years") +
  scale_y_continuous(breaks=c(1,2,3,4,5),
                     labels=c('1 star', '2 star', '3 star', '4 star', '5 star')) +
  labs(x=NULL, y='Rating', title = "Review of Korean traditional food restaurant",
       subtitle = 'Rating scores over time',
       caption="Source ; Tripadvisor") +
  theme(plot.title=element_text(face='bold'),text=element_text(family='sans'))

# review trend capture
ggplot(tripadv.agg,aes(x=ym, y=n)) +
  geom_line(color='khaki4', size=1) +
  geom_smooth(color='dodgerblue', size=1) +
  scale_x_date(date_labels = "%Y", date_breaks="2 years") +
  labs(x=NULL, y='Number of Reviews', title = "Review of Korean traditional food restaurant",
       subtitle = 'Number of reviews over time',
       caption="Source ; Tripadvisor") +
  theme(plot.title=element_text(face='bold'),text=element_text(family='sans'))















































