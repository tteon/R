text <- c('Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg',
          'A vegetarian diet excludes all animal flesh (meat, poultry, seafood).',
          'Economists surveyed by Refinitiv expect the economy added 160,000 jobs.')
source <- c('BBC', 'CNN', 'FOX')

library(dplyr)
text.df <- tibble(source=source, text=text)
text.df
class(text.df)

install.packages('tidytext')
library(tidytext)
unnest_tokens(tbl=text.df, output=word, input=text)

# example
head(iris)
iris %>% head()

tidy.docs <- text.df %>%
  unnest_tokens(output=word, input=text)
tidy.docs
print(tidy.docs, n=Inf)

?unnest_tokens

tidy.docs %>%
  count(source) %>%
  arrange(desc(n))

?anti_join

word.removed <- tibble(word=c('http','bbc.in','1g0j4agg'))
anti_join(tidy.docs,word.removed, by='word')

tidy.docs <- tidy.docs %>%
  anti_join(word.removed, by='word')
tidy.docs$word

# removing digit things
grep('\\d+', tidy.docs$word)
tidy.docs <- tidy.docs[-grep('\\d+', tidy.docs$word),]
tidy.docs$word

text.df$text <- gsub('(f|ht)tp\\S+\\s*', '', text.df$text)
text.df$text <- gsub('\\d+', '', text.df$text)
text.df$text

tidy.docs <- text.df %>%
  unnest_tokens(output=word, input=text)
print(tidy.docs, n=Inf)

stop_words

tidy.docs <- tidy.docs %>%
  anti_join(stop_words, by='word')
tidy.docs$word

tidy.docs$word <- gsub('\\s+', '', tidy.docs$word)

# stemming
library(SnowballC)
tidy.docs <- tidy.docs %>%
  mutate(word=wordStem=(word))
tidy.docs$word

tidy.docs$word <- gsub('economist', 'economi', tidy.docs$word)
tidy.docs$word

#transformation corpus -> tidy

library(tm)
corpus.docs <- VCorpus(VectorSource(text))
corpus.docs
meta(corpus.docs, tag='author', type='local') <- source
tidy(corpus.docs)

tidy(corpus.docs) %>%
  unnest_tokens(word, text) %>%
  select(source=author, word)







