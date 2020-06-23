# corpus ver.

text <- c('Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg',
          'A vegetarian diet excludes all animal flesh (meat, poultry, seafood).',
          'Economists surveyed by Refinitiv expect the economy added 160,000 jobs.')


# preprocessing workflow
library(tm)
corpus.docs <- VCorpus(VectorSource(text))
corpus.docs <- tm_map(corpus.docs, content_transformer(tolower))
corpus.docs <- tm_map(corpus.docs, removeWords, stopwords('english'))
myRemove <- content_transformer(function(x, pattern)
  {return(gsub(pattern, '', x))})
corpus.docs <- tm_map(corpus.docs, myRemove, '(f|ht)tp\\S+\\s*')
corpus.docs <- tm_map(corpus.docs, removePunctuation)
corpus.docs <- tm_map(corpus.docs, removeNumbers)
corpus.docs <- tm_map(corpus.docs, stripWhitespace)
corpus.docs <- tm_map(corpus.docs, content_transformer(trimws))
corpus.docs <- tm_map(corpus.docs, stemDocument)
corpus.docs <- tm_map(corpus.docs, content_transformer(gsub),
                      pattern='economist', replacement='economi')


corpus.docs
?DocumentTermMatrix
corpus.dtm <- DocumentTermMatrix(corpus.docs, control=list(wordLengths=c(2, Inf)))
corpus.dtm

nTerms(corpus.dtm)
Terms(corpus.dtm)

nDocs(corpus.dtm)
Docs(corpus.dtm)

# modify document title
rownames(corpus.dtm)
rownames(corpus.dtm) <- c('BBC', 'CNN', 'FOX')

Docs(corpus.dtm)

inspect(corpus.dtm)
inspect(corpus.dtm[1:2, 10:15])

library(tidytext)
tidy(corpus.dtm)

text <- c('Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg',
          'A vegetarian diet excludes all animal flesh (meat, poultry, seafood).',
          'Economists surveyed by Refinitiv expect the economy added 160,000 jobs.')
source <- c('BBC', 'CNN', 'FOX')

# tidy ver.

library(dplyr)
library(SnowballC)
library(tidytext)

text.df <- tibble(source=source, text=text)
text.df$text <- gsub('(f|ht)tp\\S+\\s*', '', text.df$text)
text.df$text <- gsub('\\d+', '', text.df$text)
tidy.docs <- text.df %>%
  unnest_tokens(output=word, input=text) %>%
  anti_join(stop_words, by='word') %>%
  mutate(word=wordStem(word))
tidy.docs$word <- gsub('\\s+', '', tidy.docs$word)
tidy.docs$word <- gsub('economist', 'economi', tidy.docs$word)

tidy.docs %>% print(n=Inf)

tidy.docs %>%
  count(source, word)

tidy.dtm <- tidy.docs %>%
  count(source, word) %>%
  cast_dtm(document=source, term=word, value=n)
tidy.dtm

Terms(tidy.dtm)
Docs(tidy.dtm)
inspect(tidy.dtm)












