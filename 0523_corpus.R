text <- c('Crash dieting is not the best way to lose weight. http://bbc.in/1G0J4Agg',
          'A vegetarian diet excludes all animal flesh (meat, poultry, seafood).',
          'Economists surveyed by Refinitiv expect the economy added 160,000 jobs.')

install.packages('tm')
library(tm)
data(crude)
crude

crude[[1]]
crude[[1]]$content
crude[[1]]$meta

corpus.docs <- VCorpus(VectorSource(text))
class(corpus.docs)

corpus.docs
inspect(corpus.docs[[1]])

as.character(corpus.docs[[1]])
lapply(corpus.docs, as.character)

str(corpus.docs[[1]])

corpus.docs[[1]]$content
lapply(corpus.docs, content)
paste(as.vector(unlist(lapply(corpus.docs, content))),collapse=" ")

corpus.docs[[1]]$meta
meta(corpus.docs[[1]])
meta(corpus.docs[[1]], tag='id')
meta(corpus.docs[[1]], tag='datetimestamp')
# insert the author blank process 
meta(corpus.docs[[1]], tag='author', type='local') <- 'BBC'
meta(corpus.docs[[1]])

source <- c('BBC', 'CNN', 'FOX')
meta(corpus.docs, tag='author', type='local') <- source
lapply(corpus.docs , meta , tag='author')


category <- c('health', 'lifestyle', 'business')
meta(corpus.docs, tag='category', type='local') <- category
lapply(corpus.docs , meta , tag='category')
# delect 'origin' meta item
meta(corpus.docs, tag='origin', type='local') <- NULL
lapply(corpus.docs , meta)

corpus.docs.filtered <-
  tm_filter(corpus.docs,
            FUN=function(x)
              any(grep('weight|diet', content(x))))
lapply(corpus.docs.filtered, content)

idx <- meta(corpus.docs, 'author')=='FOX' | meta(corpus.docs, 'category')=='health'
lapply(corpus.docs[idx], content)

writeCorpus(corpus.docs)
list.files(pattern='\\.txt')

# text preprocessing _ Transformation
getTransformations()

corpus.docs <- tm_map(corpus.docs, content_transformer(tolower))
lapply(corpus.docs, content)

stopwords('english')
corpus.docs <- tm_map(corpus.docs, removeWords, stopwords('english'))
lapply(corpus.docs, content)

# Customizing function
myRemove <- content_transformer(function(x, pattern)
  {return(gsub(pattern, '', x))})

corpus.docs <- tm_map(corpus.docs, myRemove, '(f|ht)tp\\S+\\s*')
lapply(corpus.docs, content)

corpus.docs <- tm_map(corpus.docs, removePunctuation)
lapply(corpus.docs, content)

corpus.docs <- tm_map(corpus.docs, removeNumbers)
lapply(corpus.docs, content)

# removing the blank which existing between words 
corpus.docs <- tm_map(corpus.docs, stripWhitespace)
lapply(corpus.docs, content)

corpus.docs <- tm_map(corpus.docs, content_transformer(trimws))
lapply(corpus.docs, content)

# extracting the stem 
corpus.docs <- tm_map(corpus.docs, stemDocument)
lapply(corpus.docs, content)

# lemmenization
corpus.docs <- tm_map(corpus.docs, content_transformer(gsub),pattern='economist', replacement='economi')
lapply(corpus.docs, content)














