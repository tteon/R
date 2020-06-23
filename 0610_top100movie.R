url <- 'https://www.imdb.com/search/title/?title_type=feature&release_date=2018-01-01,2018-12-31&count=100'
library(xml2)
html <- read_html(url)
library(XML)
html.parsed <- htmlParse(html)

# rank
rank <- xpathSApply(html.parsed,"//span[@class='lister-item-index unbold text-primary']",xmlValue)
rank
rank <- as.numeric(rank)
str(rank)

# title
title <- xpathSApply(html.parsed,"//span[@class='lister-item-index unbold text-primary']/following-sibling::a",xmlValue)
title

title <- xpathSApply(html.parsed,"//h3[@class='lister-item-header']/a",xmlValue)
title

# description
description <- xpathSApply(html.parsed, "//p[@class='text-muted']",xmlValue)
description <- trimws(description)
description
# description trim ver.
description <- xpathSApply(html.parsed, "//p[@class='text-muted']",xmlValue,trim=T)
description <- trimws(description)
description
# runningtime
runtime <- xpathSApply(html.parsed, "//span[@class='runtime']",xmlValue)
runtime <- as.numeric(gsub(' min', '', runtime))
runtime
summary(runtime)

# genre
genre <- xpathSApply(html.parsed, "//span[@class='genre']",xmlValue,trim=T)
genre <- gsub(',.*', '',genre)
genre <- as.factor(genre)
genre

# rating
rating <- xpathSApply(html.parsed, "//div[@name='ir']/strong",xmlValue)
rating <- as.numeric(rating)

# rating
rating <- xpathSApply(html.parsed, "//div[@name='ir']",xmlGetAttr, 'data-value')
rating <- as.numeric(rating)
summary(rating)

# director
director <- xpathSApply(html.parsed, "//div[@class='lister-item-content']/p[@class='']/a[1]",xmlValue)
director
# indecing metohd
director <- xpathSApply(html.parsed, "//div[@class='lister-item-content']/p[3]/a[1]",xmlValue)
director
# votes
votes <- xpathSApply(html.parsed, "//p[@class='sort-num_votes-visible']/span[@name='nv'][1]",xmlValue)
votes
votes <- as.numeric(gsub(',','',votes))
votes
# indecing method
votes <- xpathSApply(html.parsed, "//p[@class='sort-num_votes-visible']/span[2]",xmlValue)
votes
# Attribute 
votes <- xpathSApply(html.parsed, "//p[@class='sort-num_votes-visible']/span[2]",xmlGetAttr, 'data-value')
votes

# gross
gross <- xpathSApply(html.parsed, "//p[@class='sort-num_votes-visible']/span[@name='nv']",xmlGetAttr, 'data-value')
gross

gross <- xpathSApply(html.parsed, "//p[@class='sort-num_votes-visible']/span[5]",xmlGetAttr, 'data-value')
gross

length(gross)

gross.nodes <- getNodeSet(html.parsed,"//p[@class='sort-num_votes-visible']")
# filling the value at NA
gross.nodes
gross <- sapply(gross.nodes, function(x) {
  val <- unlist(xpathSApply(x, "./span[5]", xmlGetAttr, "data-value"))
  if (is.null(val)) val <- NA 
  else val
})
gross
length(gross)

# at once time 
gross <- xpathSApply(html.parsed, "//p[@class='sort-num_votes-visible']",
                     function(x) {
      val <- unlist(xpathSApply(x, "./span[5]", xmlGetAttr, "data-value"))
      if (is.null(val)) val <- NA 
      else val
})
gross <- as.numeric(gsub(",",'',gross))
summary(gross)

movie2018 <- data.frame(rank=rank, title=title, description=description , runtime=runtime, genre=genre ,rating=rating, director=director , votes=votes ,gross=gross)
str(movie2018)
View(movie2018)


library(ggplot2)
library(scales)

ggplot(movie2018, aes(x=genre, y=gross/1000)) +
   geom_boxplot(fill='plum2',  color='black') +
   geom_point(color='blue') +
   scale_y_continuous(labels=comma) +
   labs(x='Genre', y='Gross (1,000 Dollars)',title='Most Popular Films 100 of Year 2018', subtitle='Distribution of gross', caption ='Source: IMDb') +
   theme(plot.title=element_text(face='bold'),
          axis.text=element_text(face='bold')) 
  
ggplot(movie2018, aes(x=genre, y=gross/1000)) +
  geom_boxplot(fill='plum2',  color='black') +
  geom_point(position='jitter', color='blue', alpha=0.5) +
  geom_text(aes(label=ifelse(rank <= 3, paste(rank, "-", title), '')),
            col='red', fontface='bold', size=3, hjust=0, vjust=0) +
  scale_y_continuous(labels=comma) +
  labs(x='Genre', y='Gross (1,000 Dollars)',title='Most Popular Films 100 of Year 2018', subtitle='Distribution of gross', caption ='Source: IMDb') +
  theme(plot.title=element_text(face='bold'),
        axis.text=element_text(face='bold')) 













































