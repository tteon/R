url <- "https://www.imdb.com/title/tt0114709/?ref_=nv_sr_srsg_3"
library(RCurl)
html <- getURL(url)
# not wokring since security function 
html <- getURL(url, ssl.verifypeer=F, ssl.verifyhost=F)
'''
library(httr)
html <- GET(url)

library(xml2)
html <- read_html(url)
'''

library(XML)
html.parsed <- htmlParse(html)

rating <- xpathSApply(doc=html.parsed , path="//span[@itemprop='ratingValue']", fun=xmlValue)
rating

rating <- xpathSApply(doc=html.parsed , path="//strong/span", fun=xmlValue)
rating

rating <- as.numeric(rating)
rating
str(rating)

# bring out image url
poster <- xpathSApply(html.parsed, path="//img[@alt='Toy Story Poster']", xmlGetAttr, "src")
poster

poster <- xpathSApply(html.parsed, path="//img[@title='Toy Story Poster", xmlGetAttr, "src")
poster

poster <- xpathSApply(html.parsed, path="//div[@class='poster']//img", xmlGetAttr, "src")
poster

download.file(poster, 'toystory.jpg', mode='wb')
list.files(pattern='.jpg')

install.packages('imager')
library(imager)
plot(load.image('toystory.jpg'), axes=F)

summary <- xpathSApply(html.parsed, path="//div[@class='summary_text']", xmlValue)
summary <- trimws(summary)
summary



















