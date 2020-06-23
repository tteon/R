getwd()
install.packages(c('twitteR','ROAuth','base64enc'))
library(twitteR)
library(ROAuth)
library(base64enc)
consumerKey <- 'PPHdcrwtggBmjLXhLZeYMjALa'
consumerSecret <- 'I02EDs8fF8SbXkPIKcwesk3B6ntPt7kkpkfHr9zVKmfqjHXVoO'
accessToken <- '830318842647703553-h97dbYhmKz7GGNEaOzc8W9uB50ufTr0'
accessTokenSecret <- 'z0O6tIo1R7hERjjaL8qcYkUIqtr8iTnGMNZNHaonHVbc2'

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)
 
keyword <- enc2utf8('빅데이터')

bigdata <- searchTwitter(keyword, n=500, lang='ko')
length(bigdata)
head(bigdata)

keyword2 <- enc2utf8('코로나')
bigdata2 <- searchTwitter(keyword2, since='2020-02-01', until='2020-05-25', lang='ko')
length(bigdata2)
head(bigdata2)

keyword3 <- enc2utf8('현대차')
bigdata3 <- searchTwitter(keyword3, since='2020-02-01', until='2020-05-25', lang='ko',n=500)
length(bigdata3)
head(bigdata3)

getwd()









