

url = 'https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=189537&target=after'
library(RCurl)
html <- getURL(url)
html

# check the webpage of encoding format
# charset = " "
library(stringr)
unlist(str_extract_all(html, "<meta.+?>"))

library(readr)
guess_encoding(file=url)
guess_encoding(file=url)[1,1]
enc <- as.character(guess_encoding(file=url)[1,1])
enc

html <- getURL(url, .encoding=enc)
html

# check local encoding environment
localeToCharset()
Encoding(html)
?Encoding

library(XML)
html.parsed <- htmlParse(html, encoding = localeToCharset())

# author
author <- xpathSApply(html.parsed, "//a[@class='author']", xmlValue)
author

# date

date <- xpathSApply(html.parsed, "//td[@class='num']/text()", xmlValue)
date

xpathSApply(html.parsed, "//td[@class='num']/node()[not(self::*)]", xmlValue)

# converting process 
# important !!!!!!!!
?strptime
date <- as.Date(date, format="%y.%m.%d")
date

# title
title <- xpathSApply(html.parsed, "//a[@class='movie color_b']", xmlValue)
title

# figure out ERROR to convert text with encoding 
library(stringi)
stri_enc_mark(title)
stri_enc_isutf8(title)

title <- str_conv(string=title, encoding="UTF-8")
title

# review
xpathSApply(html.parsed, "//td[@class='title']/text()", xmlValue) %>% 
  str_conv(encoding="UTF-8")

# extract only needs with info using normalize-space function
xpathSApply(html.parsed, "//td[@class='title']/text()[normalize-space()]", xmlValue) %>% 
  str_conv(encoding="UTF-8") %>%
  str_trim()

# Add NULL values 
xpathSApply(html.parsed, "//td[@class='title']/text()[normalize-space()]", xmlValue) %>% 
  str_conv(encoding="UTF-8") %>%
  str_trim()

review <-xpathSApply(html.parsed, "//td[@class='title']", function(x) {
  val <- unlist(xpathSApply(x, "./text()[normalize-space()]", xmlValue))
  if (is.null(val)) val <- NA
  else val
}) %>%
  str_conv(encoding="UTF-8") %>%
  str_trim()
review

# rating
rating <- xpathSApply(html.parsed, "//div[@class='list_netizen_score']/em", xmlValue)
rating <- as.numeric(rating)
rating

# knowing page counts process using rating data counts
# total pages
total.pages <- xpathSApply(html.parsed, "//strong[@class='c_88 fs_11']", xmlValue)
total.pages <- ceiling(as.numeric(total.pages)/10) # remember two points ceiling , /10
total.pages

navermovReview <- function (baseurl, n=NULL) {
  library(RCurl)
  library(readr)
  library(XML)
  library(stringr)
  if (is.null(n)){
    url <- paste0(baseurl, "1")
    enc <- as.character(readr::guess_encoding(file=url)[1,1])
    html <- getURL(url, .encoding=enc)
    html.parsed <- html.parse(html, encoding=localeToCharset())
    total.pages <- xpathSApply(html.parsed, "//strong[@class='c_88 fs_11']", xmlValue)
    total.pages <- ceiling(as.numeric(total.pages)/10)
    ifelse(total.pages > 1000, n <- 1000, n <- total.pages)
}
navermov.review <- data.frame()
for (i in c(1:n)) {
  url <- paste0(baseurl, i)
  enc <- as.character(readr::guess_encoding(file=url)[1,1])
  html <- getURL(url, .encoding=enc)
  html.parsed <- htmlParse(html, encoding=localeToCharset())
  author <- xpathSApply(html.parsed, "//a[@class='author']", xmlValue)
  date <- xpathSApply(html.parsed, "//td[@class='num']/node()[not(self::*)]", xmlValue)
  date <- as.Date(date, format="%y.%m.%d")
  title <- xpathSApply(html.parsed, "//a[@class='movie color_b']", xmlValue)
  title <- str_conv(string=title, encoding="UTF-8")
  review <-xpathSApply(html.parsed, "//td[@class='title']", function(x) {
    val <- unlist(xpathSApply(x, "./text()[normalize-space()]", xmlValue))
    if (is.null(val)) val <- NA
    else val
  }) %>%
    str_conv(encoding="UTF-8") %>%
    str_trim()
  rating <- xpathSApply(html.parsed, "//div[@class='list_netizen_score']/em", xmlValue)
  rating <- as.numeric(rating)
  if (length(date) >0) {
    navermov.r <- data.frame(author = author, date = date , title = title , review = review ,rating = rating , strongAsFactor=F)
    navermov.review <- rbind(navermov.review, navermov.r)
  }
  Sys.sleep(sample(10,1)*0.1)
}
navermov.review <- cbind(id=1:nrow(navermov.review), navermov.review)
return(navermov.review)
}

baseurl <- "https://movie.naver.com/movie/point/af/list.nhn?st=mcode&sword=189537&target=&page="
navermov.review <- navermovReview(baseurl=baseurl, n=10)
View(navermov.review)  

save('navermov-alive.rda')

library(KoNLP)
library(rJava)


str(navermov.review)

alive.review <- navermov.review$review
alive.review5 <- alive.review[1:5]
alive.review5

library(tidyverse)

alive.review5 <- alive.review5 %>%
  str_replace_all("[ㄱ-ㅣ]", "") %>%
  str_replace_all("[[:punct:]]", "") %>%
  str_replace_all("[[:digit:]]", "") %>%
  str_trim()

alive.review5

# dictionary tunning
buildDictionary(ext_dic=c("sejong", "woorimalsam", "insighter"))

?SimplePos09

alive.words <- SimplePos09(alive.review5)
alive.words

alive.words <- str_match_all(alive.words, pattern="([가-힣]+)/[NPM]")
alive.words

# extraction technique , has condition using function ( wordlength >= 2)
alive.words <- sapply(alive.words, function(x) x[,2][str_length(x[,2]) >= 2])
alive.words
# unlist To vector
alive.words <- unlist(alive.words)
alive.words

sort(table(alive.words), decreasing=T)

# 'autoSpacing'
alive.words <- alive.review %>%
  str_replace_all("[ㄱ-ㅣ]", "") %>%
  str_replace_all("[[:punct:]]", "") %>%
  str_replace_all("[[:digit:]]", "") %>%
  str_trim() %>%
  SimplePos09(autoSpacing=T) %>%
  str_match_all(pattern="([가-힣]+)/[NPM]") %>%
  sapply(function(x) x[,2][str_length(x[,2]) >= 2]) %>%
  unlist()
sort(table(alive.words), decreasing=T)[1:10]


library(wordcloud2)
alive.wc <-
  wordcloud2(data=table(alive.words[!(alive.words == "영화" |
                                        alive.words == "박신혜" |
                                        alive.words == "유아인" |
                                        alive.words == "좀비" |
                                        alive.words == "스토리")]),
             minSize=1, color="random-light", backgroundColor="black")
alive.wc

# To get the nice quallity results of wordcloud 
install.packages("webshot")
library(webshot)
library(htmlwidgets)
saveWidget(alive.wc, "alive.html", selfcontained = F)
webshot::install_phantomjs()
webshot(url="alive.html", file="alive.png", delay=10) 
library(pander)
openFileInOS("alive.png")
openFileInOS("alive.html")
















































