url = "https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&date=20200619"

library(RCurl)
library(XML)
library(readr) # checking encoding 
library(stringr) # preprocessing function

# web encoding
guess_encoding(file=url)
enc <- as.character(guess_encoding(file=url)[1, 1])
enc
html <- getURL(url, .encoding=enc)
html.parsed <- htmlParse(html, encoding=localeToCharset())

# section extraction
section <- xpathSApply(html.parsed,
                       "//h4/em", xmlValue)
section
str_conv(section, encoding="UTF-8")
section <- str_trim(str_conv(section, encoding="UTF-8"))
section






































