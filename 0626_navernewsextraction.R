

url <- "https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&date=20200626"

library(RCurl)
library(XML)
# confirm the webpage at each target site 
library(readr)
library(stringr)


# check the encoding format 
guess_encoding(file=url)
enc <- as.character(guess_encoding(file=url)[1,1])
enc
html <- getURL(url, .encoding=enc)
html.parsed <- htmlParse(html, encoding=localeToCharset())
html.parsed

# section
section <- xpathSApply(html.parsed, "//h4/em", xmlValue)
section 

str_conv(section, encoding='UTF-8')
str_trim(str_conv(section, encoding='UTF-8'))
# except to last 2 section ( TV , photo)
section <-str_trim(str_conv(section, encoding='UTF-8'))[1:6]
section

# rank & quote
news.quote1 <- xpathSApply(html.parsed, "//li[@class='num1']/dl/dt/a",xmlValue)
news.quote1 <- str_conv(news.quote1, encoding='UTF-8')
news.quote1

news.quote5 <- xpathSApply(html.parsed, "//li[@class='num5']/dl/dt/a",xmlValue)
news.quote5 <- str_conv(news.quote5, encoding='UTF-8')
news.quote5

ranknews <- data.frame()
for (i in c(1:5)) {
  # define xpath because of loop utilizing
  xpath <- paste0("//li[@class='num", i, "']/dl/dt/a")
  section <- xpathSApply(html.parsed, "//h4/em", xmlValue)
  section <- str_trim(str_conv(section, encoding='UTF-8'))[1:6]
  news.quote <- xpathSApply(html.parsed, xpath, xmlValue)
  news.quote <- str_conv(news.quote, encoding='UTF-8')
  news <- cbind(rank=i, section=section, quote=news.quote)
  ranknews <-rbind(ranknews, news, stringsAsFactors=F)
}

str(ranknews)
view(ranknews)

# content
news.link1 <- xpathSApply(html.parsed, "//li[@class='num1']/dl/dt/a", xmlGetAttr, "href")
news.link1 
# URL capture
for (link in news.link1) {
  link <- paste0("https://news.naver.com/", link)
  print(link)
  enc2 <- as.character(guess_encoding(link)[1, 1])
  html2 <- getURL(link, .encoding=enc2)
  html.parsed2 <- htmlParse(html2, encoding=localeToCharset())
}


news.content <- c()
for (link in news.link1) {
  link <- paste0("https://news.naver.com/", link)
  print(link)
  enc2 <- as.character(guess_encoding(link)[1, 1])
  html2 <- getURL(link, .encoding=enc2)
  html.parsed2 <- htmlParse(html2, encoding=localeToCharset())
  # not include ancestor node
  xpath <- "//div[@id='articleBodyContents']//text()[not(ancestor::script)][not(ancestor::a)]"
  content <- xpathSApply(html.parsed2, xpath, xmlValue)
  # load the data in line with format
  content <- paste(content, collapse = " ")
  news.content <- c(news.content, content)
  news.content <- str_conv(news.content, encoding="UTF-8")
  news.content <- str_trim(news.content)
}
news.content

# crawling complete code

ranknews <- data.frame()
for (i in c(1:5)) {
  # define xpath because of loop utilizing
  xpath <- paste0("//li[@class='num", i, "']/dl/dt/a")
  section <- xpathSApply(html.parsed, "//h4/em", xmlValue)
  section <- str_trim(str_conv(section, encoding='UTF-8'))[1:6]
  news.quote <- xpathSApply(html.parsed, xpath, xmlValue)
  news.quote <- str_conv(news.quote, encoding='UTF-8')
  news <- cbind(rank=i, section=section, quote=news.quote)
  news.link <- xpathSApply(html.parsed, xpath, xmlGetAttr, "href")
  news.content <- c()
  for (link in news.link) {
    link <- paste0("https://news.naver.com", link)
    print(link)
    # not include ancestor node
    xpath <- "//div[@id='articleBodyContents']//text()[not(ancestor::script)][not(ancestor::a)]"
    enc2 <- as.character(guess_encoding(file=link)[1, 1])
    html2 <- getURL(link, .encoding=enc2)
    html.parsed2 <- htmlParse(html2, encoding=localeToCharset())
    content <- xpathSApply(html.parsed2, xpath, xmlValue)
    # load the data in line with format
    content <- paste(content, collapse = " ")
    news.content <- c(news.content, content)
  }
  news.content <- str_conv(news.content, encoding="UTF-8")
  news.content <- str_trim(news.content)
  news <- cbind(news, content=news.content)
  ranknews <-rbind(ranknews, news, stringsAsFactors=F)
}

str(ranknews)
view(ranknews)

# overall crawling code

naverRanknews <- function(from=Sys.Date(), to=Sys.Date()) {
  library(RCurl)
  library(XML)
  library(readr)
  library(stringr)
  ranknews <- data.frame()
  headurl <- "https://news.naver.com/main/ranking/popularDay.nhn?rankingType=popular_day&date="
  from <- as.Date(from)
  to <- as.Date(to)
  period <- gsub("-", "", as.character(seq(from, to, by=1)))
  for (d in period) {
    tailurl <- d
    url <- paste0(headurl, tailurl)
    enc <- as.character(guess_encoding(file=url)[1,1])
    html <- getURL(url, .encoding=enc)
    html.parsed <- htmlParse(html, encoding=localeToCharset())
    for(i in c(1:5)) {
      xpath <- paste0("//li[@class='num", i, "']/dl/dt/a")
      section <- xpathSApply(html.parsed, "//h4/em", xmlValue)
      section <- str_trim(str_conv(section, encoding="UTF-8"))[1:6]
      news.quote <- xpathSApply(html.parsed, xpath, xmlValue)
      news.quote <- str_conv(news.quote, encoding="UTF-8")
      news <- cbind(date=d, rank=i, section=section, quote=news.quote)
      news.link <- xpathSApply(html.parsed, xpath, xmlGetAttr, "href")
      news.content <- c()
      for (link in news.link){
        link <- paste0("https://news.naver.com", link)
        xpath <- "//div[@id='articleBodyContents']//text()[not(ancestor::script)][not(ancestor::a)]"
        enc2 <- as.character(guess_encoding(file=link)[1,1])
        html2 <- getURL(link, .encoding=enc2)
        html.parsed2 <- htmlParse(html2, encoding=localeToCharset())
        content <- xpathSApply(html.parsed2, xpath, xmlValue)
        content <- paste(content, collapse=" ")
        news.content <- c(news.content, content)
        Sys.sleep(sample(10,1)*0.01)
      }
      news.content <- str_conv(news.content, encoding="UTF-8")
      news.content <- gsub("[\r\n\t]", "", news.content)
      news <- cbind(news, content=news.content)
      ranknews <- rbind(ranknews, news, stringsAsFactors=FALSE)
    }
  }
  ranknews <- cbind(id=1:nrow(ranknews), ranknews)
  return(ranknews)
}

f_ranknews <- naverRanknews(from="2020-01-01", to="2020-06-01")
str(f_ranknews)







































