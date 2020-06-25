
url <- "https://www.tripadvisor.com/Restaurant_Review-g294197-d3922438-Reviews-Arirang_Korean_Restaurant-Seoul.html"
library(xml2)
library(XML)
html <- read_html(url)
html.parsed <- htmlParse(html)
<h1 data-test-target="top-info-header" class="restaurants-detail-top-info-TopInfo__restaurantName--1IKBe">Arirang Korean Restaurant</h1>

  
# name
name <- xpathSApply(html.parsed, "//h1[@class='restaurants-detail-top-info-TopInfo__restaurantName--1IKBe']",xmlValue)
name

# location

<span class="restaurants-detail-top-info-TopInfo__infoCellIcon--3FMyc"><span class="_213lPF2w _2HBN-k68 rZHcZ9a2" style="font-size:20px"></span></span>
  
location <- xpathSApply(html.parsed, "//span[@class='restaurants-detail-top-info-TopInfo__infoCellIcon--3FMyc']/following-sibling::a",xmlValue)[1]
location

# author

<div class="info_text pointer_cursor" onclick="widgetEvCall('handlers.usernameClick', event, this);"><div>Cpwee</div></div>  
# normal case  
review <- xpathSApply(html.parsed, "//div[@class='info_text pointer_cursor']/div",xmlValue)
review

# considering another author , point out "|" punt
author <- xpathSApply(html.parsed, "//div[@class='info_text pointer_cursor']/div | //div[@class='info_text ']/div",xmlValue)
author


# date
date <- xpathSApply(html.parsed, "//span[@class='ratingDate']",xmlGetAttr,'title')
# locale setting 
Sys.setlocale("LC_TIME","English")
date <- as.Date(date, format="%B %d,%Y")
date
# setting again , local ver
Sys.setlocale()

# quote
quote <- xpathSApply(html.parsed, "//span[@class='noQuotes']",xmlValue)
quote


# review
# except reply owner
xpathSApply(html.parsed, "//div[@class='prw_rup prw_reviews_text_summary_hsx']//p[@class='partial_entry']",xmlValue)

# upgrade 
xpathSApply(html.parsed, "//div[@class='ui_column is-9']//div[@class='prw_rup prw_reviews_text_summary_hsx']//p[@class='partial_entry']",xmlValue)

review <- xpathSApply(html.parsed, "//div[@class='ui_column is-9']//div[@class='prw_rup prw_reviews_text_summary_hsx']//p[@class='partial_entry']",xmlValue)
review

# rating
xpathSApply(html.parsed, "//div[@class='ui_column is-9']/span", xmlGetAttr, 'class')

xpathSApply(html.parsed, "//div[@class='reviewSelector cx_brand_refresh_phase2']//div[@class='ui_column is-9']/span", xmlGetAttr, 'class')

rating <- xpathSApply(html.parsed, "//div[@class='reviewSelector cx_brand_refresh_phase2']//div[@class='ui_column is-9']/span[1]", xmlGetAttr, 'class')
rating
library(stringr)
# extraction from right str
# str_sub is extraction function
rating <-as.numeric(str_sub(rating, start=-2))/10
rating

# total pages

total.pages <- xpathSApply(html.parsed, "//a[@class='pageNum last   cx_brand_refresh_phase2']",xmlValue)
total.pages <- as.numeric(gsub(",", "", total.pages))
# how change their these website address
'''
https://www.tripadvisor.com/Restaurant_Review-g294197-d3922438-Reviews-Arirang_Korean_Restaurant-Seoul.html
https://www.tripadvisor.com/Restaurant_Review-g294197-d3922438-Reviews-or10-Arirang_Korean_Restaurant-Seoul.html
https://www.tripadvisor.com/Restaurant_Review-g294197-d3922438-Reviews-or20-Arirang_Korean_Restaurant-Seoul.html
'''
tripadvReview <- function(baseurl, n=NULL) {
  library(xml2)
  library(XML)
  library(stringr)
  if (is.null(n)) {
    html <- read_html(baseurl)
    html.parsed <- htmlParse(html)
    total.pages <- xpathSApply(html.parsed, "//a[@class='pageNum last   cx_brand_refresh_phase2']",xmlValue)
    total.pages <- as.numeric(gsub(",", "", total.pages))
    n <- total.pages
  }
  tripadv.review <- data.frame()
  Sys.setlocale("LC_TIME", "English")
  for ( i in c(1:n)) {
    if (i==1) {url <- baseurl}
    else{
      spliturl <- unlist(strsplit(baseurl, "Reviews-"))
      url <- paste0(spliturl[1], "Reviews-or", (i-1)*10, spliturl[2])
    }
    html <- read_html(url)
    html.parsed <- htmlParse(html)
    name <- xpathSApply(html.parsed, "//h1[@class='restaurants-detail-top-info-TopInfo__restaurantName--1IKBe']",xmlValue)
    location <- xpathSApply(html.parsed, "//span[@class='restaurants-detail-top-info-TopInfo__infoCellIcon--3FMyc']/following-sibling::a",xmlValue)[1]
    author <- xpathSApply(html.parsed, "//div[@class='info_text pointer_cursor']/div | //div[@class='info_text ']/div",xmlValue)
    date <- xpathSApply(html.parsed, "//span[@class='ratingDate']",xmlGetAttr,'title')
    date <- as.Date(date, format="%B %d,%Y")
    quote <- xpathSApply(html.parsed, "//span[@class='noQuotes']",xmlValue)
    review <- xpathSApply(html.parsed, "//div[@class='ui_column is-9']//div[@class='prw_rup prw_reviews_text_summary_hsx']//p[@class='partial_entry']",xmlValue)
    rating <- xpathSApply(html.parsed, "//div[@class='reviewSelector cx_brand_refresh_phase2']//div[@class='ui_column is-9']/span[1]", xmlGetAttr, 'class')
    rating <-as.numeric(str_sub(rating, start=-2))/10
    if(length(date)>0){
      tripadv.r <- data.frame(name=name, location=location, author=author, date=date, quote=quote, review=review, rating=rating, stringsAsFactors = F)
      tripadv.review <- rbind(tripadv.review , tripadv.r)
    }
    Sys.sleep(sample(10,1)*0.1)
  }
  Sys.setlocale()
  tripadv.review <- cbind(id=1:nrow(tripadv.review), tripadv.review)
  return(tripadv.review)
}

baseurl <- 'https://www.tripadvisor.com/Restaurant_Review-g294197-d3922438-Reviews-Arirang_Korean_Restaurant-Seoul.html'
tripadv.review <- tripadvReview(baseurl=baseurl, n=5)
str(tripadv.review)
view(tripadv.review)

tripadv.review <- tripadvReview(baseurl=baseurl)
save(tripadv.review, file='tripadv-planet.rda')




























