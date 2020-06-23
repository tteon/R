

url <- 'https://www.amazon.com/Who-Moved-My-Cheese-Amazing/product-reviews/0399144463/ref=cm_cr_getr_d_paging_btm_prev_1?ie=UTF8&reviewerType=all_reviews&pageNumber=1'
library(xml2)
library(XML)
html <- read_html(url)
html.parsed <- htmlParse(html)

# product
product <- xpathSApply(html.parsed, "//h1/a[@data-hook='product-link']", xmlValue)
product

# author

xpathSApply(html.parsed, "//span[@class='a-profile-name']", xmlValue)
xpathSApply(html.parsed, "//span[@class='a-profile-name']", xmlValue)[-c(1,2)]

xpathSApply(html.parsed, "//div[@class='a-profile-content']/span[@class='a-profile-name']", xmlValue)

author <- xpathSApply(html.parsed, "//div[@class='a-section review aok-relative']//span[@class='a-profile-name']", xmlValue)
author

# date

date <- xpathSApply(html.parsed, "//span[@data-hook='review-date']", xmlValue)
date

date <- gsub("Reviewed.*on ", '' , date)
date
# transformation procedure
Sys.setlocale('LC_TIME', 'English')
?strptime
date <- as.Date(date, format='%B %d, %Y')
date

Sys.setlocale()

# quote

quote <- xpathSApply(html.parsed, "//a[@data-hook='review-title']/span", xmlValue)
quote

# review

review <- xpathSApply(html.parsed, "//span[@data-hook='review-body']", xmlValue)
review

# rating
rating <- xpathSApply(html.parsed, "//i[@data-hook='review-star-rating']/span", xmlValue)
rating

rating <- xpathSApply(html.parsed, "//i[@data-hook='review-star-rating']", xmlValue)
rating

# extraction
rating <- as.numeric(substr(rating, 1, 3))
rating

# helpful
helpful <- xpathSApply(html.parsed, "//span[@data-hook='helpful-vote-statement']", xmlValue)
helpful















































