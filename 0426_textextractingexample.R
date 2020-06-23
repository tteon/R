

url = 'http://www.abrahamlincolnonline.org/lincoln/speeches/gettysburg.htm'
library(RCurl)
html <- getURL(url)
html

pattern <- "<.*?>"
text <- gsub(pattern, '', html)
text

text <- gsub('[\t\n]', '' , text)
text


url2 = 'https://unstats.un.org/unsd/tradekb/knowledgebase/country-code'
html2 <- getURL(url2)
html2

library(stringr)
pattern <- "[A-Z]{3}\\s([[:alpha:],.'()-]+\\s?){1,}"
countrycode <- unlist(str_extract_all(html2, pattern))
head(countrycode)
countrycode <- countrycode[-c(1:4)]
head(countrycode)

# remove blank
country.code <- str_trim(countrycode)
country.code

code <- str_sub(country.code, 1, 3)
country <- str_sub(country.code, 5 )
iso3code <- data.frame(code=code, country=country)
head(iso3code)








