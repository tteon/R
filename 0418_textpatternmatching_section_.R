# backreference; "( )( ) ...\N"

txt <- '# a small thing makes a big difference #'
txt
# from a to a
gsub("([[:alpha:]]).+\\1", "*", txt)

gsub("([[:alpha:]]).+?\\1", "*", txt, perl=T)

string <- "<div class='power'>100%</div>"
string

gsub("(<.*?>)(.*)(<.*?>)", "\\2", string)

state.name
# matching for starting sentence 'New'
grep("^New", state.name, value=T)
# upper NEW 
gsub("^New(.*[xy].*)", "NEW\\1", state.name,
     ignore.case=T)

# Example for text pattern matching 
telephones <- "Barabasi, Alebert-Laszlo917 1843James Bond(02)563-1987(1)John F. Kennedy051-766-5879(123)Dr. who(062)324-9576McCartney, J. Paul0648323912"
telephones

pattern <- "[[:alpha:]., -]{2,}"
name <- unlist(regmatches(x=telephones,
                          m=gregexpr(pattern=pattern,
                                     telephones)))
name

pattern <-
  "\\(?(\\d{2,3})?\\)?(-| )?\\d{3}(-| )?\\d{4}\\(?(\\d{1,3})?\\)?"
phone <- unlist(regmatches(x=telephones, m=gregexpr(pattern=pattern,telephones)))

phone

data.frame(name=name, phone=phone)

# text function 

string <- c("data analytics is useful",
            "business analyics is helpful",
            "visulization of data is interesting fo data scientists")
string

#return index
grep(pattern="data", x=string)
#return values
grep(pattern="data", x=string, value=T)

string[grep(pattern="data", x=string)]


grep("useful|helpful" , string, value=T)
grep("useful|helpful" , string, value=T, invert=T)

# return the values to logical value ( T | F)
grepl(pattern="data", x=string)

state.name
grepl(pattern="new", x=state.name, ignore.case = T)
state.name[grepl(pattern="new", x=state.name, ignore.case = T)]
sum(grepl(pattern="new", x=state.name, ignore.case = T))

# g means global
regexpr(pattern="data", text=string)
gregexpr(pattern="data", text=string)

# pattern extraction
regmatches(x=string, m=regexpr("data", string))
regmatches(x=string, m=gregexpr("data", string))

regmatches(x=string, m=gregexpr("data", string), invert=T)

# pattern replacement
sub(pattern="data", replacement="text", x=string)
gsub(pattern="data", replacement="text", x=string)

# pattern split
strsplit(x=string, split=" ")
unlist(strsplit(x=string, split=" "))
unique(unlist(strsplit(x=string, split=" ")))

# string package
string <- c("data analytics is useful",
            "business analytics is helpful",
            "visualization of data is interesting fo data scientists")
string

install.packages("stringr")
library(stringr)

str_detect(string=string, pattern='data')
str_detect(string, fixed('DATA' , ignore_case = T))

# excluding regular expression using fixed argument
str_detect(c('abz', 'ayz', 'a.z'), fixed('a.z'))
str_detect(c('abz', 'ayz', 'a.z'), 'a\\.z')

# ' str_locate = regexpr ' has a equivant function
str_locate(string, 'data')
str_locate_all(string, 'data')

# ' str_extract = regmatches ' 
str_extract()
str_extract_all(string, 'data')
# if u need to return matrix format , then using simplify
str_extract_all(string, 'data', simplify=T)
#
sentences5 <- sentences[1:5]

str_match()
str_match_all()

# each function examples 
str_extract(sentences5, "(a|A|the|The) (\\w+)")
str_match(sentences5, "(a|A|the|The) (\\w+)")
str_match_all(sentences5, "(a|A|the|The) (\\w+)")

# sub , gsub = repalce , replace_all
str_replace(string=string , pattern='data', replacement = 'text')
str_replace_all(string=string , pattern='data', replacement = 'text')

# 
str_split(string, " ")
unlist(str_split(string, " "))
unique(unlist(str_split(string, " ")))

str_split(string, ' ', n=3)
str_split(string, ' ', n=3, simplify=T)

str_length(string)

str_count(string, 'data')
str_count(string, '\\w+')

equivant_sentences <- str_pad(string=c('a', 'abc', 'abcde'), width=6, side='left' , pad=' ')
str_count(equivant_sentences, '')

mon <- 1:12
mon
# pad
str_pad(mon , width=2, side='left', pad='0')

# trim
str_trim()

str.pad <- str_pad(string, width=max(str_length(string)), side='both' , pad=' ' )
str.pad

str_trim(str.pad, side='both')

# paste function 
str_c('data', 'mining', sep=' ')

str.mining <- str_c(c('data mining', 'text mining'),'is useful', sep=' ')
str.mining

# collapse
str_c(str.mining, collapse = ';')
str_c(str.mining, collapse = '\n')
cat(str_c(str.mining, collapse = '\n'))

str_sub(string=str.mining, start=1, end=4)

str_sub(str.mining, 5, 5) <- '-'
str.mining

# start , end 
str_sub('abcdefg', start=-2)
str_sub('abcdefg', end=-3)

# encoding 
Sys.getlocale()
Sys.getlocale(category='LC_CTYPE')
localeToCharset()

star <- 'º° Çì´Â ¹ã, Copyright 1941. À± µ¿ ÁÖÁÖ'
star

Encoding(star)

star2 <- iconv(x=star, from=localeToCharset(), to='UTF-8')
Encoding(star2)
star2

Encoding(star2) <- 'CP949'
star2

Encoding(star2) <- 'UTF-8'
star2

length(iconvlist())

url <- 'https://www.naver.com/'
install.packages("RCurl")
library(RCurl)
library(stringr)

enc.chk <- getURL(url=url)
enc.chk
# encoding checking
unlist(str_extract_all(enc.chk, "<meta.+?>"))[1:2]
Encoding(enc.chk)

install.packages('readr')
library(readr)
guess_encoding(file=url)

url <- 'https://www.mk.co.kr/'
enc.chk <- getURL(url=url)
enc.chk

unlist(str_extract_all(enc.chk, '<li class=("first2"|"add")  >.*?</li>'))
unlist(str_extract_all(enc.chk, "<meta.+?>"))[1:2]

guess_encoding(file=url)

enc.chk <- getURL(url=url, .encoding='EUC-KR')
unlist(str_extract_all(enc.chk, "<li class=('first2'|'add').*?</li>"))






































































