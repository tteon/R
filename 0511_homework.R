base 함수
# string 대조하여 장,단점 도출


string <- c("data analytics is useful",
            "business analytics is helpful",
            "visualization of data is interesting fo data scientists")

# 1. 패턴검출 <-> str.detect
grep('data',string)
grepl('data',string)


# 2. 패턴위치탐색 <-> str.locate
regexpr('data',string)
gregexpr('data',string)
# 1-1 ) 해당 표현이 등장한 문자열 위치가 어디인지 
# 1-2 ) 원하는 패턴의 총 문자 수가 얼마인지 
# 1-3 ) 보고된 결과가 바이트 수로 표현

# 3. 패턴추출 <-> str.extract
mypattern <- regexpr('data',string)
regmatches(string,mypattern)
regmatches(string,mypattern,invert=T)
substr(string,1,3)


# 4. 패턴치환 <-> str.replace 
sub('data', 'DATA', string) 
gsub('data', 'DATA', string) 

# 5. 패턴분할 <-> str_split
strsplit(string, split=' ')

text <- 'The cat sat on the mat.'

unlist(str_extract_all(text, '\\w+'))
# \w 는 숫자 혹은 알파벳으로 표현된 모든 단어 앞의 \는 \w을 위한 escape 표현
unlist(str_extract_all(text, '.at[.]'))
# . 은 임의의 문자를 뜻한다. 그러므로 ㅁatㅁ로 매칭되는 문자 추출
unlist(str_extract_all(text, 'c.+t'))
# c 임의의문자 +최소1회이상 그리고 t 도 들어있는. The는 T 때문에 포함 
unlist(str_extract_all(text, '(c|s)at'))
# | 는 or 을 뜻한다 즉 c나 s 로 시작한 그룹 그리고 at로 끝나는 단어를 찾는 정규표현식
unlist(str_extract_all(text, 'c(..) s\\1'))
# \1은 백레퍼런스 , 
#정규표현식을 이용하여 문자열 패턴을 매칭할 때 한 번 매칭된 것과 동일한 문자열을 나중에 다시 매칭할 수 있다.


#3
library(RCurl)
url <- 'https://www.imdb.com/title/tt0114709/'
html <- getURL(url=rul)
html

#3-1
rating<-unlist(str_extract_all(html,'<span itemprop=("ratingValue")>.*?</span>'))
rating
#3-2
ratingpattern<- regexpr('<span itemprop=("ratingValue")>.*?</span>' ,html)
rating2 <- unlist(regmatches(html,ratingpattern))
rating2

# 4-a
url <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data'
wine <- read.csv(url, header=FALSE , col.names=c('identifier',
                                                 'Alcohol',
                                                 'Malic acid',
                                                 'Ash',
                                                 'Alcalinity of ash',
                                                 'Magnesium',
                                                 'Total pheonls',
                                                 'Flavanoids',
                                                 'Nonflavanoid phenols',
                                                 'Proanthocyanins',
                                                 'Color intensity',
                                                 'Hue',
                                                 'OD280/OD315 of diluted wines',
                                                 'Proline'))

wine <- as.data.frame(wine)
str(wine)
head(wine)

mean(wine$identifier=='1')
mean(wine$identifier=='2')
mean(wine$identifier=='3')
wine$identifier <- as.factor(wine$identifier)

# 4-b
set.seed(123)
train <- sample(nrow(wine), 0.7*nrow(wine))
wine.train <- wine[train,]
wine.test <- wine[-train,]

# 4-c
library(rpart)
wine.dtree <- rpart(identifier~., wine.train , method='class')
wine.dtree
library(rpart.plot)
wine.dtree.pred <- predict(wine.dtree, newdata=wine.test, type='prob')
wine.dtree.pred
wine.dtree.pred <- predict(wine.dtree, newdata=wine.test, type='class')
wine.dtree.pred

table(wine.test$identifier, wine.dtree.pred, dnn=c('Actual', 'Predicted'))

# 4-d
install.packages('caret')
library(caret)
confusionMatrix(table(wine.dtree.pred, wine.test$identifier))
