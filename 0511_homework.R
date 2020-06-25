base �Լ�
# string �����Ͽ� ��,���� ����


string <- c("data analytics is useful",
            "business analytics is helpful",
            "visualization of data is interesting fo data scientists")

# 1. ���ϰ��� <-> str.detect
grep('data',string)
grepl('data',string)


# 2. ������ġŽ�� <-> str.locate
regexpr('data',string)
gregexpr('data',string)
# 1-1 ) �ش� ǥ���� ������ ���ڿ� ��ġ�� ������� 
# 1-2 ) ���ϴ� ������ �� ���� ���� ������ 
# 1-3 ) ������ ����� ����Ʈ ���� ǥ��

# 3. �������� <-> str.extract
mypattern <- regexpr('data',string)
regmatches(string,mypattern)
regmatches(string,mypattern,invert=T)
substr(string,1,3)


# 4. ����ġȯ <-> str.replace 
sub('data', 'DATA', string) 
gsub('data', 'DATA', string) 

# 5. ���Ϻ��� <-> str_split
strsplit(string, split=' ')

text <- 'The cat sat on the mat.'

unlist(str_extract_all(text, '\\w+'))
# \w �� ���� Ȥ�� ���ĺ����� ǥ���� ��� �ܾ� ���� \�� \w�� ���� escape ǥ��
unlist(str_extract_all(text, '.at[.]'))
# . �� ������ ���ڸ� ���Ѵ�. �׷��Ƿ� ��at���� ��Ī�Ǵ� ���� ����
unlist(str_extract_all(text, 'c.+t'))
# c �����ǹ��� +�ּ�1ȸ�̻� �׸��� t �� ����ִ�. The�� T ������ ���� 
unlist(str_extract_all(text, '(c|s)at'))
# | �� or �� ���Ѵ� �� c�� s �� ������ �׷� �׸��� at�� ������ �ܾ ã�� ����ǥ����
unlist(str_extract_all(text, 'c(..) s\\1'))
# \1�� �鷹�۷��� , 
#����ǥ������ �̿��Ͽ� ���ڿ� ������ ��Ī�� �� �� �� ��Ī�� �Ͱ� ������ ���ڿ��� ���߿� �ٽ� ��Ī�� �� �ִ�.


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