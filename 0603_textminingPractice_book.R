library(stringr)
# stripping white space
mytext <- c('software environment', 'software  environment', 'software\tenvironment')
mytext
str_split(mytext,' ')

sapply(str_split(mytext, ' '),length)
sapply(str_split(mytext, ' '),str_length)

mytext.nowhitespace <- str_replace_all(mytext,"[[:space:]]{1,}"," ")
mytext.nowhitespace

sapply(str_split(mytext.nowhitespace, ' '),length)
sapply(str_split(mytext.nowhitespace, ' '),str_length)

# unified these sentences 

mytext <- 'The 45th President of the United States, Donald Trump, states that he knows how to play trump with the former president'
myword <- unlist(str_extract_all(mytext,boundary('word')))
table(myword)

myword <- str_replace(myword,'Trump', 'Trump_unique_')
myword <- str_replace(myword,'States', 'States_unique_')
table(myword)

mytext <- c('He is one of statisticians agreeing that R is the No.1 statistical software.', 'He is one of statisticians agreeing that R is the No. one statistical software.')
str_split(mytext,' ')

mytext2 <- str_split(str_replace_all(mytext,'[[:digit:]]{1,}[[:space:]]{1,}',"")," ")
str_c(mytext2[[1]],collapse=' ')
str_c(mytext2[[2]],collapse=' ')

mytext3 <- str_split(str_replace_all(mytext, '[[:digit:]]{1,}[[:space:]]{1,}', + "_number_"),' ')

mytext <- 'Baek et al. (2014) argued that the state of default-setting is critical for people to protect their own personal privacy on the internet.'
str_split(mytext,'\\. ')
str_split(mytext, ' ')

mytext2 <- str_replace_all(mytext,'-',' ')
mytext2 <- str_replace_all(mytext2, '[[:upper:]]{1}[[:alpha:]]{1,}[[:space:]](etal\\.)[[:space:]]\\([[:digit:]]{4}\\)','_reference_')
mytext2 <- str_replace_all(mytext2,'\\.[[:space:]]{0,}','')
mytext2

mytext <- c('She is an actor', 'She is the actor')
mystopwords <- '(\\ba )|(\\ban )|(\\bthe )'
str_replace_all(mytext,mystopwords,'')

library('tm')
length(stopwords('en'))
length(stopwords('SMART'))
stopwords('en')
stopwords('SMART')

# stemming

mystemmer.func <- function(mytextobject,mystemmer,mystemmed){
   mytext <- str_replace_all(mytext, '(\\bam )|(\\bare )|(\\bis )|(\\bwas )|(\\bbe )','be ') 
}
mytext <- c('I am a boy. You are a boy. He might be a boy.')
mytext.stem <- mystemmer.func(mytext)
table(str_split(mytext,' '))
table(str_split(mytext.stem,' '))

# n-gram

mytext <- 'The United States comprises fifty states. In the United States, each state has its own laws. However, federal law overrides state law in the United States.'
length(table(myword))
sum(table(myword))
# 2-gram
mytext.2gram <- str_replace_all(mytext,'\\bUnited States','United_States')
myword2 <- unlist(str_extract_all(mytext.2gram,boundary('word')))
length(table(myword2))
sum(table(myword2))
# 3-gram
mytext.3gram <- str_replace_all(mytext,'\\b(t|T)he United States','The_United_States')
myword3 <- unlist(str_extract_all(mytext.3gram,boundary('word')))
length(table(myword3))
sum(table(myword3))

# tm library function
library('tm')
my.text.location <- "C:/Users/user/Documents/ymbaek_papers"
mypaper <- VCorpus(DirSource(my.text.location))
mypaper
summary(mypaper)

mypaper[[2]]
mypaper[[2]]$content
mypaper[[2]]$meta
meta(mypaper[[2]],tag='author') <- 'Y. M. Baek'
meta(mypaper[[2]])

'''
tm_map() function list
removeNumbers()
removePunctuation()
stripWhitespace()
removeWords()
stemDocumnet()
content_transformer()
ex_content_transformer(tolower)
'''

# check it , is pre-processing really vadlity ?
myfunc <- function(x) {str_extract_all(x,"[[:alnum:]]{1,}[[:punct:]]{1}?[[:alnum:]]{1,}")}
mypuncts <- lapply(mypaper, myfunc)
table(unlist(mypuncts))

myfunc <- function(x) {str_extract_all(x,"[[:digit:]]{1,}")}
mydigits <- lapply(mypaper, myfunc)
table(unlist(mydigits))

myfunc <- function(x) {str_extract_all(x,"[[:upper:]]{1}[[:alpha:]]{1,}")}
myuppers <- lapply(mypaper, myfunc)
table(unlist(myuppers))

mycorpus <- tm_map(mypaper, removeNumbers)

mytempfunc <- function(myobject,oldexp,newexp){
  newobject <- tm_map(myobject,content_transformer(function(x, pattern) gsub(pattern, newexp, x)), oldexp)
}

mycorpus <- mytempfunc(mycorpus,'-collar','collar')
mycorpus <- mytempfunc(mycorpus,'\\b((c|C)o-)','co')
mycorpus <- mytempfunc(mycorpus,'\\b(c|C)ross-','cross')
mycorpus <- mytempfunc(mycorpus,'e\\.g\\.','for example')
mycorpus <- mytempfunc(mycorpus,'i\\.e\\.','that is')
mycorpus <- mytempfunc(mycorpus,"\\'s","")
mycorpus <- mytempfunc(mycorpus,"s'","s")
mycorpus <- mytempfunc(mycorpus,"ICD-","ICD")
mycorpus <- mytempfunc(mycorpus,"\\b((i|I)nter-)",'inter')
mycorpus <- mytempfunc(mycorpus,"K-pop","kpop")
mycorpus <- mytempfunc(mycorpus,"\\b((m|M)eta-)","meta")
mycorpus <- mytempfunc(mycorpus,"\\b((o|O)pt-)","opt")
mycorpus <- mytempfunc(mycorpus,"\\b((p|P)ost)-","post")
mycorpus <- mytempfunc(mycorpus,"-end","end")
mycorpus <- mytempfunc(mycorpus,"\\b((w|W)ithin-)","within")
mycorpus <- mytempfunc(mycorpus,"=","is the equal")
mycorpus <- mytempfunc(mycorpus,"and/or","and or")
mycorpus <- mytempfunc(mycorpus,"his/her","his her")
mycorpus <- mytempfunc(mycorpus,"-"," ")

mycorpus <- tm_map(mycorpus, removePunctuation)
mycorpus <- tm_map(mycorpus, stripWhitespace)
mycorpus <- tm_map(mycorpus, content_transformer(tolower))
mycorpus <- tm_map(mycorpus, removeWords, words=stopwords('SMART'))
mycorpus <- tm_map(mycorpus, stemDocument, language='en')

mycharfunc <- function(x) {str_extract_all(x,".")}
mywordfunc <- function(x) {str_extract_all(x,boundary('word'))}
#BEFORE
mychar <- lapply(mypaper, mycharfunc)
myuniquechar0 <- length(table(unlist(mychar)))
mytotalchar0 <- sum(table(unlist(mychar)))
myword <- lapply(mypaper, mywordfunc)
myuniqueword0 <- length(table(unlist(myword)))
mytotalword0 <- sum(table(unlist(myword)))
#AFTER
mychar <- lapply(mycorpus, mycharfunc)
myuniquechar1 <- length(table(unlist(mychar)))
mytotalchar1 <- sum(table(unlist(mychar)))
myword <- lapply(mycorpus, mywordfunc)
myuniqueword1 <- length(table(unlist(myword)))
mytotalword1 <- sum(table(unlist(myword)))
#result
results.comparing <- rbind(c(myuniquechar0,myuniquechar1),
                           c(mytotalchar0,mytotalchar1),
                           c(myuniqueword0,myuniqueword1),
                           c(mytotalword0,mytotalword1))
colnames(results.comparing) <- c('before','after')
rownames(results.comparing) <- c('고유문자 수','총 문자 수','고유단어 수','총 단어 수')
results.comparing

#DTM TDM

#TermDocumnetMatrix 를 사용하면 가로줄에는 단어가, 세로줄에는 문서가 배치된다.
dtm.e <- DocumentTermMatrix(mycorpus)
dtm.e

rownames(dtm.e[,])
colnames(dtm.e[,])

inspect(dtm.e[1:3,50:55])

dtm.e.tfidf <- DocumentTermMatrix(mycorpus, control=list(weighting=function(x) weightTfIdf(x, normalize=FALSE)))
dtm.e.tfidf

# TF-IDF comparison
value.tf.dtm <- as.vector(as.matrix(dtm.e[,]))
value.tfidf.dtm <- as.vector(as.matrix((dtm.e.tfidf[,])))
word.label.dtm <- rep(colnames(dtm.e[,]),each=dim(dtm.e[,])[1])
doc.label.dtm <- rep(rownames(dtm.e[,]),dim(dtm.e[,])[2])
mydata <- data.frame(word.label.dtm,doc.label.dtm,value.tf.dtm,value.tfidf.dtm)
colnames(mydata) <- c('word','doc','tf','tfidf')
mydata[120:130,]

cor.test(mydata$tf,mydata$tfidf,method='kendall')
cor.test(mydata$tf[mydata$tf>0],mydata$tfidf[mydata$tfidf>0],method='kendall')

mydata2 <- subset(mydata, tf>0&tfidf>0)
mydata3 <- subset(mydata, tf>median(mydata2$tf) & tfidf<median(mydata2$tfidf))
table(mydata3$word)[table(mydata3$word)>0]

# n-gram
install.packages('RWeka')
library('RWeka')
bigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=3))
mytext <- c('The United States comprises fifty states.','In the United States,each state has its own laws.','However, federal law overrides state law in the United States.')
mytemp <- VCorpus(VectorSource(mytext))
ngram.tdm <- TermDocumentMatrix(mytemp, control=list(tokenize=bigramTokenizer))
bigramlist <- apply(ngram.tdm[,],1,sum)
sort(bigramlist,decreasing=T)

ngram.tdm <- TermDocumentMatrix(mycorpus, control=list(tokenize=bigramTokenizer))
bigramlist <- apply(ngram.tdm[,],1,sum)
sort(bigramlist,decreasing=T)[1:10]









