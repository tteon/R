library(KoNLP)
library(tm)
library(stringr)

mytextlocation <- "C:/Users/user/Documents/covid_after"
covid_after <- VCorpus(DirSource(mytextlocation))
covid_after
# number expression checking
fun_number <- function(x) {
  str_extract_all(x$content, "[[:graph:]]{0,}[[:digit:]]{1,}[[:graph:]]{0,}")
}
covid_digits <-lapply(covid_after, fun_number)
table(unlist(covid_digits))

# punct expression checking
fun_spe_char <- function(x) {
  str_extract_all(x$content, "[[:alnum:]]{1,}[[:punct:]]{1,}[[:alnum:]]{1,}")
}
covid_puncts <- lapply(covid_after, fun_spe_char)
table(unlist(covid_puncts))

# Eng expression checking
fun_english <- function(x) {
  str_extract_all(x$content, "[[:graph:]]{0,}([[:upper:]]{1}|[[:lower:]]{1})[[:lower:]]{0,}[[:graph:]]{0,}")
}
covid_english <- lapply(covid_after, fun_english)
table(unlist(covid_english))

# Preprocessing
# this section use replace " " technique for removing unwanted expression
mytempfunc <- function(myobject,oldexp,newexp){
  newobject <- tm_map(myobject,
                      content_transformer(function(x,pattern){str_replace_all(x,pattern,newexp)}),
                      oldexp)
  newobject
}

covid_corpus <- covid_after 
#특수기호들 제거 
covid_corpus <- mytempfunc(covid_corpus, "‘","")
covid_corpus <- mytempfunc(covid_corpus, "’","")
covid_corpus <- mytempfunc(covid_corpus, "“","")
covid_corpus <- mytempfunc(covid_corpus, "”","")
covid_corpus <- mytempfunc(covid_corpus, "’","")
covid_corpus <- mytempfunc(covid_corpus, '"',"")
covid_corpus <- mytempfunc(covid_corpus, "/","")
covid_corpus <- mytempfunc(covid_corpus, " · ",", ")
covid_corpus <- mytempfunc(covid_corpus, "·",", ")
covid_corpus <- mytempfunc(covid_corpus, "ㆍ",", ")
covid_corpus <- mytempfunc(covid_corpus, "-","")
covid_corpus <- mytempfunc(covid_corpus, "－","")
#?의 경우 공란 대체 
covid_corpus <- mytempfunc(covid_corpus, "\\?"," ")
#괄호속의 표현은 모두 삭제 
covid_corpus <- mytempfunc(covid_corpus, "\\([[:print:]]{1,}\\)","")
#공란 처리 
covid_corpus <- tm_map(covid_corpus, stripWhitespace)

str(covid_corpus)
covid_corpus[[1]]$content

myNounFun <- function(mytext){
  myNounList <- paste(extractNoun(mytext),collapse=' ')
  myNounList
}

covid_noun_corpus <- covid_corpus

for (i in 1:length(covid_corpus)) {
  covid_noun_corpus[[i]]$content <- myNounFun(covid_corpus[[i]]$content)
}

# check all words in corpus

covid_words_nouns <- lapply(covid_noun_corpus,
                            function(x){str_extract_all(x$content,boundary("word"))}
                            )
table(unlist(covid_words_nouns))

#개별숫자들의 경우 추가 삭제하였다.
covid_noun_corpus <- mytempfunc(covid_noun_corpus,"[[:digit:]]{1,}\\,{0,1}[[:digit:]]{0,}","")

# dtm.a(after_covid)
dtm.a <- DocumentTermMatrix(covid_noun_corpus)
dtm.a

# wordcloud

covid_a_word.freq <- apply(dtm.a[,],2,sum)
covid_a_word.freq
covid_a_sort.word.freq <- sort(covid_a_word.freq,decreasing=TRUE)
covid_a_sort.word.freq
covid_a_cumsum.word.freq <- cumsum(covid_a_sort.word.freq)
covid_a_cumsum.word.freq

covid_a_prop.word.freq <- covid_a_cumsum.word.freq/covid_a_cumsum.word.freq[covid_a_cumsum.word.freq]
covid_a_prop.word.freq

#선그래프용 데이터 
myfig <- data.frame(1:length(covid_a_word.freq),covid_a_prop.word.freq)
colnames(myfig) <- c('loc','prop')
#점그래프용 데이터 
myfig2 <- myfig[round(31.5*(0:10)),]
myfig2$lblloc <- paste(10*(1:10),"%th\nposition",sep="")

library(ggplot2)
ggplot()+
  geom_line(data=myfig,aes(x=loc,y=prop),color='red')+
  geom_point(data=myfig2,aes(x=loc,y=prop),size=2,color='blue')+
  scale_x_continuous(breaks=myfig2$loc,labels=myfig2$lblloc)+
  scale_y_continuous(breaks=0.20*(0:5),labels=paste(20*(0:5),"%",sep=""))+
  labs(x='\n단어빈도 정렬 발생위치',y='누적비율\n')+
  theme_classic()





























