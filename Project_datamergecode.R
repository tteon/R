# datamerge
dir<-('C:/Users/user/Documents/covid_after')
file_list <- list.files(dir)
data <- data.frame()

for(file in file_list){
  print(file)
  temp <- read.csv(paste(dir, file, sep = "\\"), header=T, sep=',', stringsAsFactors=F)
  data <- rbind(data,temp)
}

# 
library(KoNLP)
library(tm)
library(stringr)

str(data)

covid_after_title <- data$title
str(covid_after_title)
head(covid_after_title)

SimplePos22(covid_after_title)
# imcomplement
corpus_covid_after <- VCorpus(covid_after_title)
corpus_covid_after
#
pattern = "["

str_replace_all(covid_after_title, "’","")
str_replace_all(covid_after_title, "“","")
str_replace_all(covid_after_title, "”","")
str_replace_all(covid_after_title, "’","")
str_replace_all(covid_after_title, '"',"")
str_replace_all(covid_after_title, "/","")
str_replace_all(covid_after_title, " · ",", ")
str_replace_all(covid_after_title, "·",", ")
str_replace_all(covid_after_title, "ㆍ",", ")
str_replace_all(covid_after_title, "-","")
str_replace_all(covid_after_title, "－","")

#?의 경우 공란 대체 
str_replace_all(covid_after_title, "\\?"," ")
#괄호속의 표현은 모두 삭제 
str_replace_all(covid_after_title, "\\([[:print:]]{1,}\\)","")

covid_after_title

?regex
#
mytempfunc <- function(myobject,oldexp,newexp){
  newobject <- tm_map(myobject,
                      content_transformer(function(x,pattern){str_replace_all(x,pattern,newexp)}),
                      oldexp)
  newobject
}
corpus_covid_after <- covid_after_title
str(corpus_covid_after)
#특수기호들 제거 
covid_after_title <- mytempfunc(covid_after_title, "‘","")
mycorpus <- mytempfunc(mycorpus, "’","")
mycorpus <- mytempfunc(mycorpus, "“","")
mycorpus <- mytempfunc(mycorpus, "”","")
mycorpus <- mytempfunc(mycorpus, "’","")
mycorpus <- mytempfunc(mycorpus, '"',"")
mycorpus <- mytempfunc(mycorpus, "/","")
mycorpus <- mytempfunc(mycorpus, " · ",", ")
mycorpus <- mytempfunc(mycorpus, "·",", ")
mycorpus <- mytempfunc(mycorpus, "ㆍ",", ")
mycorpus <- mytempfunc(mycorpus, "-","")
mycorpus <- mytempfunc(mycorpus, "－","")
#?의 경우 공란 대체 
mycorpus <- mytempfunc(mycorpus, "\\?"," ")
#괄호속의 표현은 모두 삭제 
mycorpus <- mytempfunc(mycorpus, "\\([[:print:]]{1,}\\)","")
#공란 처리 
mycorpus <- tm_map(mycorpus, stripWhitespace)

# wordcloud

word.freq <- apply(dtm.k[,],2,sum)
sort.word.freq <- sort(word.freq,decreasing=TRUE)
cumsum.word.freq <- cumsum(sort.word.freq)
prop.word.freq <- cumsum.word.freq/cumsum.word.freq[length(cumsum.word.freq)]
#선그래프용 데이터 
myfig <- data.frame(1:length(word.freq),prop.word.freq)
colnames(myfig) <- c('loc','prop')
#점그래프용 데이터 
myfig2 <- myfig[round(31.5*(0:10)),]
myfig2$lblloc <- paste(10*(1:10),"%th\nposition",sep="")
ggplot()+
  geom_line(data=myfig,aes(x=loc,y=prop),color='red')+
  geom_point(data=myfig2,aes(x=loc,y=prop),size=2,color='blue')+
  scale_x_continuous(breaks=myfig2$loc,labels=myfig2$lblloc)+
  scale_y_continuous(breaks=0.20*(0:5),labels=paste(20*(0:5),"%",sep=""))+
  labs(x='\n단어빈도 정렬 발생위치',y='누적비율\n')+
  theme_classic()



