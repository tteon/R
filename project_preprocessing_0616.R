library(KoNLP)
library(tm)
library(stringr)
mytextlocation <-"C:/Users/user/Documents/news_data"
mypaper <- VCorpus(DirSource(mytextlocation))
mypaper

# 말뭉치 텍스트데이터 사전처리
# 숫자표현은 어떤 것들이 사용되었는지 확인

fun_number <- function(x) {
  str_extract_all(x$content, "[[:graph:]]{0,}[[:digit:]]{1,}[[:graph:]]{0,}")
}
mydigits <- lapply(mypaper, fun_number)
table(unlist(mydigits))

# 특수문자는 어떤 것들이 사용되었고, 그 전후의 표현은 어떤가?
fun_spe_char <- function(x){
  str_extract_all(x$content,"[[:alnum:]]{1,}[[:punct:]]{1,}[[:alnum:]]{1,}")
}
mypuncts <- lapply(mypaper, fun_spe_char)
table(unlist(mypuncts))

# 영문 표현들은 어떤 것들이 사용되었고, 그 전후의 표현은 어떤가?
fun_english <- function(x) {
  str_extract_all(x$content, "[[:graph:]]{0,}([[:upper:]]{1}|[[:lower:]]{1})[[:lower:]]{0,}[[:graph:]]{0,}")
}
myEnglish <- lapply(mypaper, fun_english)
table(unlist(myEnglish))

mytempfunc <- function(myobject,oldexp,newexp){
  newobject <- tm_map(myobject, content_transformer(function(x,pattern){str_replace_all(x,pattern,newexp)}),
                      oldexp)
}
mycorpus <- mypaper
#간단한 사전처리를 실시하자. 
#영문표현들은 모두 삭제하였다(한국어 분석이기 때문에)
mycorpus <- str_remove_all(mycorpus, "[[:lower:]]")
# 특수문자들 제거
mycorpus <- mytempfunc(mycorpus, "‘","")
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

mycorpus[[1]]$content

#명사 추출 후 문서를 명사들의 나열로 바꾸어주는 개인맞춤 함수
myNounFun <- function(mytext){
  myNounList <- paste(extractNoun(mytext),collapse=' ')
  myNounList
}
#말뭉치의 각 문서들에서 명사들만이 나열된 텍스트 추출
myNounCorpus <- mycorpus
for (i in 1:length(mycorpus)){
  myNounCorpus[[i]]$content <- myNounFun(mycorpus[[i]]$content)
}

# 전체 말뭉치 단어를 확인해 보자.
words_nouns <- lapply(myNounCorpus, function(x){str_extract_all(x$content,boundary('word'))})
table(unlist(words_nouns))

# DTM 구축
dtm.k <-DocumentTermMatrix(myNounCorpus)
dtm.k
saveRDS(myNounCorpus, "CorpusK_preprocess.RData")
saveRDS(dtm.k, "dtmkK.RData")








































