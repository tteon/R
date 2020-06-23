library(curl)
library(rvest)
library(N2H4)
install.packages("xlsx")
library(xlsx)
install.packages('rJava')
library(rJava)
options(stringsAsFactors = F)

success <- function(res){
  cat("Request done! Status:", res$status, "\n")
  res$content<-iconv(rawToChar(res$content),from="CP949",to="UTF-8")
  data <<- c(data, list(res))
}

failure <- function(msg){
  cat("Oh noes! Request failed!", msg, "\n")
}
cate<-getMainCategory()

subcate<-lapply(cate[,2], getSubCategory)

scate<-c()
for(i in 1:length(subcate)){
  scate<-rbind(scate, data.frame(cate_name=cate[i,1],sid1=cate[i,2],subcate[[i]]))
}

strDate<-as.Date("2020-05-26")
endDate<-as.Date("2020-06-18")

strTime<-Sys.time()
midTime<-Sys.time()

for (date in strDate:endDate){
  date<-gsub("-","",as.character(as.Date(date,origin = "1970-01-01")))
  print(paste0(date," / start Time: ", strTime," / spent Time: ", Sys.time()-midTime," / spent Time at first: ", Sys.time()-strTime))
  midTime<-Sys.time()
  
  pageUrli<-paste0("http://news.naver.com/main/list.nhn?sid1=110&mid=shm&mode=LS2D&date=",date)
  trym<-0
  max<-try(getMaxPageNum(pageUrli, max=1000), silent = T)
  while(trym<=5&&class(max)=="try-error"){
    max<-try(getMaxPageNum(pageUrli, , max=1000), silent = T)
    Sys.sleep(abs(rnorm(1)))
    trym<-trym+1
    print(paste0("try again max num: ",pageUrli))
  }
  
  for (pageNum in 1:max){
    print(paste0(date," / ",pageNum, " / start Time: ", strTime," / spent Time: ", Sys.time()-midTime," / spent Time at first: ", Sys.time()-strTime))
    midTime<-Sys.time()
    pageUrl<-paste0(pageUrli,"&page=",pageNum)
    tryp<-0
    newsList<-try(getUrlListByCategory(pageUrl), silent = T)
    while(tryp<=5&&class(newsList)=="try-error"){
      newsList<-try(getUrlListByCategory(pageUrl), silent = T)
      Sys.sleep(abs(rnorm(1)))
      tryp<-tryp+1
      print(paste0("try again max num: ",pageUrl))
    }      
    pool <- new_pool()
    data <- list()
    sapply(newsList$links, function(x) curl_fetch_multi(x,success,failure))
    res <- multi_run()
    
    if( identical(data, list()) ){
      res <- multi_run()
    }
    
    loc<-sapply(data, function(x) grepl("^http(|s)://news.naver",x$url))
    cont<-sapply(data, function(x) x$content)
    cont<-cont[loc]
    
    titles<-unlist(lapply(cont,function(x) getContentTitle(read_html(x))))
    bodies<-unlist(lapply(cont,function(x) getContentBody(read_html(x))))
    presses<-unlist(lapply(cont,function(x) getContentPress(read_html(x))))
    data<-data.frame(title=titles,press=presses,body=bodies)
    
    dir.create("./data",showWarnings=F)
    write.xlsx(data, file=paste0("./data/news_",date,"_",pageNum,".xlsx"),row.names = F)
  }
}

?write.csv
