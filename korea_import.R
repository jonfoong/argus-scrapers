library(httr)
library(tm)
library(tidyverse)
library(pushoverr)
library(rvest)
library(googlesheets4)


url <- "http://www.customs.go.kr/english/ad/tradeCountry/TradeCommodityList.do?mi=8044"
ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")

gs4_deauth()
update<-read_sheet("https://docs.google.com/spreadsheets/d/1GR_xobGPzgXLBb5enjXS8toEUqmlEejeoGjx052ot7I/edit?usp=sharing",4,col_names = TRUE)
if (update$Korea=='not updated'){

# specify month
cmth<-Sys.Date() %>% str_remove("\\d{4}-") %>% 
  str_extract("\\d{2}") %>% as.numeric()-1

if(cmth==0){
  mth<-'12'
} 
if(nchar(cmth)==2){
  mth<-sprintf('%s',cmth)
} else{
  mth<-sprintf('0%s',cmth)
}

#extraction
krimportdata<-POST(url,body = list(
  currentPageNo='1',
  year='2020', month=mth, hsUnit='6', hsCd='27',
  hsCd4='11', hsCd6='11', hsCd10='', eximDitc='01'), ua) %>%
  httr::content(.,as="parsed") %>% html_text()

#wrangling
x<-krimportdata %>% str_split_fixed(.,"Annual Percentagy Change",2) %>% .[2] %>%
  str_remove_all(.,"\n") %>% str_remove_all(.,"\t") %>% 
  str_remove_all(.,"-?\\d+%") %>% str_remove_all(.,"-") %>%
  gsub("\r"," ",.) %>% gsub(",",'',.) %>% 
  removeWords(.,c("USD","weight","Quantity")) %>%
  str_extract_all(.,"\\D+\\s{1,}\\d+\\s\\d+") %>% 
  unlist(., use.names=FALSE) %>% trimws()
x1<-str_extract(x,"\\d+\\s\\d+") %>% 
  as.data.frame() %>%
  separate(.,.,sep="\\s",into = c("USD","Weight")) %>% 
  sapply(.,as.numeric) %>% 
  as.data.frame() %>%
  .[,c(2,1)]
x1$Weight<-x1$Weight/1000000
Country<-str_extract(x,"\\D+\\s{1,}") %>% trimws()
krimport<-add_column(x1,Country,.before="Weight") %>% 
  filter(!USD==0 & !Weight==0)
total<-krimport %>% summarise(weight=sum(Weight),USD=sum(USD))
}

#notification
if (!total$weight==0){
  pushover(message='Korea imports are updated', 
           user="uccrmx7ajshvdsgbx2e2qy17eorpsx", 
           app="akhzmh5yoco7koy31oos1micwsbxh7")
}

