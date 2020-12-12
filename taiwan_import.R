library(httr)
library(rvest)
library(tidyverse)
library(pushoverr)

url <- "https://portal.sw.nat.gov.tw/APGA/GA30E"
httr::set_config(httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"))

mth<-Sys.Date() %>% str_remove("\\d{4}-") %>% 
  str_extract("\\d{2}") %>% as.numeric() -1

x<-read_html(url) %>% as.character() %>%
  str_extract_all(.,sprintf('name=\"maxMonth\" value=\"%d\"',mth)) %>%
  unlist()

if (is_empty(x)==FALSE){
  pushover(message='Taiwan imports are updated', 
           user="uccrmx7ajshvdsgbx2e2qy17eorpsx", 
           app="akhzmh5yoco7koy31oos1micwsbxh7")
}