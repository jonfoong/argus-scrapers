library(httr)
library(htmlunit)
library(rvest)
library(tidyverse)
library(pushoverr)

url <- "http://43.248.49.97/queryDataForEN/queryDataByWhereEn"
httr::set_config(httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.97 Safari/537.36"))
mth<-3

y<-hu_read_html(url) %>%
  html_node(.,xpath = "/html/body/div/form/table/tbody/tr[3]/td[2]/div[1]/select[2]]")

library(htmlunit)
x<-web_client(
  emulate = c("chrome"),
  proxy_host = NULL,
  proxy_port = NULL
)

wc_go(x, url)







if (str_detect(y,as.character(mth))==TRUE){
  pushover(message='China imports are updated', 
           user="uccrmx7ajshvdsgbx2e2qy17eorpsx", 
           app="akhzmh5yoco7koy31oos1micwsbxh7")
} else {
  pushover(message='China imports are NOT updated', 
           user="uccrmx7ajshvdsgbx2e2qy17eorpsx", 
           app="akhzmh5yoco7koy31oos1micwsbxh7")
}
