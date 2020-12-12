library(httr)
library(rvest)
library(tm)
library(tidyverse)
library(pushoverr)
library(googlesheets4)

url <- "http://www.customs.go.th/statistic_report.php?show_search=1"
ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36")

gs4_deauth()
update<-read_sheet("https://docs.google.com/spreadsheets/d/1GR_xobGPzgXLBb5enjXS8toEUqmlEejeoGjx052ot7I/edit?usp=sharing",4,col_names = TRUE)
if (update$Thailand=='not updated'){


# specify month
cmth<-Sys.Date() %>% str_remove("\\d{4}-") %>% 
  str_extract("\\d{2}") %>% as.numeric() -1

if(cmth==0){
  mth<-12
} else{
  mth<-cmth
}

#extraction
thimportdata<-POST(url,body = list(
  month=mth, year='2020', top_menu='menu_homepage',left_menu='',
  current_id='', s_page='', order_by='', sort_type='',
  lang='th', ini_menu='', ini_content='', show_search='',
  fix_active='', hierarchy='', xmonth='', tab='by_country',
  global_key='', imex_type='import', tariff_code='27111100000',
  country_code=''),ua) %>%
  httr::content(.,as="parsed") %>% html_text()

#wrangling 
v<-thimportdata %>% 
  str_split(.,"(Baht)") %>% unlist(., use.names=FALSE) %>% .[3] %>%
  str_remove_all(.,"\t") %>% str_remove_all(.,",") %>% 
  str_remove_all(.,"\r") %>% gsub("\n"," ",.) %>% 
  gsub("\\s{2,}"," ",.) %>% gsub(")","",.) %>%
  str_extract_all(.,"\\D+\\s{1,}\\d+\\s\\d+") %>%
  unlist(., use.names=FALSE) %>% trimws() %>% head(.,-1) %>%
  str_remove(.,"\\D\\D\\s")
v1<-str_extract(v,"\\d+\\s\\d+") %>% 
  as.data.frame() %>%
  separate(.,.,sep="\\s",into = c("Weight","Baht")) %>% 
  sapply(.,as.numeric) %>% 
  as.data.frame() 
v1$Weight<-v1$Weight/1000000
Country<-str_extract(v,"\\D+\\s{1,}") %>% trimws()
thimport<-add_column(v1,Country,.before="Weight") %>% 
  filter(!Baht==0 & !Weight==0)

#notification
if (!sum(thimport$Weight)==0){
  pushover(message='Thai imports are updated', 
           user="uccrmx7ajshvdsgbx2e2qy17eorpsx", 
           app="akhzmh5yoco7koy31oos1micwsbxh7")
}
}
