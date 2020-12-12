library(rvest)
library(pushoverr)
library(tidyverse)
library(googlesheets4)

# Set Aus time
Sys.setenv(TZ='GMT-10')

url<-"https://aemo.com.au/energy-systems/gas/gas-bulletin-board-gbb/gbb-reports/lng-maintenance"

x<-read_html(url) %>% html_nodes("#content > div > div > div > div > div > div.col-12.col-lg-8.layout-col > div.component.file-list > div > ul > li:nth-child(1) > a > div > div.field-updated.field-publisheddate") %>%
  as.character() %>% str_extract(.,"\\d+/\\d+/\\d+")

y<-read_html(url) %>% html_nodes('#content > div > div > div > div > div > div.col-12.col-lg-8.layout-col > div.component.file-list > div > ul > li:nth-child(2) > a > div > div.field-updated.field-publisheddate > span') %>%
  as.character() %>% str_extract(.,"\\d+/\\d+/\\d+")

z<-read_html(url) %>% html_nodes('#content > div > div > div > div > div > div.col-12.col-lg-8.layout-col > div.component.file-list > div > ul > li:nth-child(3) > a > div > div.field-updated.field-publisheddate > span') %>%
  as.character() %>% str_extract(.,"\\d+/\\d+/\\d+")

#check google sheet
gs4_deauth()
update<-read_sheet("https://docs.google.com/spreadsheets/d/1GR_xobGPzgXLBb5enjXS8toEUqmlEejeoGjx052ot7I/edit?usp=sharing",4,col_names = TRUE)

if (update$Maintenance=='not updated'){

if (format(Sys.Date(),"%d/%m/%Y")==x){
  pushover(message='New maintenance entry!', 
           user="uccrmx7ajshvdsgbx2e2qy17eorpsx", 
           app="akhzmh5yoco7koy31oos1micwsbxh7")
}
if (format(Sys.Date(),"%d/%m/%Y")==y){
  pushover(message='Second maintenance entry today!', 
           user="uccrmx7ajshvdsgbx2e2qy17eorpsx", 
           app="akhzmh5yoco7koy31oos1micwsbxh7")
}
if (format(Sys.Date(),"%d/%m/%Y")==z){
  pushover(message='Third maintenance entry today!', 
           user="uccrmx7ajshvdsgbx2e2qy17eorpsx", 
           app="akhzmh5yoco7koy31oos1micwsbxh7")
}
}
