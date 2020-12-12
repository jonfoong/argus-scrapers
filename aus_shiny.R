library(httr)
library(jsonlite)
library(shiny)
library(plotly)
library(tidyverse)
library(shinythemes)
library(DT)
library(xml2)
library(rvest)

#scrape data from JSON
url<-sprintf("https://aemo.com.au/aemo/api/v1/GasBBReporting/DailyProductionAndFlow?FacilityIds=540093,580010,540101,544261,540047,530030,540083,540096,580020,540071,540077,520075,540059,520054,520090,540094,540098,540080,540090,540086,540050,540097,540055,520047,540089,540070,540092,530071,530042,540088,540075,544253,540061,530038,530039,530040,580100,580040,540064,530043,550050,550045,550046,550054,520053,530061,520060,580050,540084,530041,530044,580060,580070,540065,550052,530060,540058,540085,540102,540073,540057,540095,544260,540110,540040,540082,540072,540062,540103,550061,550060,540060,540066,540067,540076,540068,580210,570050,540051,532005,530110,540045,540046,540091,580030,540069,540087,580180,540074&FromGasDate=%s&ToGasDate=%s",
             (Sys.Date()-365) %>% format(.,"%d/%m/%Y"),Sys.Date() %>% format(.,"%d/%m/%Y"))
rf <- GET(url)
rfc <- content(rf)

#parse JSON to dataframe
df <- do.call(rbind, lapply(rfc$data$DailyProductionAndFlowList, function(x) {
  x[sapply(x, is.null)] <- "NULL"
  as.data.frame(x, stringsAsFactors = FALSE)
}))
df2<-df %>% mutate(Date=as.Date(GasDate, format = "%Y-%m-%d")) %>%
  mutate(HeldInStorage=HeldInStorage%>%as.numeric())

#extract storage data
roma<-df2 %>% filter(FacilityName=="Roma Underground Storage (RUGS)") %>%
  select(Date,HeldInStorage) 
roma[which(roma$Date=="2020-09-04"),2]<-43378
roma[which(roma$Date=="2020-09-05"),2]<-43378
roma <- roma %>% mutate(Levels=round((HeldInStorage*100/50000),1)) %>%
  mutate(facility="roma")

iona<-df2 %>% filter(FacilityName=="Iona Underground Gas Storage") %>%
  select(Date,HeldInStorage) %>% mutate(Levels=round((HeldInStorage*100/26000),1)) %>%
  mutate(facility="iona")
storage<-rbind(roma,iona)

#extract production data
longford <-df2 %>% filter(FacilityName=="Longford Gas Plant") %>%
  select(Date,Supply) %>% mutate(utilisation=round((Supply*100/1115),1))
#extract pipeline data 
msp<-df2 %>% filter(FacilityName=="Moomba to Sydney Pipeline System" &
                      LocationName=="Moomba Hub") %>%
  select(Date,TransferIn) %>% mutate(pipe='msp')
names(msp)[2]<-"flow"

vni<-df2 %>% filter(FacilityName=="Moomba to Sydney Pipeline System" &
                      LocationName=="Culcairn") %>%
  select(Date,TransferOut) %>% mutate(pipe="vni")
names(vni)[2]<-"flow"

swqp<-df2 %>% filter(FacilityName=="South West Queensland Pipeline" &
                       LocationName=="Wallumbilla Hub") %>% 
  mutate(flow=TransferIn-TransferOut,pipe="swqp") %>%
  select(Date,flow,pipe)

aplng<-df2 %>% filter(FacilityName=="APLNG Pipeline" &
                        LocationName=="Curtis Island") %>%
  mutate(flow=Demand,pipe="aplng") %>%
  select(Date,flow,pipe)

qclng<- df2 %>% filter(FacilityName=="Wallumbilla to Gladstone Pipeline" &
                         LocationName=="Curtis Island") %>%
  mutate(flow=Demand,pipe="qclng") %>%
  select(Date,flow,pipe)

glng<- df2 %>% filter(FacilityName=="GLNG Gas Transmission Pipeline" &
                        LocationName=="Curtis Island") %>%
  mutate(flow=Demand,pipe="glng") %>%
  select(Date,flow,pipe)

pipeline <- rbind(swqp,msp,vni) %>% mutate(flow=round(flow,1))
lng<-rbind(aplng,qclng,glng) %>% mutate(flow=round(flow,1))

#extract data for DWGM
dat<-read.csv("http://www.nemweb.com.au/REPORTS/CURRENT/VicGas/INT310_V4_PRICE_AND_WITHDRAWALS_1.CSV") %>%
  mutate(gas_date=as.Date(gas_date, format = "%d %b %Y")) %>%
  filter(schedule_interval==1) %>% .[1:14,]

dat2<-read.csv("http://www.nemweb.com.au/REPORTS/CURRENT/VicGas/INT287_V4_GAS_CONSUMPTION_1.CSV") %>%
  mutate(gas_date=as.Date(gas_date, format = "%d %b %Y")) %>%
  arrange(desc(gas_date)) %>% .[1:14,] %>%
  subset(select=c(total_gas_used))
dat2<-round(dat2/1000,0)
dat2[1,1]<-NA

dat3<-cbind(dat,dat2)

#extract GSH data 
#GSH

url <- "https://nemweb.com.au/Reports/Current/GSH/GSH_Daily_Trans_Summary/"
pg <- read_html(url)
x<-paste("https://nemweb.com.au",html_attr(html_nodes(pg, "a"), "href"),sep = "") %>% .[-1]
y<-x[(sum(x==x)-6):sum(x==x)]
z<-list()
for (i in y){
  temp <- tempfile()
  download.file(i,temp)
  dat <- read.table(unz(temp,str_extract(i,"PUBLIC_DAILYTRANSACTIONSUMMARY_\\d+_\\d+.zip") %>%
                          str_replace_all(.,"zip","CSV")),skip=1,header = T,fill=T,sep = ",")
  z[which(y==i)]<-list(dat[1:nrow(dat)-1,5:ncol(dat)-1])
}
b<-bind_rows(z, .id = "column_label") %>% .[,c(-1,-2,-8,-9,-10,-11)] %>% mutate(CURRENTDATE =as.Date(CURRENTDATE)) %>%
  mutate(FROM_GAS_DATE =as.Date(FROM_GAS_DATE)) %>%
  mutate(TO_GAS_DATE =as.Date(TO_GAS_DATE)) %>% 
  dplyr::arrange(desc(CURRENTDATE))

names(b)[names(b) == "CURRENTDATE"] <- "DATE"
names(b)[names(b) == "VOLUME_WEIGHTED_AVERAGE_PRICE"] <- "VOLUME WEIGHTED AVERAGE PRICE"
names(b)[names(b) == "TOTAL_NUMBER_OF_TRADES"] <- "TOTAL_TRADES"
names(b)[names(b) == "TOTAL_QUANTITY"] <- "QUANTITY"
## UI
ui<-navbarPage(theme = shinytheme("flatly"),
               "East Australia gas dashboard",
               tabPanel("Victoria DWGM",
                        actionButton(inputId = 'onewdwgm',label = '1w'),
                        actionButton(inputId = 'twowdwgm',label = '2w'),
                        downloadButton("downloadData5", " Export"),
                        plotlyOutput("DWGM")),
               tabPanel("Pipeline",
                        actionButton(inputId = 'threemonthpipe',label = '3m'),
                        actionButton(inputId = 'sixmonthpipe',label = '6m'),
                        actionButton(inputId = 'oneyearpipe',label = '1y'),
                        downloadButton("downloadData1", " Export"),
                        plotlyOutput("pipeline")),
               tabPanel("LNG Production",
                        actionButton(inputId = 'threemonthlng',label = '3m'),
                        actionButton(inputId = 'sixmonthlng',label = '6m'),
                        actionButton(inputId = 'oneyearlng',label = '1y'),
                        downloadButton("downloadData2", " Export"),
                        plotlyOutput("lng")),
               tabPanel("Longford Production",
                        actionButton(inputId = 'threemonth',label = '3m'),
                        actionButton(inputId = 'sixmonth',label = '6m'),
                        actionButton(inputId = 'oneyear',label = '1y'),
                        downloadButton("downloadData3", " Export"),
                        plotlyOutput("longford")),
               tabPanel("Storage",
                        actionButton(inputId = 'threemonthstorage',label = '3m'),
                        actionButton(inputId = 'sixmonthstorage',label = '6m'),
                        actionButton(inputId = 'oneyearstorage',label = '1y'),
                        downloadButton("downloadData4", " Export"),
                        plotlyOutput("storage")),
               tabPanel("Gas Supply Hub",
                        downloadButton("downloadData6", " Export"),
                        dataTableOutput("GSH"))
)

server<-function(input,output){
  rv<-reactiveValues(data=longford)
  observeEvent(input$threemonth,ignoreInit = F, {rv$data<-longford[(nrow(longford)-90):nrow(longford),]})
  observeEvent(input$sixmonth,ignoreInit = T, {rv$data<-longford[(nrow(longford)-180):nrow(longford),]})
  observeEvent(input$oneyear,ignoreInit = T,{rv$data<-longford[(nrow(longford)-364):nrow(longford),]})
  output$downloadData3 <- downloadHandler(
    filename = "longford.csv",
    content = function(file) {
      write.csv(rv$data, file)
    }
  )
  output$longford<-renderPlotly({rv$data %>% plot_ly(x=~Date,y=~utilisation,type='scatter',mode='lines')%>%
      layout(hovermode="x unified",title = 'Longford production',
             yaxis = list(title = 'Utilisation rate (%)',
                          zeroline = TRUE)) %>%
      config(displayModeBar = F)
  })
  rv2<-reactiveValues(data=storage)
  observeEvent(input$threemonthstorage,{rv2$data<-storage %>% filter(!Date<max(storage$Date)-90)})
  observeEvent(input$sixmonthstorage,{rv2$data<-storage %>% filter(!Date<max(storage$Date)-180)})
  observeEvent(input$oneyearstorage,{rv2$data<-storage %>% filter(!Date<max(storage$Date)-364)})
  output$downloadData4 <- downloadHandler(
    filename = "storage.csv",
    content = function(file) {
      write.csv(rv2$data, file)
    }
  )
  output$storage<-renderPlotly({rv2$data %>% plot_ly(x=~Date,y=~Levels,color=~facility,type='scatter',mode='lines')%>%
      layout(hovermode="x unified",title = 'Storage',
             yaxis = list(title = 'Level (%)',
                          zeroline = TRUE)) %>%
      config(displayModeBar = F)
  })
  rv3<-reactiveValues(data=pipeline)
  observeEvent(input$threemonthpipe,{rv3$data<-pipeline %>% filter(!Date<max(pipeline$Date)-90)})
  observeEvent(input$sixmonthpipe,{rv3$data<-pipeline %>% filter(!Date<max(pipeline$Date)-180)})
  observeEvent(input$oneyearpipe,{rv3$data<-pipeline %>% filter(!Date<max(pipeline$Date)-364)})
  output$downloadData1 <- downloadHandler(
    filename = "pipeline.csv",
    content = function(file) {
      write.csv(rv3$data, file)
    }
  )
  output$pipeline<-renderPlotly({rv3$data %>% plot_ly(x = ~Date, y = ~flow, color = ~pipe, type = 'scatter', mode = 'lines') %>%
      layout(hovermode="x unified",title = 'Pipeline flows',
             yaxis = list(title = 'Flows (TJ/d)',
                          zeroline = TRUE)) %>%
      config(displayModeBar = F)
  })
  rv5<-reactiveValues(data=lng)
  observeEvent(input$threemonthlng,{rv5$data<-lng %>% filter(!Date<max(lng$Date)-90)})
  observeEvent(input$sixmonthlng,{rv5$data<-lng %>% filter(!Date<max(lng$Date)-180)})
  observeEvent(input$oneyearlng,{rv5$data<-lng %>% filter(!Date<max(lng$Date)-364)})
  output$downloadData2 <- downloadHandler(
    filename = "lng.csv",
    content = function(file) {
      write.csv(rv5$data, file)
    }
  )
  output$lng<-renderPlotly({rv5$data %>% plot_ly(x = ~Date, y = ~flow, color = ~pipe, type = 'scatter', mode = 'lines') %>%
      layout(hovermode="x unified",title = 'LNG production pipeline flows',
             yaxis = list(title = 'Flows (TJ/d)',
                          zeroline = TRUE)) %>%
      config(displayModeBar = F)
  })
  rv4<-reactiveValues(data=dat3)
  observeEvent(input$onewdwgm,{rv4$data<-dat3 %>% filter(!gas_date<max(dat3$gas_date)-6)})
  observeEvent(input$twowdwgm,{rv4$data<-dat3 %>% filter(!gas_date<max(dat3$gas_date)-13)})
  output$downloadData5 <- downloadHandler(
    filename = "dwgm.csv",
    content = function(file) {
      write.csv(rv4$data, file)
    }
  )
  output$DWGM<-renderPlotly({rv4$data %>% plot_ly() %>% 
      add_trace(x = ~gas_date, y = ~total_gas_used, type = 'bar', textposition = "inside",name = 'Demand (TJ/d)',
                marker = list(color = '#C9EFF9'),
                hoverinfo = "text",
                text = ~paste(total_gas_used,'TJ',sep='')) %>% 
      add_trace(x = ~gas_date, y = ~price_value, type = 'scatter', mode = 'lines',name = 'Price (A$/GJ)', yaxis = 'y2',
                line = list(color = '#45171D'),
                hoverinfo = "text",
                text = ~paste('A$',round(price_value,2),sep='')) %>% 
      layout(title = 'Victoria DWGM daily 6am price',
             xaxis = list(type='date',title = "",tickformat = "%d %B<br>(%a)"),
             yaxis = list(side = 'right', title = 'Demand', showgrid = T, zeroline = T, range=c(min(dat3$total_gas_used,na.rm = T)-100,max(dat3$total_gas_used,na.rm = T)+50)),
             yaxis2 = list(side = 'left', overlaying = "y", title = 'Price', showgrid = F, zeroline = T,range=c(min(dat3$price_value,na.rm = T)-1,max(dat3$price_value,na.rm = T)+0.2)),
             legend = list(x = 100, y = 1.2)) %>% 
      config(displayModeBar = F)
  })
  output$downloadData6 <- downloadHandler(
    filename = "gsh.csv",
    content = function(file) {
      write.csv(b, file)
    }
  )
  output$GSH = DT::renderDataTable({
    datatable(b,
              style = "bootstrap")
  })
}
shinyApp(ui, server)

