library(shinydashboard)
library(shiny)
library(leaflet)
library(rgdal)
library(gpclib)
library(maptools)
library(dplyr)
library(mapproj)
library(choroplethrMaps)
library(gridExtra)
library(readr)
library(R6)
library(choroplethr)
library(raster)
library(tidyverse)

# Load in Shapefile

gpclibPermit()

jeffco <- readOGR(dsn = './shapefiles/jefferson', layer = 'precinct_ll')
jeffco@data$id <- rownames(jeffco@data)
jeffco.points <- fortify(jeffco, region='id')
jeffco.df <- inner_join(jeffco.points, jeffco@data, by='id')
jeffco.df <- mutate(jeffco.df, region=PRECINCT)

fayette <-readOGR(dsn='./shapefiles/fayette', layer='VotingPrecinct')
fayette@data$id <- rownames(fayette@data)
fayette.points <- fortify(fayette, region='id')
fayette.df <- inner_join(fayette.points, fayette@data, by='id')
fayette.df <- mutate(fayette.df, region=CODE)

boone <-readOGR(dsn='./shapefiles/boone', layer='BoonePrecints2')
boone@data$id <- rownames(boone@data)
boone.points <- fortify(boone, region='id')
boone.df <- inner_join(boone.points, boone@data, by='id')
boone.df <- mutate(boone.df, region=PRECINCTCD)

# Load in Elections
elections<-list(
  President_Jefferson=read_csv('./elections/jefferson/President.csv'),
  USSen_Jefferson=read_csv('./elections/jefferson/USSen.csv'),
  USHouse3=read_csv('./elections/jefferson/USHouseKY3.csv'),
  USHouse4_Jefferson=read_csv('./elections/jefferson/USHouseKY4.csv'),
  Sen19=read_csv('./elections/jefferson/Sen19.csv'),
  Sen33=read_csv('./elections/jefferson/Sen33.csv'),
  Sen35=read_csv('./elections/jefferson/Sen35.csv'),
  Sen37=read_csv('./elections/jefferson/Sen37.csv'),
  House28=read_csv('./elections/jefferson/House28.csv'),
  House29=read_csv('./elections/jefferson/House29.csv'),
  House30=read_csv('./elections/jefferson/House30.csv'),
  House31=read_csv('./elections/jefferson/House31.csv'),
  House32=read_csv('./elections/jefferson/House32.csv'),
  House33=read_csv('./elections/jefferson/House33.csv'),
  House34=read_csv('./elections/jefferson/House34.csv'),
  House35=read_csv('./elections/jefferson/House35.csv'),
  House36=read_csv('./elections/jefferson/House36.csv'),
  House37=read_csv('./elections/jefferson/House37.csv'),
  House38=read_csv('./elections/jefferson/House38.csv'),
  House40=read_csv('./elections/jefferson/House40.csv'),
  House41=read_csv('./elections/jefferson/House41.csv'),
  House42=read_csv('./elections/jefferson/House42.csv'),
  House43=read_csv('./elections/jefferson/House43.csv'),
  House44=read_csv('./elections/jefferson/House44.csv'),
  House46=read_csv('./elections/jefferson/House46.csv'),
  House48=read_csv('./elections/jefferson/House48.csv'),
  Council2=read_csv('./elections/jefferson/Council2.csv'),
  Council4=read_csv('./elections/jefferson/Council4.csv'),
  Council6=read_csv('./elections/jefferson/Council6.csv'),
  Council8=read_csv('./elections/jefferson/Council8.csv'),
  Council10=read_csv('./elections/jefferson/Council10.csv'),
  Council12=read_csv('./elections/jefferson/Council12.csv'),
  Council14=read_csv('./elections/jefferson/Council14.csv'),
  Council16=read_csv('./elections/jefferson/Council16.csv'),
  Council18=read_csv('./elections/jefferson/Council18.csv'),
  Council20=read_csv('./elections/jefferson/Council20.csv'),
  Council22=read_csv('./elections/jefferson/Council22.csv'),
  Council24=read_csv('./elections/jefferson/Council24.csv'),
  Council26=read_csv('./elections/jefferson/Council26.csv'),
  SchoolBoard2=read_csv('./elections/jefferson/SchoolBoard2.csv'),
  SchoolBoard4=read_csv('./elections/jefferson/SchoolBoard4.csv'),
  SchoolBoard7=read_csv('./elections/jefferson/SchoolBoard7.csv'),
  President_Fayette=read_csv('./elections/fayette/president_fayette.csv'),
  USSen_Fayette=read_csv('./elections/fayette/ussen_fayette.csv'),
  USHouse6=read_csv('./elections/fayette/uscongky6_fayette.csv'),
  Sen13=read_csv('./elections/fayette/Sen13.csv'),
  House39=read_csv('./elections/fayette/House39.csv'),
  House45=read_csv('./elections/fayette/House45.csv'),
  House56=read_csv('./elections/fayette/House56.csv'),
  House62=read_csv('./elections/fayette/House62.csv'),
  House72=read_csv('./elections/fayette/House72.csv'),
  House75=read_csv('./elections/fayette/House75.csv'),
  House76=read_csv('./elections/fayette/House76.csv'),
  House77=read_csv('./elections/fayette/House77.csv'),
  House79=read_csv('./elections/fayette/House79.csv'),
  House88=read_csv('./elections/fayette/House88.csv'),
  LFUCG1=read_csv('./elections/fayette/LFUCG1.csv'),
  LFUCG2=read_csv('./elections/fayette/LFUCG2.csv'),
  LFUCG3=read_csv('./elections/fayette/LFUCG3.csv'),
  LFUCG4=read_csv('./elections/fayette/LFUCG4.csv'),
  LFUCG5=read_csv('./elections/fayette/LFUCG5.csv'),
  LFUCG6=read_csv('./elections/fayette/LFUCG6.csv'),
  LFUCG7=read_csv('./elections/fayette/LFUCG7.csv'),
  LFUCG8=read_csv('./elections/fayette/LFUCG8.csv'),
  LFUCG9=read_csv('./elections/fayette/LFUCG9.csv'),
  LFUCG10=read_csv('./elections/fayette/LFUCG10.csv'),
  LFUCG11=read_csv('./elections/fayette/LFUCG11.csv'),
  LFUCG12=read_csv('./elections/fayette/LFUCG12.csv'),
  FCBOE1=read_csv('./elections/fayette/FCBOE1.csv'),
  FCBOE3=read_csv('./elections/fayette/FCBOE3.csv'),
  FCBOE5=read_csv('./elections/fayette/FCBOE5.csv'),
  KYSC5=read_csv('./elections/fayette/KYSC5.csv'),
  President_Boone=read_csv('./elections/boone/President.csv'),
  USSen_Boone=read_csv('./elections/boone/USSen_boone.csv'),
  President_Boone=read_csv('./elections/boone/President.csv'),
  USHouse4_Boone=read_csv('./elections/boone/USHouse4_boone.csv'),
  Sen11=read_csv('./elections/boone/Sen11.csv'),
  House60=read_csv('./elections/boone/House60.csv'),
  House61=read_csv('./elections/boone/House61.csv'),
  House63=read_csv('./elections/boone/House63.csv'),
  House66=read_csv('./elections/boone/House66.csv'),
  House69=read_csv('./elections/boone/House69.csv'),
  BCBOE1=read_csv('./elections/boone/BoardOfEducation1.csv'),
  BCBOE2=read_csv('./elections/boone/BoardOfEducation2.csv'),
  BCBOE3=read_csv('./elections/boone/BoardOfEducation3.csv'),
  WVBOE=read_csv('./elections/boone/WaltonVeronaSchoolBoard.csv'),
  UnionCommissioner=read_csv('./elections/boone/UnionCityCommissioners.csv'),
  FlorenceCityCouncil=read_csv('./elections/boone/FlorenceCityCouncil.csv'),
  WaltonCityCouncil=read_csv('./elections/boone/WaltonCityCouncil.csv')
)

# Function for creating leaflet
create_leaflet <- function(election, candidates, colors, county){
  if(county=='jefferson'){
    shape <- subset(jeffco, PRECINCT %in% election$Precinct)
    shape$precinct <- shape$PRECINCT
    lng_lat<-c(-85.694006,38.224536)
  }else if(county=='fayette'){
    shape <-subset(fayette, CODE %in% election$Precinct, select=c('CODE','OBJECTID'))
    shape$precinct <- shape$CODE
    lng_lat<-c(-84.5037,38.0406)
  }else if(county=='boone'){
    shape<-subset(boone, PRECINCTCD %in% election$Precinct, select=c('PRECINCTCD','id'))
    shape$precinct <- shape$PRECINCTCD
    lng_lat<-c(-84.7316,38.9941)
  }
  election$winner<-sapply(1:length(election$Precinct), function(i){
    if(which(election[i,2:length(election)] == max(election[i,2:length(election)])) %>% length() > 1){
      'Tie'
    }else names(election[i,2:length(election)])[which(election[i,2:length(election)] == max(election[i,2:length(election)]))] 
  })
  for(i in 1:(length(candidates))){
    shape[[i+3]]<-sapply(shape$precinct, function(j) ifelse(j %in% election$Precinct,election[[i+1]][which(election$Precinct == j)],'None'))
  }
  names(shape)<-c('Precinct','id','Precinct2',names(election)[2:(length(names(election))-1)]) # -1 for 'winner' field
  shape$winner<-sapply(shape$Precinct, function(i) ifelse(i %in% election$Precinct,election$winner[which(election$Precinct == i)],'None')) %>% unlist() %>% as.factor()
  
  pal <- colorFactor(
    sapply(levels(shape$winner),function(i){
      if(i %in% candidates) colors[which(candidates==i)]
      else 'white'
    }),
    levels(shape$winner))
  pop<-paste0('<strong>',shape$Precinct,'</strong>')
  if(length(candidates)>1){
    for(i in 1:length(candidates)){
      pop<-paste0(pop,'<br>',candidates[i],':', shape[[which(names(shape) == candidates[i])]]) 
    }
  }else  pop<-paste0(pop,'<br>',candidates[1],':', shape[[which(names(shape) == candidates[1])]]) 
  
  
  map <- leaflet(data=shape) %>% 
    addTiles() %>%  
    setView(lng=lng_lat[1], lat=lng_lat[2], zoom=10) %>% 
    addPolygons(data=shape, stroke=F, 
                smoothFactor=0.2, 
                fillOpacity = 0.75, 
                color=~pal(winner),
                popup=pop) %>% 
    addPolylines(weight=2, color='black')
}


# Create Leaflets
leaflets<-list(
  President_Jefferson=create_leaflet(elections$President_Jefferson,names(elections$President_Jefferson[2:7]),c('red','blue','yellow','orange','green','purple'), 'jefferson'),
  USSen_Jefferson=create_leaflet(elections$USSen_Jefferson, names(elections$USSen_Jefferson[2:3]),c('red','blue'), 'jefferson'),
  USHouse3=create_leaflet(elections$USHouse3, names(elections$USHouse3[2:3]),c('red','blue'), 'jefferson'),
  USHouse4_Jefferson=create_leaflet(elections$USHouse4_Jefferson, names(elections$USHouse4_Jefferson[2:3]),c('red','blue'), 'jefferson'),
  Sen19=create_leaflet(elections$Sen19, names(elections$Sen19[2:3]),c('red','blue'), 'jefferson'),
  Sen33=create_leaflet(elections$Sen33, names(elections$Sen33[2:3]),c('red','blue'), 'jefferson'),
  Sen35=create_leaflet(elections$Sen35, names(elections$Sen35[2]),'blue', 'jefferson'),
  Sen37=create_leaflet(elections$Sen37, names(elections$Sen37[2]),'blue', 'jefferson'),
  House28=create_leaflet(elections$House28, names(elections$House28[2:3]),c('red','blue'), 'jefferson'),
  House29=create_leaflet(elections$House29, names(elections$House29[2]),'red', 'jefferson'),
  House30=create_leaflet(elections$House30, names(elections$House30[2:3]),c('red','blue'), 'jefferson'),
  House31=create_leaflet(elections$House31, names(elections$House31[2:3]),c('red','blue'), 'jefferson'),
  House32=create_leaflet(elections$House32, names(elections$House32[2]),'red', 'jefferson'),
  House33=create_leaflet(elections$House33, names(elections$House33[2:3]),c('red','blue'), 'jefferson'),
  House34=create_leaflet(elections$House34, names(elections$House34[2]),'blue', 'jefferson'),
  House35=create_leaflet(elections$House35, names(elections$House35[2]),'blue', 'jefferson'),
  House36=create_leaflet(elections$House36, names(elections$House36[2]),'red', 'jefferson'),
  House37=create_leaflet(elections$House37, names(elections$House37[2:3]),c('red','blue'), 'jefferson'),
  House38=create_leaflet(elections$House38, names(elections$House38[2:3]),c('red','blue'), 'jefferson'),
  House40=create_leaflet(elections$House40, names(elections$House40[2:3]),c('red','blue'), 'jefferson'),
  House41=create_leaflet(elections$House41, names(elections$House41[2]),'blue', 'jefferson'),
  House42=create_leaflet(elections$House42, names(elections$House42[2:3]),c('red','blue'), 'jefferson'),
  House43=create_leaflet(elections$House43, names(elections$House43[2:3]),c('red','blue'), 'jefferson'),
  House44=create_leaflet(elections$House44, names(elections$House44[2]),'blue', 'jefferson'),
  House46=create_leaflet(elections$House46, names(elections$House46[2:3]),c('red','blue'), 'jefferson'),
  House48=create_leaflet(elections$House48, names(elections$House48[2:3]),c('red','blue'), 'jefferson'),
  Council2=create_leaflet(elections$Council2, names(elections$Council2[2]),'blue', 'jefferson'),
  Council4=create_leaflet(elections$Council4, names(elections$Council4[2]),'blue', 'jefferson'),
  Council6=create_leaflet(elections$Council6, names(elections$Council6[2]),'blue', 'jefferson'),
  Council8=create_leaflet(elections$Council8, names(elections$Council8[2]),'blue', 'jefferson'),
  Council10=create_leaflet(elections$Council10, names(elections$Council10[2]),'blue', 'jefferson'),
  Council12=create_leaflet(elections$Council12, names(elections$Council12[2]),'blue', 'jefferson'),
  Council14=create_leaflet(elections$Council14, names(elections$Council14[2:3]),c('red','blue'), 'jefferson'),
  Council16=create_leaflet(elections$Council16, names(elections$Council16[2:3]),c('red','blue'), 'jefferson'),
  Council18=create_leaflet(elections$Council18, names(elections$Council18[2]),'red', 'jefferson'),
  Council20=create_leaflet(elections$Council20, names(elections$Council20[2]),'red', 'jefferson'),
  Council22=create_leaflet(elections$Council22, names(elections$Council22[2]),'red', 'jefferson'),
  Council24=create_leaflet(elections$Council24, names(elections$Council24[2]),'blue', 'jefferson'),
  Council26=create_leaflet(elections$Council26, names(elections$Council26[2]),'blue', 'jefferson'),
  SchoolBoard2=create_leaflet(elections$SchoolBoard2, names(elections$SchoolBoard2[2:4]),c('purple','yellow','green'), 'jefferson'),
  SchoolBoard4=create_leaflet(elections$SchoolBoard4, names(elections$SchoolBoard4[2:3]),c('purple','yellow'), 'jefferson'),
  SchoolBoard7=create_leaflet(elections$SchoolBoard7, names(elections$SchoolBoard7[2:5]),c('purple','yellow','green','orange'), 'jefferson'),
  President_Fayette=create_leaflet(elections$President_Fayette, names(elections$President_Fayette)[2:7],c('red','blue','yellow','orange','green','purple'), 'fayette'),
  USSen_Fayette=create_leaflet(elections$USSen_Fayette, names(elections$USSen_Fayette[2:3]),c('red','blue'), 'fayette'),
  USHouse6=create_leaflet(elections$USHouse6, names(elections$USHouse6[2:3]),c('red','blue'), 'fayette'),
  Sen13=create_leaflet(elections$Sen13, names(elections$Sen13[2]),'blue', 'fayette'),
  House39=create_leaflet(elections$House39, names(elections$House39[2:3]),c('red','blue'), 'fayette'),
  House45=create_leaflet(elections$House45, names(elections$House45[2:3]),c('red','blue'), 'fayette'),
  House56=create_leaflet(elections$House56, names(elections$House56[2:3]),c('red','blue'), 'fayette'),
  House62=create_leaflet(elections$House62, names(elections$House62[2:3]),c('red','blue'), 'fayette'),
  House72=create_leaflet(elections$House72, names(elections$House72[2:3]),c('red','blue'), 'fayette'),
  House75=create_leaflet(elections$House75, names(elections$House75[2:3]),c('red','blue'), 'fayette'),
  House76=create_leaflet(elections$House76, names(elections$House76[2]),'blue', 'fayette'),
  House77=create_leaflet(elections$House77, names(elections$House77[2]),'blue', 'fayette'),
  House79=create_leaflet(elections$House79, names(elections$House79[2:3]),c('red','blue'), 'fayette'),
  House88=create_leaflet(elections$House88, names(elections$House88[2]),'red', 'fayette'),
  LFUCG1=create_leaflet(elections$LFUCG1, names(elections$LFUCG1[2]),'purple', 'fayette'),
  LFUCG2=create_leaflet(elections$LFUCG2, names(elections$LFUCG2[2:3]),c('purple','yellow'), 'fayette'),
  LFUCG3=create_leaflet(elections$LFUCG3, names(elections$LFUCG3[2]),'purple', 'fayette'),
  LFUCG4=create_leaflet(elections$LFUCG4, names(elections$LFUCG4[2:3]),c('purple','yellow'), 'fayette'),
  LFUCG5=create_leaflet(elections$LFUCG5, names(elections$LFUCG5[2]),'purple', 'fayette'),
  LFUCG6=create_leaflet(elections$LFUCG6, names(elections$LFUCG6[2]),'purple', 'fayette'),
  LFUCG7=create_leaflet(elections$LFUCG7, names(elections$LFUCG7[2]),'purple', 'fayette'),
  LFUCG8=create_leaflet(elections$LFUCG8, names(elections$LFUCG8[2]),'purple', 'fayette'),
  LFUCG9=create_leaflet(elections$LFUCG9, names(elections$LFUCG9[2]),'purple', 'fayette'),
  LFUCG10=create_leaflet(elections$LFUCG10, names(elections$LFUCG10[2]),'purple', 'fayette'),
  LFUCG11=create_leaflet(elections$LFUCG11, names(elections$LFUCG11[2]),'purple', 'fayette'),
  LFUCG12=create_leaflet(elections$LFUCG12, names(elections$LFUCG12[2:3]),c('purple','yellow'), 'fayette'),
  FCBOE1=create_leaflet(elections$FCBOE1, names(elections$FCBOE1[2:3]),c('purple','yellow'), 'fayette'),
  FCBOE3=create_leaflet(elections$FCBOE3, names(elections$FCBOE3[2]),'purple', 'fayette'),
  FCBOE5=create_leaflet(elections$FCBOE5, names(elections$FCBOE5[2:3]),c('purple','yellow'), 'fayette'),
  KYSC5=create_leaflet(elections$KYSC5, names(elections$KYSC5[2:3]),c('purple','yellow'), 'fayette'),
  President_Boone=create_leaflet(elections$President_Boone,names(elections$President_Boone[2:7]),c('red','blue','yellow','orange','green','purple'), 'boone'),
  USSen_Boone=create_leaflet(elections$USSen_Boone, names(elections$USSen_Boone[2:3]),c('red','blue'), 'boone'),
  USHouse4_Boone=create_leaflet(elections$USHouse4_Boone, names(elections$USHouse4_Boone[2:3]),c('red','blue'), 'boone'),
  Sen11=create_leaflet(elections$Sen11, names(elections$Sen11[2]),'red', 'boone'),
  House60=create_leaflet(elections$House60, names(elections$House60[2]),'red', 'boone'),
  House61=create_leaflet(elections$House61, names(elections$House61[2:3]),c('red','blue'), 'boone'),
  House63=create_leaflet(elections$House63, names(elections$House63[2]),'red', 'boone'),
  House66=create_leaflet(elections$House66, names(elections$House66[2]),'red', 'boone'),
  House69=create_leaflet(elections$House69, names(elections$House69[2]),'red', 'boone'),
  BCBOE1=create_leaflet(elections$BCBOE1, names(elections$BCBOE1[2]),'purple', 'boone'),
  BCBOE2=create_leaflet(elections$BCBOE2, names(elections$BCBOE2[2]),'purple', 'boone'),
  BCBOE3=create_leaflet(elections$BCBOE3, names(elections$BCBOE3[2]),'purple', 'boone'),
  WVBOE=create_leaflet(elections$WVBOE, names(elections$WVBOE[2:6]),c('purple','yellow','green','orange', 'brown'), 'boone'),
  UnionCommissioner=create_leaflet(elections$UnionCommissioner, names(elections$UnionCommissioner[2:6]),c('purple','yellow','green','orange', 'brown'), 'boone'),
  FlorenceCityCouncil=create_leaflet(elections$FlorenceCityCouncil, names(elections$FlorenceCityCouncil[2:12]),c('purple','yellow','green','orange', 'brown', 'hotpink','deepskyblue','burlywood','turquoise','darkslategray','yellowgreen'), 'boone'),
  WaltonCityCouncil=create_leaflet(elections$WaltonCityCouncil, names(elections$WaltonCityCouncil[2:8]),c('purple','yellow','green','orange', 'brown', 'hotpink','deepskyblue'), 'boone')
)


ui<-dashboardPage(
  dashboardHeader(title = 'Kentucky 2016 General Election',
                  titleWidth = 600),
  dashboardSidebar(
    selectInput('county', 'Select County', c('Jefferson' = 'jefferson', 'Fayette' = 'fayette', 'Boone' = 'boone'), selected = 'jefferson'),
    uiOutput('selectElections')
  ),
  dashboardBody(
      tags$style(type = "text/css", 
                 "#selected_election {height: calc(100vh - 80px) !important;}",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      leafletOutput('selected_election'), width = 12, height = 12
  )
)

server<-function(input,output){
  output$selected_election<-renderLeaflet({
    leaflets[[which(names(leaflets) == input$election_selection)]]
  })
  
  output$selectElections<-renderUI({
    if(input$county == 'jefferson'){
        selectInput('election_selection', 'Election:',
                    c('President' = 'President_Jefferson',
                      'US Senate' = 'USSen_Jefferson',
                      'US House - 3' = 'USHouse3',
                      'US House - 4 (Partial)' = 'USHouse4_Jefferson',
                      'KY Sen - 19' = 'Sen19',
                      'KY Sen - 33' = 'Sen33',
                      'KY Sen - 35' = 'Sen35',
                      'KY Sen - 37' = 'Sen37',
                      'KY House - 28' = 'House28',
                      'KY House - 29' = 'House29',
                      'KY House - 30' = 'House30',
                      'KY House - 31' = 'House31',
                      'KY House - 32' = 'House32',
                      'KY House - 33 (Partial)' = 'House33',
                      'KY House - 34' = 'House34',
                      'KY House - 35' = 'House35',
                      'KY House - 36 (Partial)' = 'House36',
                      'KY House - 37' = 'House37',
                      'KY House - 38' = 'House38',
                      'KY House - 40' = 'House40',
                      'KY House - 41' = 'House41',
                      'KY House - 42' = 'House42',
                      'KY House - 43' = 'House43',
                      'KY House - 44' = 'House44',
                      'KY House - 46' = 'House46',
                      'KY House - 48' = 'House48',
                      'Metro Council - 2' = 'Council2',
                      'Metro Council - 4' = 'Council4',
                      'Metro Council - 6' = 'Council6',
                      'Metro Council - 8' = 'Council8',
                      'Metro Council - 10' = 'Council10',
                      'Metro Council - 12' = 'Council12',
                      'Metro Council - 14' = 'Council14',
                      'Metro Council - 16' = 'Council16',
                      'Metro Council - 18' = 'Council18',
                      'Metro Council - 20' = 'Council20',
                      'Metro Council - 22' = 'Council22',
                      'Metro Council - 24' = 'Council24',
                      'Metro Council - 26' = 'Council26',
                      'JCPS School Board - 2' = 'SchoolBoard2',
                      'JCPS School Board - 4' = 'SchoolBoard4',
                      'JCPS School Board - 7' = 'SchoolBoard7'), 
                  'President_Jefferson')
    }else if(input$county == 'fayette'){
      selectInput('election_selection', 'Election',
                  c('President' = 'President_Fayette',
                    'US Senate' = 'USSen_Fayette',
                    'US House - 6 (Partial)' = 'USHouse6',
                    'KY Sen - 13' ='Sen13',
                    'KY House - 39 (Partial)' = 'House39',
                    'KY House - 45' = 'House45',
                    'KY House - 56 (Partial)' = 'House56',
                    'KY House - 62 (Partial)' = 'House62',
                    'KY House - 72 (Partial)' = 'House72',
                    'KY House - 75' = 'House75',
                    'KY House - 76' = 'House76',
                    'KY House - 77' = 'House77',
                    'KY House - 79' = 'House79',
                    'KY House - 88' = 'House88',
                    'LFUCG Council - 1' = 'LFUCG1',
                    'LFUCG Council - 2' = 'LFUCG2',
                    'LFUCG Council - 3' = 'LFUCG3',
                    'LFUCG Council - 4' = 'LFUCG4',
                    'LFUCG Council - 5' = 'LFUCG5',
                    'LFUCG Council - 6' = 'LFUCG6',
                    'LFUCG Council - 7' = 'LFUCG7',
                    'LFUCG Council - 8' = 'LFUCG8',
                    'LFUCG Council - 9' = 'LFUCG9',
                    'LFUCG Council - 10' = 'LFUCG10',
                    'LFUCG Council - 11' = 'LFUCG11',
                    'LFUCG Council - 12' = 'LFUCG12',
                    'Fayette Board of Education - 1' = 'FCBOE1',
                    'Fayette Board of Education - 3' = 'FCBOE3',
                    'Fayette Board of Education - 5' = 'FCBOE5',
                    'KY Supreme Court - 5' = 'KYSC5'),
                  'President_Fayette')
    }else if(input$county == 'boone'){
      selectInput('election_selection', 'Election',
        c('President' = 'President_Boone',
          'US Senate' ='USSen_Boone',
          'US House - 4 (Partial)' = 'USHouse4_Boone',
          'KY Sen - 11' = 'Sen11',
          'KY House - 60' = 'House60',
          'KY House - 61 (Partial)' = 'House61',
          'KY House - 63 (Partial)' = 'House63',
          'KY House - 66' = 'House66',
          'KY House - 69 (Partial)' = 'House69',
          'Boone County Board of Education - 1' = 'BCBOE1',
          'Boone County Board of Education - 2' = 'BCBOE2',
          'Boone County Board of Education - 3' = 'BCBOE3',
          'Walton-Verona School Board' = 'WVBOE',
          'Union City Commissioners' = 'UnionCommissioner',
          'Florence City Council' = 'FlorenceCityCouncil',
          'Walton City Council' = 'WaltonCityCouncil'
        ),
      'President_Boone')
    }    else input$selected_election == 'President_Jefferson'
  })
    
} 

shinyApp(ui, server)