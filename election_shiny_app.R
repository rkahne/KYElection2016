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
jeffco <- readOGR(dsn = '.', layer = 'precinct_ll')

gpclibPermit()

jeffco@data$id <- rownames(jeffco@data)
jeffco.points <- fortify(jeffco, region='id')
jeffco.df <- inner_join(jeffco.points, jeffco@data, by='id')
jeffco.df <- mutate(jeffco.df, region=PRECINCT)

# Load in Elections
elections<-list()
elections<-list(
  President=read_csv('./elections/President.csv'),
  USSen=read_csv('./elections/USSen.csv'),
  USHouse3=read_csv('./elections/USHouseKY3.csv'),
  USHouse4=read_csv('./elections/USHouseKY4.csv'),
  Sen19=read_csv('./elections/Sen19.csv'),
  Sen33=read_csv('./elections/Sen33.csv'),
  Sen35=read_csv('./elections/Sen35.csv'),
  Sen37=read_csv('./elections/Sen37.csv'),
  House28=read_csv('./elections/House28.csv'),
  House29=read_csv('./elections/House29.csv'),
  House30=read_csv('./elections/House30.csv'),
  House31=read_csv('./elections/House31.csv'),
  House32=read_csv('./elections/House32.csv'),
  House33=read_csv('./elections/House33.csv'),
  House34=read_csv('./elections/House34.csv'),
  House35=read_csv('./elections/House35.csv'),
  House36=read_csv('./elections/House36.csv'),
  House37=read_csv('./elections/House37.csv'),
  House38=read_csv('./elections/House38.csv'),
  House40=read_csv('./elections/House40.csv'),
  House41=read_csv('./elections/House41.csv'),
  House42=read_csv('./elections/House42.csv'),
  House43=read_csv('./elections/House43.csv'),
  House44=read_csv('./elections/House44.csv'),
  House46=read_csv('./elections/House46.csv'),
  House48=read_csv('./elections/House48.csv'),
  Council2=read_csv('./elections/Council2.csv'),
  Council4=read_csv('./elections/Council4.csv'),
  Council6=read_csv('./elections/Council6.csv'),
  Council8=read_csv('./elections/Council8.csv'),
  Council10=read_csv('./elections/Council10.csv'),
  Council12=read_csv('./elections/Council12.csv'),
  Council14=read_csv('./elections/Council14.csv'),
  Council16=read_csv('./elections/Council16.csv'),
  Council18=read_csv('./elections/Council18.csv'),
  Council20=read_csv('./elections/Council20.csv'),
  Council22=read_csv('./elections/Council22.csv'),
  Council24=read_csv('./elections/Council24.csv'),
  Council26=read_csv('./elections/Council26.csv'),
  SchoolBoard2=read_csv('./elections/SchoolBoard2.csv'),
  SchoolBoard4=read_csv('./elections/SchoolBoard4.csv'),
  SchoolBoard7=read_csv('./elections/SchoolBoard7.csv')
)

# Function for creating leaflet
create_lou_leaflet <- function(election, candidates, colors){
  jeffco_f <- subset(jeffco, PRECINCT %in% election$Precinct)
  election$winner<-sapply(1:length(election$Precinct), function(i){
    if(which(election[i,2:length(election)] == max(election[i,2:length(election)])) %>% length() > 1){
      'Tie'
    }else names(election[i,2:length(election)])[which(election[i,2:length(election)] == max(election[i,2:length(election)]))] 
  })
  for(i in 1:(length(candidates))){
    jeffco_f[[i+2]]<-sapply(jeffco_f$PRECINCT, function(j) ifelse(j %in% election$Precinct,election[[i+1]][which(election$Precinct == j)],'None'))
  }
  names(jeffco_f)<-c('PRECINCT','id',names(election)[2:(length(names(election))-1)]) # -1 for 'winner' field
  jeffco_f$winner<-sapply(jeffco_f$PRECINCT, function(i) ifelse(i %in% election$Precinct,election$winner[which(election$Precinct == i)],'None')) %>% unlist() %>% as.factor()
  
  pal <- colorFactor(
    sapply(levels(jeffco_f$winner),function(i){
      if(i %in% candidates) colors[which(candidates==i)]
      else 'white'
    }),
    levels(jeffco_f$winner))
  pop<-paste0('<strong>',jeffco_f$PRECINCT,'</strong>')
  if(length(candidates)>1){
    for(i in 1:length(candidates)){
      pop<-paste0(pop,'<br>',candidates[i],':', jeffco_f[[which(names(jeffco_f) == candidates[i])]]) 
    }
  }else  pop<-paste0(pop,'<br>',candidates[1],':', jeffco_f[[which(names(jeffco_f) == candidates[1])]]) 
  
  
  map <- leaflet(data=jeffco_f) %>% 
    addTiles() %>%  
    setView(lng=-85.694006, lat=38.224536, zoom=10) %>% 
    addPolygons(data=jeffco_f, stroke=F, 
                smoothFactor=0.2, 
                fillOpacity = 0.75, 
                color=~pal(winner),
                popup=pop) %>% 
    addPolylines(weight=2, color='black')
}

# Create Leaflets
leaflets<-list(
  President=create_lou_leaflet(elections$President,names(elections$President[2:7]),c('red','blue','yellow','orange','green','purple')),
  USSen=create_lou_leaflet(elections$USSen, names(elections$USSen[2:3]),c('red','blue')),
  USHouse3=create_lou_leaflet(elections$USHouse3, names(elections$USHouse3[2:3]),c('red','blue')),
  USHouse4=create_lou_leaflet(elections$USHouse4, names(elections$USHouse4[2:3]),c('red','blue')),
  Sen19=create_lou_leaflet(elections$Sen19, names(elections$Sen19[2:3]),c('red','blue')),
  Sen33=create_lou_leaflet(elections$Sen33, names(elections$Sen33[2:3]),c('red','blue')),
  Sen35=create_lou_leaflet(elections$Sen35, names(elections$Sen35[2]),'blue'),
  Sen37=create_lou_leaflet(elections$Sen37, names(elections$Sen37[2]),'blue'),
  House28=create_lou_leaflet(elections$House28, names(elections$House28[2:3]),c('red','blue')),
  House29=create_lou_leaflet(elections$House29, names(elections$House29[2]),'red'),
  House30=create_lou_leaflet(elections$House30, names(elections$House30[2:3]),c('red','blue')),
  House31=create_lou_leaflet(elections$House31, names(elections$House31[2:3]),c('red','blue')),
  House32=create_lou_leaflet(elections$House32, names(elections$House32[2]),'red'),
  House33=create_lou_leaflet(elections$House33, names(elections$House33[2:3]),c('red','blue')),
  House34=create_lou_leaflet(elections$House34, names(elections$House34[2]),'blue'),
  House35=create_lou_leaflet(elections$House35, names(elections$House35[2]),'blue'),
  House36=create_lou_leaflet(elections$House36, names(elections$House36[2]),'red'),
  House37=create_lou_leaflet(elections$House37, names(elections$House37[2:3]),c('red','blue')),
  House38=create_lou_leaflet(elections$House38, names(elections$House38[2:3]),c('red','blue')),
  House40=create_lou_leaflet(elections$House40, names(elections$House40[2:3]),c('red','blue')),
  House41=create_lou_leaflet(elections$House41, names(elections$House41[2]),'blue'),
  House42=create_lou_leaflet(elections$House42, names(elections$House42[2:3]),c('red','blue')),
  House43=create_lou_leaflet(elections$House43, names(elections$House43[2:3]),c('red','blue')),
  House44=create_lou_leaflet(elections$House44, names(elections$House44[2]),'blue'),
  House46=create_lou_leaflet(elections$House46, names(elections$House46[2:3]),c('red','blue')),
  House48=create_lou_leaflet(elections$House48, names(elections$House48[2:3]),c('red','blue')),
  Council2=create_lou_leaflet(elections$Council2, names(elections$Council2[2]),'blue'),
  Council4=create_lou_leaflet(elections$Council4, names(elections$Council4[2]),'blue'),
  Council6=create_lou_leaflet(elections$Council6, names(elections$Council6[2]),'blue'),
  Council8=create_lou_leaflet(elections$Council8, names(elections$Council8[2]),'blue'),
  Council10=create_lou_leaflet(elections$Council10, names(elections$Council10[2]),'blue'),
  Council12=create_lou_leaflet(elections$Council12, names(elections$Council12[2]),'blue'),
  Council14=create_lou_leaflet(elections$Council14, names(elections$Council14[2:3]),c('red','blue')),
  Council16=create_lou_leaflet(elections$Council16, names(elections$Council16[2:3]),c('red','blue')),
  Council18=create_lou_leaflet(elections$Council18, names(elections$Council18[2]),'red'),
  Council20=create_lou_leaflet(elections$Council20, names(elections$Council20[2]),'red'),
  Council22=create_lou_leaflet(elections$Council22, names(elections$Council22[2]),'red'),
  Council24=create_lou_leaflet(elections$Council24, names(elections$Council24[2]),'blue'),
  Council26=create_lou_leaflet(elections$Council26, names(elections$Council26[2]),'blue'),
  SchoolBoard2=create_lou_leaflet(elections$SchoolBoard2, names(elections$SchoolBoard2[2:4]),c('purple','yellow','green')),
  SchoolBoard4=create_lou_leaflet(elections$SchoolBoard4, names(elections$SchoolBoard4[2:3]),c('purple','yellow')),
  SchoolBoard7=create_lou_leaflet(elections$SchoolBoard7, names(elections$SchoolBoard7[2:5]),c('purple','yellow','green','orange'))
)


ui<-dashboardPage(
  dashboardHeader(title = 'Jefferson County, Kentucky 2016 General Election',
                  titleWidth = 600),
  dashboardSidebar(
    radioButtons('election_selection', 'Election:',
                 c('President' = 'President',
                   'US Senate' = 'USSen',
                   'US House - 3' = 'USHouse3',
                   'US House - 4 (Partial)' = 'USHouse4',
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
                   'JCPS School Board - 7' = 'SchoolBoard7'), 'President')
  ),
  dashboardBody(
    tags$style(type = "text/css", "#selected_election {height: calc(100vh - 80px) !important;}"),
    leafletOutput('selected_election'), width = 12, height = 12
  )
)

server<-function(input,output){
  output$selected_election<-renderLeaflet({
    leaflets[[which(names(leaflets) == input$election_selection)]]
  })
} 

shinyApp(ui, server)