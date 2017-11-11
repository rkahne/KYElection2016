library(shinydashboard)
library(shiny)
library(leaflet)
library(tidyverse)

load('Election_2016_Leaflets.rda')

ui<-dashboardPage(
  dashboardHeader(title = 'Kentucky 2016 General Election',
                  titleWidth = 600),
  dashboardSidebar(
    selectInput('county', 'Select County', c('Jefferson' = 'jefferson', 
                                             'Fayette' = 'fayette', 
                                             'Boone' = 'boone', 
                                             'Kenton' = 'kenton', 
                                             'Pendleton' = 'pendleton',
                                             'Campbell' = 'campbell'), selected = 'jefferson'),
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
          'KY House - 69 (Partial)' = 'House69_Boone',
          'Boone County Board of Education - 1' = 'BCBOE1',
          'Boone County Board of Education - 2' = 'BCBOE2',
          'Boone County Board of Education - 3' = 'BCBOE3',
          'Walton-Verona School Board' = 'WVBOE',
          'Union City Commissioners' = 'UnionCommissioner',
          'Florence City Council' = 'FlorenceCityCouncil',
          'Walton City Council' = 'WaltonCityCouncil'
        ),
      'President_Boone')
    }else if(input$county == 'kenton'){
      selectInput('election_selection', 'Election',
                  c('President' = 'President_Kenton',
                    'US Senate' = 'USSen_Kenton',
                    'US House - 4 (Partial)' = 'USHouse4_Kenton',
                    'KY Sen - 17 (Partial)' = 'Sen17',
                    'KY Sen - 23' = 'Sen23',
                    'KY House - 61 (Partial)' = 'House61_Kenton',
                    'KY House - 63 (Partial)' = 'House63_Kenton',
                    'KY House - 64 (Partial)' = 'House64',
                    'KY House - 65' = 'House65',
                    'KY House - 69 (Partial)' = 'House69_Kenton',
                    'Kenton County Board of Education - 1' = 'KCBOE1',
                    'Kenton County Board of Education - 2' = 'KCBOE2',
                    'Kenton County Board of Education - 5' = 'KCBOE5',
                    'Erlanger-Elsmere Board of Education' = 'ErlangerElsmereBOE',
                    'Erlanger-Elsmere Board of Education (Unexpired Term)' = 'ErlangerElsmereBOEUE',
                    'Covington Board of Education' = 'CovingtonBOE',
                    'Ludlow Board of Education' = 'LudlowBOE',
                    'Beechwood Board of Education' = 'BeechwoodBOE',
                    'Covington Mayor' = 'CovingtonMayor',
                    'Covington City Commission' = 'CovingtonComission',
                    'Bromley City Council' = 'BromleyCouncil',
                    'Crescent Springs City Council' = 'CrecentSpringsCouncil',
                    'Crestview Hills City Council' = 'CresviewHillsCouncil'
                  ),
                  'President_Kenton')
    }else if(input$county == 'pendleton'){
      selectInput('election_selection', 'Election',
                  c('President' = 'President_Pendleton',
                    'US Senate' = 'USSen_Pendleton',
                    'US House - 4 (Partial)' = 'USHouse4_Pendleton',
                    'KY House - 78 (Partial)' = 'House78',
                    'Falmouth City Council' = 'FalmouthCouncil',
                    'Butler City Council' = 'ButlerCouncil'
                  ),
                  'President_Pendleton')
    }else if(input$county == 'campbell'){
     selectInput('election_selection', 'Election',
                 c('President' = 'President_Campbell',
                   'US Senate' = 'USSen_Campbell',
                   'US House - 4 (Partial)' = 'USHouse4_Campbell',
                   'KY House - 64 (Partial)' =  'House64_Campbell',
                   'KY House - 67' = 'House67',
                   'KY House - 68' = 'House68',
                   'Campbell County Board of Education - 2' = 'CCBOE2',
                   'Campbell County Board of Education - 3' = 'CCBOE3',
                   'Campbell County Board of Education - 5' = 'CCBOE5',
                   'Campbell County District Judge' = 'CCDistrictJudge',
                   'Ft Thomas City Council' = 'FtThomasCouncil',
                   'Highland Heights City Council' = 'HighlandHeightsCouncil',
                   'Newport City Commission' = 'NewportCommission',
                   'Newport Mayor' = 'NewportMayor'
                 ),
                 'President_Campbell') 
    }else input$selected_election == 'President_Jefferson'
  })
    
} 

shinyApp(ui, server)