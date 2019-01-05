#Link: https://jiayu.shinyapps.io/Final_yu/

library(shiny)
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(leaflet)
library(plotly)

data <- read_xlsx('data.xlsx')
str(data)
names(data)[c(5,6,10)] <- c('Area', 'Density','GDP')
cleanD <- data%>%
  select(
    Country,Capital,Region,Population,Area,Density,GDP,Birthrate,Deathrate,Latitude,Longitude
  )%>%
  separate('Latitude', c('degree', 'minute'), sep='бу')%>%
  separate('Longitude', c('d', 'm'), sep='бу')%>%
  separate('minute', c('minute', 'delete1'), sep="'")%>%
  separate('m', c('m', 'delete2'), sep="'")%>%
  mutate(
    Density = as.numeric(gsub(',','.',Density)),
    Birthrate = as.numeric(gsub(',','.',Birthrate)),
    Deathrate = as.numeric(gsub(',','.',Deathrate)),
    degree = as.numeric(degree),
    minute = as.numeric(minute),
    d = as.numeric(d),
    m = as.numeric(m),
    Lat = degree+ minute/60,
    Lon = d+m/60
  )
ns = grepl('N',cleanD$delete1,ignore.case = T)
cleanD$Lat = ifelse(ns,cleanD$Lat,-cleanD$Lat)
ew = grepl('E',cleanD$delete2,ignore.case = T)
cleanD$Lon = ifelse(ew,cleanD$Lon,-cleanD$Lon)
cleanD = cleanD[,c(-10,-11,-12,-13,-14,-15)]
str(cleanD)






ui <- fluidPage(
  headerPanel("Countries Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput('region','Select a Region', choices=unique(cleanD$Region)),
      sliderInput("range", "Range:",
                  min = 0, max = 150000,
                  value = c(0,150000),step =100)
      
    ), #sidebarPanel
    mainPanel(
      tabsetPanel(
        tabPanel('Population Map',leafletOutput('map1')),
        tabPanel('GDP vs Density',plotlyOutput('myPlot2')),
        tabPanel('Birthrate & Deathrate',plotlyOutput('geomBar')),
        tabPanel('Data',dataTableOutput('data'))
        
      ) #tabsetPanel
    ) #mainPanel
  ) #sidebarLayout
)







server <- function(input,output){
  
  
  output$map1 <- renderLeaflet(
    leaflet() %>%
      addProviderTiles('OpenStreetMap.Mapnik') %>%
      addCircles(data = subset(cleanD, Population/10000>input$range[1] & Population/10000<input$range[2]),
                 lng = ~Lon, lat = ~Lat, weight = 1,radius = ~sqrt(Population) * 30,
                 popup = ~(paste(sep='<br/>',
                                 paste('<b>Country</b>:', subset(cleanD, Population/10000>input$range[1] & Population/10000<input$range[2])$Country),
                                 paste('<b>Capital</b>:', subset(cleanD, Population/10000>input$range[1] & Population/10000<input$range[2])$Capital),
                                 paste('<b>Population</b>:', subset(cleanD, Population/10000>input$range[1] & Population/10000<input$range[2])$Population),
                                 paste('<b>Area</b>:', subset(cleanD, Population/10000>input$range[1] & Population/10000<input$range[2])$Area),
                                 paste('<b>Region</b>:', subset(cleanD, Population/10000>input$range[1] & Population/10000<input$range[2])$Region)))
      )%>%
      setView(0,0,zoom = 1)

  )
  
  output$myPlot2 <- renderPlotly({
    p <- ggplot(subset(cleanD,Region == input$region), aes(x=Density, y=GDP,size=Population,col = GDP))+ 
      geom_point() + 
      labs(x='Population Density(per sq. mi.)', y='GDP($ per capita)') +
      ggtitle('Relationship of GDP and Population Density')
    ggplotly(p)
    } )  

  
  output$geomBar <- renderPlotly({
    barData <- cleanD %>%
      select(Country,Region,Birthrate,Deathrate)%>%
      gather(ratetype,rate,-Country,-Region)
    bar = ggplot(subset(barData,Region == input$region),aes(x=Country, y=rate,fill=ratetype)) +
          geom_bar(stat="identity", position='dodge')+
          ggtitle('rate')+
          theme(axis.text.x = element_text(angle = 60, hjust = 1))
    ggplotly(bar)
  } )  
  
  
  
  output$data <- renderDataTable(cleanD)
  
}





shinyApp(ui,server)
 



