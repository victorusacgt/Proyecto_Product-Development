library(shiny)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(RMySQL)
library(lubridate)

# Conexion a MySQL
drv <- dbDriver("MySQL")
datos <- dbConnect(drv, user='test', password='test123', host='14682d9d3659', dbname='test')

# Creacion de Tablas
dataC <- dbGetQuery(datos, statement = ('select * from confirmed_table'))
dataD <- dbGetQuery(datos, statement = ('select * from deaths_table'))
dataR <- dbGetQuery(datos, statement = ('select * from recovered_table'))


shinyUI(fluidPage(
    navbarPage("Covid-19 Dashboard", id = 'nav'),
    leafletOutput('map1', width=1350, height=600),
    absolutePanel(id = "controls1", class = "panel panel-default", fixed = TRUE,
                  draggable = FALSE, top = 40, left = "auto", right = 0, bottom = 'auto',
                  width = 330, height = "auto",
                  h3("Statistics"),
                  h4('Global cases'),
                  verbatimTextOutput('text2'),
                  plotOutput("grafica1", height = 200),
                  h4('Top 5 Country Accumulated Cases'),
                  tableOutput('tabla1')
                  ),
    absolutePanel(id = "controls2", class = "panel panel-default", fixed = TRUE,
                  draggable = FALSE, top = 130, left = 'auto', right = "auto", bottom = 'auto',
                  width = 330, height = "auto",
                  h3("Options Panel"),
                  radioButtons('radioButton', 'Select a cathegory:', choices = c('Confirmed', 'Deaths', 'Recovered'), selected = ''),
                  selectInput("select1", "Select a country", dataC['Country/Region']),
                  uiOutput("secondSelection1"),
                  dateInput('dateInput1',
                            'Set a Date:',
                            value = as.Date('2020-11-23'),
                            min = as.Date('2020-1-22'),
                            max = today() + 10,
                            weekstart = 1),
                  h4('Number of Cases'),
                  verbatimTextOutput('text1')
                  )
    )
)
