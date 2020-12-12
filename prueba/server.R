
library(shiny)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(RMySQL)

# Conexion a MySQL
drv <- dbDriver("MySQL")
datos <- dbConnect(drv, user='test', password='test123', host='14682d9d3659', dbname='test')

# Creacion de Tablas
dataC <- dbGetQuery(datos, statement = ('select * from confirmed_table'))
dataD <- dbGetQuery(datos, statement = ('select * from deaths_table'))
dataR <- dbGetQuery(datos, statement = ('select * from recovered_table'))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Select2 Dependiente
    output$secondSelection1 <- renderUI({
        selectInput('select2', 'Select a province', filter(dataC, `Country/Region`==input$select1)['Province/State'], selected = '')
    })
    
    # Datasets con cambio nombre en ultima columna
    dataC1 <- dataC
    colnames(dataC1)[ncol(dataC1)] <- 'ultima'
    
    dataD1 <- dataD
    colnames(dataD1)[ncol(dataD1)] <- 'ultima'
    
    dataR1 <- dataR
    colnames(dataR1)[ncol(dataR1)] <- 'ultima'
    
    # Creacion del mapa
    output$map1 <- renderLeaflet({
        leaflet(dataC1, options = leafletOptions(worldCopyJump = T)) %>% addTiles() %>%
            fitBounds(~min(Long), ~min(Lat), ~max(Long), ~max(Lat))
    })
    
    # Mapas con Burbujas
    observeEvent(input$radioButton, {
        if (input$radioButton == 'Confirmed'){
            pal <- colorNumeric('Set2', dataC1[ncol(dataC1)])
            leafletProxy("map1", data = dataC1) %>%
                clearShapes() %>%
                addCircles(radius = ~ultima/7, weight = 1, color = "#777777",
                           fillColor = ~pal(ultima), fillOpacity = 0.7, 
                           popup = ~paste(sep = "<br/>", "<b> Confirmed cases: </a></b>", ultima), 
                           label = ~paste(`Country/Region`)
                )
        } else if (input$radioButton == 'Deaths'){
            pal <- colorNumeric('Set1', dataD1[ncol(dataD1)])
            leafletProxy("map1", data = dataD1) %>%
                clearShapes() %>%
                addCircles(radius = ~ultima*8, weight = 1, color = "#777777",
                           fillColor = ~pal(ultima), fillOpacity = 0.7, 
                           popup = ~paste(sep = "<br/>", "<b> Deaths cases: </a></b>", ultima), 
                           label = ~paste(`Country/Region`)
                )
        } else if (input$radioButton == 'Recovered'){
            pal <- colorNumeric('Spectral', dataR1[ncol(dataR1)])
            leafletProxy("map1", data = dataR1) %>%
                clearShapes() %>%
                addCircles(radius = ~ultima/5, weight = 1, color = "#777777",
                           fillColor = ~pal(ultima), fillOpacity = 0.7, 
                           popup = ~paste(sep = "<br/>", "<b> Recovered cases: </a></b>", ultima), 
                           label = ~paste(`Country/Region`)
                )
        } 
    } 
    )
    
    # Despliegue numero de casos por fecha
    observeEvent(input$radioButton, {
        
        output$text1 <- renderText({
            d <- substr(as.character(input$dateInput1), 9, 10) #dia
            if (substr(d, 1,1) == '0') {
                d <- substr(d, 2, 2)
            } else {
                d <- substr(d, 1, 2)
            }
            
            m <- substr(as.character(input$dateInput1), 6, 7) #mes
            if (substr(m, 1,1) == '0') {
                m <- substr(m, 2, 2)
            } else {
                m <- substr(m, 1, 2)
            }
            
            a <- substr(as.character(input$dateInput1), 1, 2) #ano
            
            fecha <- paste0(m, '/', d, '/', a)
            if (input$radioButton == 'Confirmed'){
                if (input$select2 == 'NA'){
                nombres_col <- colnames(dataC)
                num_casos <- filter(dataC, `Country/Region`==input$select1)[nombres_col == fecha][[1]]
                print(paste0('Accumulative ', input$radioButton, ' Cases ', input$select1, '\n', format(round(as.numeric(num_casos), 1), nsmall=0, big.mark=",")))
                } else {
                    nombres_col <- colnames(dataC)
                    num_casos <- filter(dataC, `Country/Region`==input$select1 & `Province/State` == input$select2)[nombres_col == fecha][[1]]
                    print(paste0('Accumulative ', input$radioButton, ' Cases ', input$select1, ' ', input$select2, '\n', format(round(as.numeric(num_casos), 1), nsmall=0, big.mark=",")))
                }
            } else if (input$radioButton == 'Deaths'){
                if (input$select2 == 'NA'){
                    nombres_col <- colnames(dataD)
                    num_casos <- filter(dataD, `Country/Region`==input$select1)[nombres_col == fecha][[1]]
                    print(paste0('Accumulative ', input$radioButton, ' Cases ', input$select1, '\n', format(round(as.numeric(num_casos), 1), nsmall=0, big.mark=",")))
                } else {
                nombres_col <- colnames(dataD)
                num_casos <- filter(dataD, `Country/Region`==input$select1 & `Province/State` == input$select2)[nombres_col == fecha][[1]]
                print(paste0('Accumulative ', input$radioButton, ' Cases ', input$select1, ' ', input$select2, '\n', format(round(as.numeric(num_casos), 1), nsmall=0, big.mark=",")))
                }
            } else if (input$radioButton == 'Recovered'){
                if (input$select2 == 'NA'){
                    nombres_col <- colnames(dataR)
                    num_casos <- filter(dataR, `Country/Region`==input$select1)[nombres_col == fecha][[1]]
                    print(paste0('Accumulative ', input$radioButton, ' Cases ', input$select1, '\n', format(round(as.numeric(num_casos), 1), nsmall=0, big.mark=",")))
                } else {
                nombres_col <- colnames(dataR)
                num_casos <- filter(dataR, `Country/Region`==input$select1 & `Province/State` == input$select2)[nombres_col == fecha][[1]]
                print(paste0('Accumulative ', input$radioButton, ' Cases ', input$select1, ' ', input$select2, '\n', format(round(as.numeric(num_casos), 1), nsmall=0, big.mark=",")))
                }
            }
        })
    })
    
    # Graficos
    observeEvent(input$radioButton, {
        output$grafica1 <- renderPlot({
            if (input$radioButton == 'Confirmed'){
                if (input$select2 == 'NA'){
                dataFiltro <- t(filter(dataC, `Country/Region`==input$select1)[5:ncol(dataC)])[,1]
                
                barplot(dataFiltro, names='', col='yellow',
                        xlab="Time", 
                        ylab="Amount", 
                        main= paste0('Accumulated Confirmed Cases', '\n', input$select1, ' ', input$select2))
                } else {
                    dataFiltro <- t(filter(dataC, `Country/Region`==input$select1 & `Province/State` == input$select2)[5:ncol(dataC)])[,1]
                    
                    barplot(dataFiltro, names='', col='yellow',
                            xlab="Time", 
                            ylab="Amount", 
                            main= paste0('Accumulated Confirmed Cases', '\n', input$select1, ' ', input$select2))
                }
            } else if (input$radioButton == 'Deaths'){
                if (input$select2 == 'NA'){
                    dataFiltro <- t(filter(dataD, `Country/Region`==input$select1)[5:ncol(dataD)])[,1]
                    
                    barplot(dataFiltro, names='', col='yellow',
                            xlab="Time", 
                            ylab="Amount", 
                            main= paste0('Accumulated Confirmed Cases', '\n', input$select1, ' ', input$select2))
                } else {
                
                dataFiltro <- t(filter(dataD, `Country/Region`==input$select1 & `Province/State` == input$select2)[5:ncol(dataD)])[,1]
                
                barplot(dataFiltro, names='', col='yellow',
                        xlab="Time", 
                        ylab="Amount", 
                        main= paste0('Accumulated Deaths Cases', '\n', input$select1, ' ', input$select2))
                }
            } else if (input$radioButton == 'Recovered'){
                if (input$select2 == 'NA'){
                    dataFiltro <- t(filter(dataR, `Country/Region`==input$select1)[5:ncol(dataR)])[,1]
                    
                    barplot(dataFiltro, names='', col='yellow',
                            xlab="Time", 
                            ylab="Amount", 
                            main= paste0('Accumulated Confirmed Cases', '\n', input$select1, ' ', input$select2))
                } else {
                dataFiltro <- t(filter(dataR, `Country/Region`==input$select1 & `Province/State` == input$select2)[5:ncol(dataR)])[,1]
                
                barplot(dataFiltro, names='', col='yellow',
                        xlab="Time", 
                        ylab="Amount", 
                        main= paste0('Accumulated Recovered Cases', '\n', input$select1, ' ', input$select2))
                }
            }
        })
    })
    
    # Despliega casos globales
    observeEvent(input$radioButton,{
        output$text2 <- renderText({
            d <- substr(as.character(input$dateInput1), 9, 10) #dia
            if (substr(d, 1,1) == '0') {
                d <- substr(d, 2, 2)
            } else {
                d <- substr(d, 1, 2)
            }
            
            m <- substr(as.character(input$dateInput1), 6, 7) #mes
            if (substr(m, 1,1) == '0') {
                m <- substr(m, 2, 2)
            } else {
                m <- substr(m, 1, 2)
            }
            
            a <- substr(as.character(input$dateInput1), 1, 2) #ano
            
            fecha <- paste0(m, '/', d, '/', a)
            if (input$radioButton == 'Confirmed'){
                global_casos <- sum(select(dataC, fecha))
                print(paste0('Global confirmed cases ', input$dateInput1, '\n', format(round(as.numeric(global_casos), 1), nsmall=0, big.mark=",")))
            } else if (input$radioButton == 'Deaths'){
                global_casos <- sum(select(dataD, fecha))
                print(paste0('Global deaths cases ', input$dateInput1, '\n', format(round(as.numeric(global_casos), 1), nsmall=0, big.mark=",")))
            } else if (input$radioButton == 'Recovered'){
                global_casos <- sum(select(dataR, fecha))
                print(paste0('Global recovered cases ', input$dateInput1, '\n', format(round(as.numeric(global_casos), 1), nsmall=0, big.mark=",")))
            }
        })
    })
    
    # Tabla top 5
    observeEvent(input$radioButton,{
        output$tabla1 <- renderTable({
            d <- substr(as.character(input$dateInput1), 9, 10) #dia
            if (substr(d, 1,1) == '0') {
                d <- substr(d, 2, 2)
            } else {
                d <- substr(d, 1, 2)
            }
            
            m <- substr(as.character(input$dateInput1), 6, 7) #mes
            if (substr(m, 1,1) == '0') {
                m <- substr(m, 2, 2)
            } else {
                m <- substr(m, 1, 2)
            }
            
            a <- substr(as.character(input$dateInput1), 1, 2) #ano
            
            fecha <- paste0(m, '/', d, '/', a)
            if (input$radioButton == 'Confirmed'){
                dataC[order(select(dataC, fecha), decreasing = TRUE), ][1:5, c("Country/Region", fecha)]
            } else if (input$radioButton == 'Deaths'){
                dataD[order(select(dataD, fecha), decreasing = TRUE), ][1:5, c("Country/Region", fecha)]
            } else if (input$radioButton == 'Recovered'){
                dataR[order(select(dataR, fecha), decreasing = TRUE), ][1:5, c("Country/Region", fecha)]
            }
        })
    })
})
