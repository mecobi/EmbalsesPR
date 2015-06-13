
###############################
# User interface for EmbalsesPR
###############################

library(shiny)
library(leaflet)
library(lubridate)
library(shinythemes)

cadena1 <- "<p align='justify'>Una aplicación para <font color='blue'>buscar</font> y 
            <font color='blue'>visualizar</font> el estado actual de 
            los 11 embalses en Puerto Rico. Dependiendo del tráfico en el servidor (y los 
            instrumentos del USGS) el tiempo de búsqueda puede variar. Según USGS los datos 
            tienen estado provisional y podrian variar luego de una revisión. De ser necesario 
            recarge la página.</p>"

cadena2a <- "<p align='justify'>La <font color='blue'> <b>altura </b> </font> de los rectángulos 
            de color es proporcional al <font color='blue'><b>nivel del embalse</b></font> 
            y la flecha indica la   <font color='blue'><b>tendencia del nivel </b></font> en 
            las últimas horas. Puede oprimir en los rectángulos para obtener mas información.</p>"

cadena2b <- "<p align='justify'>El <font color='blue'> <b>radio</b> </font> de los círculos 
            de color es proporcional al <font color='blue'><b>nivel del embalse</b></font> 
            y la flecha indica la   <font color='blue'><b>tendencia del nivel </b></font> en 
            las últimas horas. Puede oprimir en los círculos para obtener mas información.</p>"

cadena3 <- "<p align='justify'> <font color='Maroon'>embalsesPR</font> es un proyecto del grupo 
           <font color='blue'>CienciaDatosPR</font> del <font color='blue'>Departamento de 
           Matemáticas</font> de la <font color='Maroon'> Universidad de Puerto Rico en Humacao
           </font>.</p>"

shinyUI(fluidPage(theme = shinytheme("cosmo"),
  #titlePanel("EmbalsesPR.",windowTitle="EmbalsesPR"),
  h1("embalsesPR",align="left",style = "color:Maroon"),
  sidebarLayout(
    sidebarPanel( 
     
      HTML(cadena1),
     
     actionButton("buscaDatos","Buscar datos..",icon=icon("search")),
     
     br(),
     br(),
     selectInput(inputId = "estilo",
                 label = "Representacion de los embalses",
                 choices = c("Rectángulos",
                             "Círculos"),
                 selected = "Rectángulos"),
     
     checkboxInput(inputId = "tendencia",
                   label = "Mostrar flecha de tendencia",
                   value = TRUE),
     checkboxInput(inputId = "escala",
                   label = "Mostrar escala",
                   value = FALSE),
     checkboxInput(inputId = "leyenda",
                   label = "Mostrar leyenda",
                   value = TRUE),
     
     br(),
     
     conditionalPanel(condition = "input.buscaDatos == true && input.estilo == 'Rectángulos'",HTML(cadena2a)),
     conditionalPanel(condition = "input.buscaDatos == true && input.estilo == 'Círculos'",HTML(cadena2b)),
     
     HTML(cadena3)),
    
  mainPanel(
    leafletOutput("mapa",width="100%",height=580)
  ))))

