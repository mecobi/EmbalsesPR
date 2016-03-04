##############################################
# ui.R - interfaz con el usuario de embalsespr
# Elio Ramos 
# CienciaDatosPR
# Departamento de Matematica 
# Universidad de Puerto Rico en Humacao 
##############################################

library(shiny)          
library(leaflet)       
library(lubridate)      
library(shinythemes)    

cadena1 <- "<p align='justify'><font color='Maroon'>embalsesPR</font> es una aplicaci&oacute;n para <font color='blue'>buscar</font> y 
            <font color='blue'>visualizar</font> el estado actual de 
            los 11 embalses en Puerto Rico utilizando datos del Servicio Geol&oacute;gico de los Estados Unidos 
            (USGS por sus siglas en ingl&eacute;s). Dependiendo del tr&aacute;fico en el servidor (y los 
            instrumentos del USGS) el tiempo de b&uacute;squeda puede variar. Seg&uacute;n USGS los datos 
            tienen estado provisional y podrian variar luego de una revisi&oacute;n. De ser necesario 
            recarge la p&aacute;gina.</p>"

# cadena2a <- "<p align='justify'>La <font color='blue'> <b>altura </b> </font> de los rect&aacute;ngulos 
#             de color es proporcional al <font color='blue'><b>nivel del embalse</b></font> 
#             y la flecha indica la   <font color='blue'><b>tendencia del nivel </b></font> en 
#             las &uacute;ltimas horas. Puede oprimir en los rect&aacute;ngulos para obtener mas informaci&oacute;n.
#             El c&oacute;digo de colores, los niveles de alerta, y las zonas (aproximadas) de racionamiento est&aacute;n 
#             basadas en informaci&oacute;n oficial provista en  <font color='blue'>http://acueductospr.com.</font</p></font>"

cadena2a <- "<p align='justify'> <ul style='padding-left:20px;text-align:justify;'> <li>La <font color='blue'> <b>altura </b> </font> 
             de los rect&aacute;ngulos de color es proporcional al <font color='blue'><b>nivel del embalse</b></font> 
             y la flecha indica la   <font color='blue'><b>tendencia del nivel </b></font> en 
             las &uacute;ltimas horas</li> <li><font color='blue'><b>Oprima</b></font>  en los rect&aacute;ngulos para obtener mas informaci&oacute;n y una gr&aacute;fica 
             del nivel en las pasadas horas.</li> 
             <li>El c&oacute;digo de colores, los niveles de alerta, y las zonas (aproximadas) de racionamiento est&aacute;n 
              basadas en informaci&oacute;n oficial provista en  <font color='blue'>http://acueductospr.com.</font</p></font> 
              </li></ul> </p>"


cadena2b <- "<p align='justify'>El <font color='blue'> <b>radio</b> </font> de los c&iacute;rculos 
            de color es proporcional al <font color='blue'><b>nivel del embalse</b></font> 
            y la flecha indica la   <font color='blue'><b>tendencia del nivel </b></font> en 
            las &uacute;ltimas horas. Puede oprimir en los c&iacute;rculos para obtener mas informaci&oacute;n.</p>"

cadena3 <- "<p align='justify'> <font color='Maroon'>embalsesPR</font> es un proyecto del grupo 
           <font color='blue'>CienciaDatosPR</font> del <font color='blue'>Departamento de 
           Matem&aacute;ticas</font> de la <font color='Maroon'> Universidad de Puerto Rico en Humacao
           </font>. &iquest;Te interesa colaborar? El c&oacute;digo fuente est&aacute; disponible en el portal 
           <a href='https://github.com/mecobi/EmbalsesPR'> github.com</a>.
           Pueden enviar sus preguntas, comentarios, o sugerencias 
           a: <a href='mailto:cienciadatospr.uprh.edu'>cienciadatospr@mate.uprh.edu</a></p>"

licencia <- "<a rel='license' href='http://creativecommons.org/licenses/by-nc-sa/4.0/'>
            <img alt='Licencia Creative Commons' style='border-width:0' src=
            'https://i.creativecommons.org/l/by-nc-sa/4.0/80x15.png' /></a><br />
            <span xmlns:dct='http://purl.org/dc/terms/' property='dct:title'>EmbalsesPR
            </span> por <a xmlns:cc='http://creativecommons.org/ns#' href='mate.uprh.edu' 
            property='cc:attributionName' rel='cc:attributionURL'>CienciaDatosPR/
            Matem&aacute;ticas/UPR-Humacao</a> se distribuye bajo una <a rel='license' 
            href='http://creativecommons.org/licenses/by-nc-sa/4.0/'>Licencia Creative 
            Commons Atribuci&oacute;n-NoComercial-CompartirIgual 4.0 Internacional</a>."


shinyUI(fluidPage(
  theme = shinytheme("cosmo"),
  includeCSS("./www/styles.css"),
  headerPanel(title="embalsesPR",windowTitle="embalsesPR"),
  #h1("embalsesPR",align="left",style = "color:Maroon"),
  sidebarLayout(
  sidebarPanel( 
    
      HTML(cadena1),
      actionButton("buscaDatos","Buscar datos..",icon=icon("search")),
      br(),
      conditionalPanel(condition = "input.buscaDatos == true",HTML(cadena2a)),
      checkboxInput(inputId = "rectangulo",
                    label = "Mostrar gráfica de estado actual",
                    value = TRUE),
      checkboxInput(inputId = "nombre",
                    label = "Mostrar nombres de los embalses",
                    value = TRUE),     
      #checkboxInput(inputId = "racionamiento",
      #              label = "Mostrar zonas de racionamiento",
      #              value = FALSE),
      HTML(cadena3),
      
      HTML(licencia)

    ),
  mainPanel(
    leafletOutput("mapa",width="100%",height=580))
  )))

