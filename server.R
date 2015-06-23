#################################################
# server.R - rutina a nivel del servidor de shiny 
# Elio Ramos 
# CienciaDatosPR
# Departamento de Matemática 
# Universidad de Puerto Rico en Humacao 
#################################################

library(shiny)          # interfaz gráfica en línea con R 
library(leaflet)        # para hacer mapa interactivo
library(lubridate)      # funciones utiles para manejo de fechas 


## llamar rutinas utiles 

source("utilities.R")

#############################
# código de colores según AAA
#############################

etiqueta <- c("seguridad","observación","ajuste","control")
colores <-  c("darkorange","yellow","blue","darkgreen")
codigo.colores <- rgb(t(col2rgb(rev(colores)))/ 255)

#####################################################################
# coordenadas (lon,lat) del centro del mapa y el correspondiente zoom
#####################################################################

x0 <- -66.5   
y0 <-  18.25
z0 <- 10  

##################################
# iconos de flechas para tendencia 
##################################

flecha_arriba <- "http://png-2.findicons.com/files/icons/2338/reflection/128/arrow_up_1.png"
flecha_abajo  <- "http://png-2.findicons.com/files/icons/2338/reflection/128/arrow_down_1.png"


################################################
## funcion para cambiar el formato de fecha/hora
################################################

genera_fecha <- function(hoyes)
{
  #hoyes <- Sys.time() 
  semana <- c("Domingo","Lunes","Martes","Miércoles","Jueves","Viernes","Sabado")
  meses  <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto",
              "Septiembre","Octubre","Noviembre","Diciembre")
  dia1  <- semana[wday(hoyes)]
  dia2  <- day(hoyes)
  mes   <- meses[month(hoyes)]
  anio  <- year(hoyes)
  fecha <- paste0(dia1," ",dia2," de ",mes," de ",anio)
  if(am(hoyes))
  {
    ampm <- "AM"
    hora  <- paste0(hour(hoyes),":",sprintf("%02d",minute(hoyes))," ",ampm)
  }else
  {
    ampm <- "PM"
    if(hour(hoyes) >= 13)
    {
      hora  <- paste0(hour(hoyes)-12,":",sprintf("%02d",minute(hoyes))," ",ampm)
    }else
    {
      hora  <- paste0(hour(hoyes),":",sprintf("%02d",minute(hoyes))," ",ampm)
    }
  }
  
  return(paste(fecha," a las ",hora))
  
}


#####################################################################################################
## funciion para añadirle fecha, nivel, color, y opacidad al data frame con los datos de los embalses
#####################################################################################################

extiende.df <- function(df)
{
  if(length(df$nivel) < 11)
  {
  
  temp <- as.data.frame(t(sapply(df$siteID,buscaNiveles)))
  df$fecha <- as.vector(unlist(temp$fecha))
  df$nivel <- as.vector(unlist(temp$nivel))
  df$tendencia <- as.vector(unlist(temp$tendencia))
  micolor     <- rep(0,11) 
  miopacidad <- rep(0,11)
  mifecha <- rep(0,11)
  opacidad <- c(1.0,1.0,0.75,0.50)

  for(siteInx in 1:length(df$fecha))
  {
    
    micolor[siteInx] <- colores[findInterval(df$nivel[siteInx],
                                             rightmost.closed = TRUE,
                                             as.vector(df[siteInx,c(9:5)]))] 
    miopacidad[siteInx] <- opacidad[findInterval(df$nivel[siteInx],
                                             rightmost.closed = TRUE,
                                             as.vector(df[siteInx,c(9:5)]))] 
    mifecha[siteInx] <- genera_fecha(df$fecha[siteInx])
  }
  
  df$micolor    <- micolor
  df$miopacidad <- miopacidad
  df$mifecha    <- mifecha
  }
  return(df)
}

########################################
## rutina principal a nivel del servidor
########################################

shinyServer(function(input, output,session){
  
  output$mensaje <- renderPrint({

    if(input$buscaDatos)
    {
    div(style = "margin: 10px;color:black;width: 800px;text-align: justify;font-size:16px;",
        HTML(paste("El <font color='blue'> <b>radio</b> </font> de los círculos de color es proporcional 
                al <font color='blue'><b>nivel del embalse</b></font> y la flecha indica la  
                <font color='blue'><b>tendencia del nivel </b></font> en las últimas horas")))
    }
  })
  
  
  output$mapa <- renderLeaflet({
    
    if(input$buscaDatos)
    {
      
        withProgress(message = 'Buscando en USGS', 
                     value=0,{
                     df2 <<- extiende.df(df)   
                    incProgress(1.0)})  
  
        nivel.norm <- normalizaNivel(df2$nivel,df2$ajuste,df2$desborde)
        
        miIcono <<- icons(
          iconUrl = ifelse(df2$tendencia >= 0, flecha_arriba, flecha_abajo),
          iconWidth = 15, iconHeight = 15,
          iconAnchorX = 7.5, iconAnchorY = 15,
        )
      
        withProgress(message= 'Generando MAPA',
                   value=0,{
      
        incProgress(0.33)        
      
        mapa <- leaflet(df2) %>% 
        addTiles("http://{s}.tile.opencyclemap.org/cycle/{z}/{x}/{y}.png",
                 attribution="opencyclemap/CienciaDatosPR/Matemáticas/UPR-Humacao",
                 options=tileOptions(detectRetina=TRUE))      %>%
        setView(x0,y0,z0) 
      
        incProgress(0.33)
      
        nombre.embalse <- toupper(df2$nombre)
      
        contenido <<- paste("<B><font color='blue'>",toupper(df2$nombre),"</font></B>","<BR/>",
                   "<font color='blue'><B>Nivel:</B></font>",
                   sprintf("%3.2f",df2$nivel)," m","<BR/>",
                   "<B><font color='blue'>Fecha:</B></font>",df2$mifecha)
      
        grosor1 <<- 0.005
        altura1 <<- 0.028
      

        # rectangulo de NIVEL MAXIMO 
        
        mapa <- mapa %>% 
          addRectangles(fill=TRUE,
                        weight=1,
                        color="black",
                        opacity=1,
                        lng1=df2$longitude-grosor1,
                        lat1=df2$latitude-altura1,
                        lng2=df2$longitude+grosor1,
                        lat2=df2$latitude+altura1,
                        popup=contenido)
      
        grosor2 <<- 0.005
        
        # rectangulo de color con NIVEL ACTUAL 
        
        mapa <- mapa %>% 
          addRectangles(fill=TRUE,
                        fillColor=df2$micolor,
                        weight=0.5,
                        color="black",
                        stroke=FALSE,
                        fillOpacity=df2$miopacidad,
                        opacity=df2$miopacidad,
                        lng1=df2$longitude-grosor2,
                        lat1=(df2$latitude-altura1),
                        lng2=df2$longitude+grosor2,
                        lat2=df2$latitude+ (altura1)*nivel.norm,
                        popup=contenido)
        
        # incluir escala con nivel 
        
          mapa <- mapa %>% 
            addRectangles(fill=FALSE,
                          group="escala",
                          weight=0.5,
                          color="black",
                          opacity=1,
                          lng1=df2$longitude-grosor1,
                          lat1=df2$latitude+altura1*normalizaNivel(df2$seguridad,df2$ajuste,df2$desborde),
                          lng2=df2$longitude+grosor1,
                          lat2=df2$latitude+altura1*normalizaNivel(df2$seguridad,df2$ajuste,df2$desborde),
                          popup=contenido)
          
          mapa <- mapa %>% 
            addRectangles(fill=FALSE,
                          group="escala",
                          weight=0.5,
                          color="black",
                          opacity=1,
                          lng1=df2$longitude-grosor1,
                          lat1=df2$latitude+altura1*normalizaNivel(df2$observacion,df2$ajuste,df2$desborde),
                          lng2=df2$longitude+grosor1,
                          lat2=df2$latitude+altura1*normalizaNivel(df2$observacion,df2$ajuste,df2$desborde),
                          popup=contenido)
          
        
          mapa <- mapa %>% 
            addRectangles(fill=FALSE,
                          group="escala",
                          weight=0.5,
                          color="black",
                          opacity=1,
                          lng1=df2$longitude-grosor1,
                          lat1=df2$latitude+altura1*normalizaNivel(df2$ajuste,df2$control,df2$desborde),
                          lng2=df2$longitude+grosor1,
                          lat2=df2$latitude+altura1*normalizaNivel(df2$ajuste,df2$control,df2$desborde),
                          popup=contenido)
        
        
       incProgress(0.33)
      
      
       # inclur flecha de tendencia 

       mapa <- mapa  %>% addMarkers(group="tendencia", 
                                    lng=df2$longitude,
                                    lat=df2$latitude + 0.028,
                                    icon=miIcono,
                                    layerId=df2$nombre)       
      
       # incluir leyenda 
       
       mapa <- mapa %>%  addLegend(position = 'topright',  
                          colors = codigo.colores,
                          labels = etiqueta, 
                          opacity = 1,
                          title = 'Estado del embalse',
                          layerId="leyenda") 
      
       # incluir fecha de ultima peticion 
       
      mapa <- mapa %>% addControl(position='bottomleft',
                                  html=paste0("Última actualizacion: <font color='blue'>",
                                              genera_fecha(Sys.time()),"</font"))
      
      
      print(mapa)
      
      })
      
    }
    
})



##################################################################################
#definir evento para añadir o quitar la flecha de tendencia sin tener que recargar
##################################################################################

observeEvent(input$tendencia,{
    if(input$buscaDatos)
    {
        proxy <- leafletProxy("mapa",session)
        if(input$tendencia)
        {
          proxy %>% showGroup("tendencia")
        }else
        { 
          proxy %>% hideGroup("tendencia")
        }
    }
  })


#######################################################################
# definir evento para añadir o quitar la leyenda sin tener que recargar 
#######################################################################

observeEvent(input$leyenda,{
  proxy <- leafletProxy("mapa",session)
  z0 <- input$mapa_zoom
  lim <- input$mapa_bounds
  x0 <- (lim$east + lim$west)/2.0
  y0 <- (lim$north + lim$south)/2.0
  if(input$leyenda)
  { 
    proxy %>% addLegend(position = 'topright',
                        colors = codigo.colores,
                        labels = etiqueta, 
                        opacity = 1,
                        title = 'Estado del embalse',
                        layerId="leyenda") %>%
              setView(x0,y0,zoom=z0)
    
  }else
  {

   proxy %>% removeControl(layerId="leyenda") %>% setView(x0,y0,zoom=z0)
  }
})
 

#############################################################################
#definir evento para añadir o quitar escala de niveles sin tener que recargar 
#############################################################################

observeEvent(input$escala,{
  if(input$buscaDatos)
  {
    proxy <- leafletProxy("mapa",session)
    if(input$escala)
    {      
       proxy %>% showGroup("escala")    
    }else
    {
       proxy %>% hideGroup("escala")
    }
  }
})

})


