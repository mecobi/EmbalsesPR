
library(shiny)
library(leaflet)
library(lubridate)


source("utilities.R")

# utilizar archivo local en caso de que no halla coneccion al internet 

offline <- FALSE

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

etiqueta <- c("seguridad","observación","ajuste","control")
colores <-  c("darkorange","yellow","blue","darkgreen")
colors2 = rgb(t(col2rgb(rev(colores)))/ 255)
x0 <- -66.5
y0 <-  18.25
z0 <- 10  

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
                     if(offline)
                     {
                       load("df.RData")
                     }else
                     {
                       df <<- extiende.df(df)
                     }
                     incProgress(1.0)})  
      
      nivel.norm <- normalizaNivel(df$nivel,df$ajuste,df$desborde)
      
      miIcono <<- icons(
        iconUrl = ifelse(df$tendencia >= 0,
                         "http://png-2.findicons.com/files/icons/2338/reflection/128/arrow_up_1.png", 
                         "http://png-2.findicons.com/files/icons/2338/reflection/128/arrow_down_1.png"),
        iconWidth = 15, iconHeight = 15,
        iconAnchorX = 7.5, iconAnchorY = 15,
      )
      
      withProgress(message= 'Generando MAPA',
                   value=0,{
      
      incProgress(0.33)        
      
      mapa <- leaflet(df) %>% 
        addTiles("http://{s}.tile.opencyclemap.org/cycle/{z}/{x}/{y}.png",
                 attribution="CienciaDatosPR-Matemáticas-UPR-Humacao",
                 options=tileOptions(detectRetina=TRUE))      %>%
        setView(x0,y0,z0) 
      
      incProgress(0.33)
      
      nombre.embalse <- toupper(df$nombre)
      
      contenido <- paste("<B><font color='blue'>",toupper(df$nombre),"</font></B>","<BR/>",
                   "<font color='blue'><B>Nivel:</B></font>",
                   sprintf("%3.2f",df$nivel)," m","<BR/>",
                   "<B><font color='blue'>Fecha:</B></font>",df$mifecha)
      
      grosor1 <- 0.005
      altura1 <- 0.028
      
      if(input$estilo == "Rectángulos")
      {
        
        # rectangulo de nivel maximo 
        
        mapa <- mapa %>% 
          addRectangles(fill=TRUE,
                        weight=1,
                        color="black",
                        opacity=1,
                        lng1=df$longitude-grosor1,
                        lat1=df$latitude-altura1,
                        lng2=df$longitude+grosor1,
                        lat2=df$latitude+altura1,
                        popup=contenido)
      
        grosor2 <- 0.005
        
        # rectangulo de color con nivel actual 
        
        mapa <- mapa %>% 
          addRectangles(fill=TRUE,
                        fillColor=df$micolor,
                        weight=0.5,
                        color="black",
                        stroke=FALSE,
                        fillOpacity=df$miopacidad,
                        opacity=df$miopacidad,
                        lng1=df$longitude-grosor2,
                        lat1=(df$latitude-altura1),
                        lng2=df$longitude+grosor2,
                        lat2=df$latitude+ (altura1)*nivel.norm,
                        popup=contenido)
        
        # escala
        
        if(input$escala)
        {
            mapa <- mapa %>% 
              addRectangles(fill=FALSE,
                            weight=0.5,
                            color="black",
                            opacity=1,
                            lng1=df$longitude-grosor1,
                            lat1=df$latitude+altura1*normalizaNivel(df$seguridad,df$ajuste,df$desborde),
                            lng2=df$longitude+grosor1,
                            lat2=df$latitude+altura1*normalizaNivel(df$seguridad,df$ajuste,df$desborde),
                            popup=contenido)
            
            mapa <- mapa %>% 
              addRectangles(fill=FALSE,
                            weight=0.5,
                            color="black",
                            opacity=1,
                            lng1=df$longitude-grosor1,
                            lat1=df$latitude+altura1*normalizaNivel(df$observacion,df$ajuste,df$desborde),
                            lng2=df$longitude+grosor1,
                            lat2=df$latitude+altura1*normalizaNivel(df$observacion,df$ajuste,df$desborde),
                            popup=contenido)
            
          
            mapa <- mapa %>% 
              addRectangles(fill=FALSE,
                            weight=0.5,
                            color="black",
                            opacity=1,
                            lng1=df$longitude-grosor1,
                            lat1=df$latitude+altura1*normalizaNivel(df$ajuste,df$control,df$desborde),
                            lng2=df$longitude+grosor1,
                            lat2=df$latitude+altura1*normalizaNivel(df$ajuste,df$control,df$desborde),
                            popup=contenido)
        }
        
        incProgress(0.33)
      
      }
      
      if(input$estilo == "Círculos")
      {
      
        mapa <- mapa %>%
          addCircles(fill=TRUE,
                     color="black",
                     stroke=TRUE,
                     weight=1,
                     opacity=1,
                     radius=mysize,
                     popup = contenido,
                     options = markerOptions(riseOnHover = TRUE))
        
        incProgress(0.33)
        
        mapa <- mapa %>%
          addCircles(fill=TRUE,
                     color=df$micolor,
                     stroke=FALSE,
                     weight=1,
                     fillOpacity=df$miopacidad,
                     opacity=df$miopacidad,
                     radius=mysize*nivel.norm,
                     popup = contenido)
        
        if(input$escala)
        {

          mapa <- mapa %>% 
            addCircles(fill=FALSE,
                     color="black",
                     stroke=TRUE,
                     weight=0.5,
                     opacity=1,
                     radius=mysize*normalizaNivel(df$seguridad,df$ajuste,df$desborde),
                     popup = contenido,
                     options = markerOptions(riseOnHover = TRUE))
          
          mapa <- mapa %>% 
            addCircles(fill=FALSE,
                       color="black",
                       stroke=TRUE,
                       weight=0.5,
                       opacity=1,
                       radius=mysize*normalizaNivel(df$observacion,df$ajuste,df$desborde),
                       popup = contenido,
                       options = markerOptions(riseOnHover = TRUE))
          
          mapa <- mapa %>% 
            addCircles(fill=FALSE,
                       color="black",
                       stroke=TRUE,
                       weight=0.5,
                       opacity=1,
                       radius=mysize*normalizaNivel(df$ajuste,df$ajuste,df$desborde),
                       popup = contenido,
                       options = markerOptions(riseOnHover = TRUE))
          
        }
        
      }
      
      

       mapa <- mapa  %>% addMarkers(lng=df$longitude,
                                    lat=df$latitude + 0.028,
                                    icon=miIcono,
                                    layerId=df$nombre)       
      
       mapa <- mapa %>%  addLegend(position = 'topright',
                          colors = colors2,
                          labels = etiqueta, 
                          opacity = 1,
                          title = 'Estado del embalse',
                          layerId="leyenda") 
      
      print(mapa)
      
      })
      
    }
    
  })

#################################################################
## definir evento para cuando el mouse se coloca sobre el embalse
#################################################################

# observeEvent(input$mapa_shape_mouseover,{
#   proxy <- leafletProxy("mapa",session)
#   mimouse <- input$mapa_shape_mouseover
#   x <- mimouse$lng
#   y <- mimouse$lat
#   inx.nombre <- df$nombre == mimouse$id
#   contenido <- paste("<B><font color='blue'>",toupper(mimouse$id),"</font></B>","<BR/>",
#                      "<font color='blue'><B>Nivel:</B></font>",
#                      sprintf("%3.2f",df$nivel[inx.nombre])," m","<BR/>",
#                      "<B><font color='blue'>Fecha:</B></font>",df$mifecha[inx.nombre])
#   proxy %>% addPopups(lng=x,lat=y,
#                       options=popupOptions(closeOnClick=TRUE),
#                       layerId=mimouse$id,
#                       popup=contenido)
#   
# })
  

############################################################
# definir evento para añadir o quitar la flecha de tendencia
############################################################

observeEvent(input$tendencia,{
    miIcono <<- icons(
    iconUrl = ifelse(df$tendencia >= 0,
                     "http://png-2.findicons.com/files/icons/2338/reflection/128/arrow_up_1.png", 
                     "http://png-2.findicons.com/files/icons/2338/reflection/128/arrow_down_1.png"),
    iconWidth = 15, iconHeight = 15,
    iconAnchorX = 7.5, iconAnchorY = 15,
    )
    proxy <- leafletProxy("mapa",session)
    z0 <- input$mapa_zoom
    if(input$tendencia)
    {
      proxy %>% addMarkers(lng=df$longitude,
                 lat=df$latitude + 0.028,
                 icon=miIcono) %>% 
                 setView(x0,y0,zoom=z0)
    }else
    {
      proxy %>% clearMarkers() %>%
                setView(x0,y0,zoom=z0)
    }
  })


################################################
# definir evento para añadir o quitar la leyenda
################################################

observeEvent(input$leyenda,{
  etiqueta <- c("seguridad","observación","ajuste","control")
  colores <-  c("darkorange","yellow","blue","darkgreen")
  colors2 = rgb(t(col2rgb(rev(colores)))/ 255)
  proxy <- leafletProxy("mapa",session)
  z0 <- input$mapa_zoom
  if(input$leyenda)
  { 
    proxy %>% addLegend(position = 'topright',
                        colors = colors2,
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

  
###############################################
# definir evento para añadir o quitar la escala 
###############################################

# observeEvent(input$escala,{
# 
#   proxy <- leafletProxy("mapa",session)
#   z0 <- input$mapa_zoom
#   
#   if(input$escala && input$estilo == 'Círculos')
#   {
#     proxy %>%             
#       addCircles(lng=df$longitude,lat=df$latitude,fill=FALSE,
#          color="black",
#          stroke=TRUE,
#          weight=0.5,
#          opacity=1,
#          radius=mysize*normalizaNivel(df$seguridad,df$ajuste,df$desborde),
#          options = markerOptions(riseOnHover = TRUE)) %>%
#      setView(x0,y0,zoom=z0)
#   }else
#   {
#     proxy %>% clearMarkers() %>%
#       setView(x0,y0,zoom=z0)
#   }
# })

})