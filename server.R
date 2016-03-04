#################################################
# server.R - rutina a nivel del servidor de shiny 
# Elio Ramos 
# CienciaDatosPR
# Departamento de Matematica 
# Universidad de Puerto Rico en Humacao 
#################################################

library(shiny)          # interfaz grafica en linea con R 
library(leaflet)        # para hacer mapa interactivo
library(lubridate)      # funciones utiles para manejo de fechas 
library(ggplot2)
library(RCurl)
library(ddR)
library(plyr)

## llamar rutinas utiles 

source("utilities.R")
load("datos.racionamiento.RData")

##############################
## codigo de colores segun AAA
##############################

etiqueta <- c("seguridad","observaci&oacute;n","ajuste","control")
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

flecha_arriba <- "./www/arrow_up.png"
flecha_abajo  <- "./www/arrow_down.png"
flecha_plana  <- "./www/arrow_right.png"
calabera      <- "./www/danger5.png"
carraizo      <- "./www/Carraizo.png"


################################################
## funcion para cambiar el formato de fecha/hora
################################################

genera_fecha <- function(hoyes)
{
  #hoyes <- Sys.time() 
  semana <- c("Domingo","Lunes","Martes","Mi&eacute;rcoles","Jueves","Viernes","Sabado")
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
## funcion para anadirle fecha, nivel, color, y opacidad al data frame con los datos de los embalses
#####################################################################################################

extiende.df <- function(df)
{
  ndat <- length(df$nombre)
  if(length(df$nivel) < ndat)
  {
  #temp <- as.data.frame(t(sapply(df$siteID,buscaNiveles)))
  #df$fecha <- as.vector(unlist(temp$fecha))
  #df$nivel <- as.vector(unlist(temp$nivel))
  #df$tendencia <- as.vector(unlist(temp$tendencia))
  #df$cambio.nivel <- as.vector(unlist(temp$cambio.nivel))

  a <- dmapply(function(x) buscaNiveles(x), df$siteID,nparts=4)
    
  temp <- ldply(collect(a),rbind)
  
  df$fecha        <- temp$fecha
  df$nivel        <- temp$nivel
  df$tendencia    <- temp$tendencia
  df$cambio.nivel <- temp$cambio.nivel


  micolor     <- rep(0,ndat) 
  miopacidad <- rep(0,ndat)
  mifecha <- rep(0,ndat)
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

  micolor[is.na(micolor)] <- "darkgreen" 
  
  df$micolor    <- micolor
  df$miopacidad <- miopacidad
  df$mifecha    <- mifecha
  }
  return(df)
}

###########################################
## funcion para mostrar flecha de tendencia
###########################################

flecha_tendencia <- function(mapa,df2)
{
  mapa <- mapa  %>% addMarkers(group="tendencia", 
                               lng=df2$longitude,
                               lat=df2$latitude + 0.028,
                               icon=miIcono,
                               layerId=df2$nombre)   
  return(mapa)
}

###########################################
## funcion para mostrar nombre del embalse 
###########################################

nombre_embalse <- function(mapa,df2)
{
  
  altura <<- 0.09
  
  mapa <- mapa %>% addPopups(df2$longitude,df2$latitude - altura, 
                             as.character(df2$nombre),
                             group="nombre",
                             options=popupOptions(
                               zoomAnimation=TRUE,
                               maxWidth= 200,
                               closeOnClick = FALSE, 
                               closeButton = FALSE))
  
}

###########################################
## funcion para mostrar leyenda del embalse
###########################################

leyenda_embalse <- function(mapa)
{
  mapa <- mapa %>%  addLegend(position = 'topright',  
                              colors = codigo.colores,
                              labels = etiqueta, 
                              opacity = 1,
                              title = 'Estado del embalse',
                              layerId="leyenda") 
  return(mapa)
}

###############################################
## funcion para mostrar rectangulos de embalses
###############################################

rectangulo_embalse <- function(mapa,df2)
{
  grosor1 <<- 0.005
  altura1 <<- 0.028
  grosor2 <<- 0.005
  
  mapa <- mapa %>% 
    addRectangles(fill=TRUE,
                  group="rectangulo",
                  weight=1,
                  color="black",
                  opacity=1,
                  lng1=df2$longitude-grosor1,
                  lat1=df2$latitude-altura1,
                  lng2=df2$longitude+grosor1,
                  lat2=df2$latitude+altura1,
                  popup=contenido)
  
  # rectangulo de color con NIVEL ACTUAL 
  
  mapa <- mapa %>% 
    addRectangles(fill=TRUE,
                  group="rectangulo",
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
                  group="rectangulo",
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
                  group="rectangulo",
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
                  group="rectangulo",
                  weight=0.5,
                  color="black",
                  opacity=1,
                  lng1=df2$longitude-grosor1,
                  lat1=df2$latitude+altura1*normalizaNivel(df2$ajuste,df2$control,df2$desborde),
                  lng2=df2$longitude+grosor1,
                  lat2=df2$latitude+altura1*normalizaNivel(df2$ajuste,df2$control,df2$desborde),
                  popup=contenido)
  
  mapa <- mapa  %>% addMarkers(group="rectangulo", 
                               lng=df2$longitude,
                               lat=df2$latitude + 0.028,
                               icon=miIcono,
                               layerId=df2$nombre)  
  
  
  
  return(mapa)
  
}

##############################################
## funcion para mostrar zonas de racionamiento
##############################################

zona_racionamiento <- function(mapa,datos.muni,lista.muni,poli)
{

  for(inx in lista.muni)
  {
    
      mapa <- mapa %>% addPolygons(lng=poli[[inx]]$lng,lat=poli[[inx]]$lat,
                                   group="poligono",
                                   weight=1)
    
      mapa <- mapa %>% addCircleMarkers(lng=datos.muni[[inx]]$lng,
                                      lat=datos.muni[[inx]]$lat,
                                      group="zona",
                                      fillColor=datos.muni[[inx]]$color,
                                      color=datos.muni[[inx]]$color,
                                      popup=paste0(datos.muni[[inx]]$lugar,", ",datos.muni[[inx]]$municipio),
                                      opacity=0.5,
                                      weight=1,
                                      radius=3)
  }
  
  mapa <- mapa %>% addLegend(title="Zona de racionamiento",
                         layerId="leyenda.racionamiento",
                         colors=c("red","green"),
                         opacity=1.0,
                         position="bottomright",
                         labels=c("en racionamiento","susceptible"))     
  return(mapa)
  
}


########################################
## rutina principal a nivel del servidor
########################################

shinyServer(function(input, output,session){
  
  output$mensaje <- renderPrint({

    if(input$buscaDatos)
    {
    div(style = "margin: 10px;color:black;width: 800px;text-align: justify;font-size:16px;",
        HTML(paste("El <font color='blue'> <b>radio</b> </font> de los c&iacute;rculos de color es proporcional 
                al <font color='blue'><b>nivel del embalse</b></font> y la flecha indica la  
                <font color='blue'><b>tendencia del nivel </b></font> en las &uacute;ltimas horas")))
    }
  })
  
  
  output$mapa <- renderLeaflet({

    if(input$buscaDatos)
    {
      
        withProgress(message = 'Buscando en USGS', 
                     value=0,{
                     df2 <<- extiende.df(df)   
                    incProgress(1.0)})  
  
        nivel.norm <<- normalizaNivel(df2$nivel,df2$ajuste,df2$desborde)
        
        df2$tendencia <- round(df2$tendencia,digits=4)
  
        iconUrl <- rep("",length(df2$tendencia))
        
        iconUrl[which(df2$tendencia > 0)]    <- flecha_arriba
        iconUrl[which(df2$tendencia < 0)]    <- flecha_abajo 
        iconUrl[which(df2$tendencia == 0)]   <- flecha_plana
        iconUrl[which(df2$tendencia == -99)] <- calabera
        
        miIcono <<- icons(
          iconUrl = iconUrl,
          iconWidth = 20, iconHeight = 20,
          iconAnchorX = 10, iconAnchorY = 25
        )
      
        withProgress(message= 'Generando MAPA',
                   value=0,{
      
        incProgress(0.33)        
      
        mapa <- leaflet(df2) %>% 
        addTiles("http://{s}.tile.opencyclemap.org/cycle/{z}/{x}/{y}.png",
                 attribution="opencyclemap/CienciaDatosPR/Matem&aacute;ticas/UPR-Humacao",
                 options=tileOptions(detectRetina=TRUE))      %>%
        setView(x0,y0,z0) 
      
        incProgress(0.33)
      
        nombre.embalse <- toupper(df2$nombre)
      
        imagenes <- paste0(df2$siteID,".png")
      
        df2$nivel[df2$tendencia <= -99] <- NA
        
        unidades <- rep(" m",length(df2$tendencia))
        
        unidades[df2$tendencia <= -99] <- ""
        
        comentario.temp <- df2$cambio.nivel*100
        
        texto <- rep("xyz",length(df2$tendencia))
        
        texto[df2$tendencia > 0] <- "Aument&oacute; "
        texto[df2$tendencia < 0] <- "Disminuy&oacute; "
        texto[df2$tendencia == 0] <- "No cambi&oacute; significativamente "
        
        comentario <- paste(texto,sprintf("%2.1f cm",abs(comentario.temp)))
        
        contenido <<- paste0("<B><font color='blue'>",toupper(df2$nombre),"</font></B>","<BR/>",
                   "<font color='black'><B>Nivel: </B></font>",
                   sprintf("%3.2f",df2$nivel),unidades,"<BR/>",
                   "<B><font color='black'>Fecha: </B></font>",df2$mifecha,"<BR>",
                   "<B><font color='black'>Comentario: </B></font>",comentario,"<BR>",
                   #"<BR> <font color='blue'>Gr&aacute;fica del nivel desde las 12 de la medianoche...</font>",
                   "<p align='center'><img src='",imagenes,"' height='250' width='250' border='0' </p>")
    
       ## SECCION de ZONAS de racionamiento 
        
       #mapa <- zona_racionamiento(mapa,datos.muni,lista.muni,poli)
         
       ## SECCION de RECTANGULOS 
                
       mapa <- rectangulo_embalse(mapa,df2) 
          
       incProgress(0.33)
       
       ## SECCION nombre del embalse
       
       mapa <- nombre_embalse(mapa,df2)
       
       ## SECCION de FLECHA de tendencia 
       
       #mapa <- flecha_tendencia(mapa,df2)
      
       # incluir leyenda 
       
       mapa <- leyenda_embalse(mapa)
       
       # incluir fecha de ultima peticion 
       
       mapa <- mapa %>% addControl(position='bottomleft',
                                  html=paste0("&Uacute;ltima actualizaci&oacute;n: <font color='blue'>",
                                              genera_fecha(Sys.time()),"</font"))
      
      print(mapa)

      })
      
    }else
    {
      mapa <- leaflet() %>%  addTiles("http://{s}.tile.opencyclemap.org/cycle/{z}/{x}/{y}.png",
                             attribution="opencyclemap/CienciaDatosPR/Matem&aacute;ticas/UPR-Humacao",
                             options=tileOptions(detectRetina=TRUE)) %>% setView(x0,y0,z0) 
    
      mapa <- mapa %>% addControl(position='bottomleft',
                                  html="<font color='blue'>NO se ha realizado b&uacute;squeda</font>")
      
      mapa <- mapa  %>% removeControl(layerId="leyenda") %>% setView(x0,y0,zoom=z0)
      
      print(mapa)
    }
    
})



##################################################################################
#definir evento para anadir o quitar la flecha de tendencia sin tener que recargar
##################################################################################

# observeEvent(input$tendencia,{
#     if(input$buscaDatos)
#     {
#         proxy <- leafletProxy("mapa",session)
#         if(input$tendencia)
#         {
#           proxy %>% showGroup("tendencia")
#         }else
#         { 
#           proxy %>% hideGroup("tendencia")
#         }
#     }
#   })


#######################################################################
# definir evento para anadir o quitar la leyenda sin tener que recargar 
#######################################################################

observeEvent(input$leyenda,{
  proxy <- leafletProxy("mapa",session)
  z0 <- input$mapa_zoom
  lim <- input$mapa_bounds
  x0 <- (lim$east + lim$west)/2.0
  y0 <- (lim$north + lim$south)/2.0
  if(input$leyenda & input$buscaDatos)
  { 
    proxy %>% addLegend(position = 'topright',
                        colors = codigo.colores,
                        labels = etiqueta, 
                        opacity = 1,
                        title = 'Estado del embalse',
                        layerId="leyenda") 

  }else
  {

   proxy %>% removeControl(layerId="leyenda") %>% setView(x0,y0,zoom=z0)
  }
})
 

#############################################################################
#definir evento para anadir o quitar escala de niveles sin tener que recargar 
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

  #################################################################################
  #definir evento para anadir o quitar el grafico del estado actual de los embalses   
  #################################################################################
  
  observeEvent(input$rectangulo,{
    if(input$buscaDatos)
    {
      proxy <- leafletProxy("mapa",session)
      z0 <- input$mapa_zoom
      lim <- input$mapa_bounds
      x0 <- (lim$east + lim$west)/2.0
      y0 <- (lim$north + lim$south)/2.0
      if(input$rectangulo)
      {      
        proxy %>% showGroup("rectangulo")  %>% 
          addLegend(position = 'topright',
                    colors = codigo.colores,
                    labels = etiqueta, 
                    opacity = 1,
                    title = 'Estado del embalse',
                    layerId="leyenda")   
      }else
      {
        proxy %>% hideGroup("rectangulo") %>% removeControl("leyenda")
      }
    }
  })  
    
#######################################################################
#definir evento para anadir o quitar la capa con zonas de racionamiento
#######################################################################
  
observeEvent(input$racionamiento,{
  if(input$buscaDatos)
  {
    proxy <- leafletProxy("mapa",session)
    if(input$racionamiento)
    {      
      proxy %>% showGroup("poligono") %>% showGroup("zona") 
      proxy %>% addLegend(title="Zonas",
                                 layerId="leyenda.racionamiento",
                                 colors=c("red","green"),
                                 opacity=1.0,
                                 position="bottomright",
                                 labels=c("En racionamiento",
                                          "Susceptible a racionamiento"))  
    }else
    {
      proxy %>% hideGroup("poligono") %>% hideGroup("zona") 
      proxy %>% removeControl(layerId="leyenda.racionamiento") 
    }
  }
  
  })
  
  #######################################################################
  # definir evento para anadir o quitar los nombres de los embalses
  #######################################################################
  
  observeEvent(input$nombre,{
    proxy <- leafletProxy("mapa",session)
    if(input$nombre & input$buscaDatos)
    { 
      proxy %>% showGroup("nombre")
    }else
    {
      
      proxy %>% hideGroup("nombre")
    }
  })

# algunos intentos de implementar un hover sobre el mapa 
# observeEvent(input$mapa_shape_mouseover,{
#  loc <- input$mapa_shape_mouseover
#  proxy <- leafletProxy("mapa",session)
#  proxy %>% addMarkers(loc$lng,loc$lat, 
#           group="hover",
#           popup="oprima aqui",
#           options=markerOptions(riseOnHover=TRUE))
#           
# })
# 
# observeEvent(input$mapa_shape_mouseout,{
#   loc <- input$mapa_shape_mouseover
#   print(loc)
#   proxy <- leafletProxy("mapa",session)
#   proxy %>% hideGroup("hover")
# })


  
})


