rm(list = ls())
library(ggplot2)
library(dataRetrieval)
library(reshape)
library(leaflet)
library(lubridate)

setwd("~/embalses")
load("embalsespr.RData")

startDate <- Sys.Date()
endDate   <- Sys.Date()

fecha  <- NULL 
nivel  <- NULL 

for(inxSite in 1:length(siteID))
{
  temp <- NULL 
  temp    <- tail(buscaNiveles(siteID[inxSite],startDate,endDate),n=1)
  fecha   <- c(fecha,paste(temp$fecha))
  nivel   <-c(nivel,temp$nivel)
}

df <- data.frame(lat=lat,lng=lon,siteID=siteID,fecha=fecha,
                 nivel=nivel,desborde=desborde,seguridad=seguridad,
                 observacion=observacion,ajuste=ajuste,control=control)

micolor <- NULL 

for(siteInx in 1:length(df$nivel))
{
  micolor[siteInx] <- asignaColor(df,siteInx)
}

nivel.norm <- normalizaNivel(df$nivel,df$ajuste,df$desborde)

mysize <- 4000

# mapa inicial 

mapa <- leaflet(df) %>% 
  addTiles()      %>%
  setView(-66.5,18.25,zoom=10)

# circulos vacios 

mapa <- mapa %>%
  addCircles(fill=TRUE,
             color="black",
             stroke=TRUE,
             weight=1,
             opacity=1,
             radius=mysize) 

# circulos llenos 

mapa <- mapa %>%
  addCircles(fill=TRUE,
             color=micolor,
             stroke=FALSE,
             weight=1,
             fillOpacity=0.75,
             opacity=1,
             radius=mysize*nivel.norm,
             popup = paste0(rownames(df),"\n ","Nivel: ",df$nivel," m"))

mapa 
