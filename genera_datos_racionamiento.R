library(leaflet)
library(maptools)
library(spatstat)
library(sp)

load("mapaPR.RData")

datos <- read.table(file="zonas.dat",header=TRUE,sep="\t")

datos$municipio <- toupper(datos$municipio)
datos$color     <- "green"

#inx.color <- grepl("Carraizo",datos$zona)

inx.color <- grep("Carraizo|Guaynabo",datos$zona)

datos$color[inx.color] <- "red"

x0 <- -66.5   
y0 <-  18.25
z0 <- 10  

# inx.duplicated <- !duplicated(cbind(datos$lon,datos$lat,fromLast=TRUE))
# 
# datos <- datos[inx.duplicated,]

lista.muni <- unique(datos$municipio)

poli       <- list()
datos.muni <- list()
municipio  <- list()
colores    <- list()

for(inx in lista.muni)
{
  temp <- mapa.municipio[mapa.municipio$NAME == inx,]
  
  poli[[inx]] <- data.frame(temp@polygons[[1]]@Polygons[[1]]@coords)
  
  names(poli[[inx]]) <- c("lng","lat")
  
  datos.muni[[inx]] <- data.frame(datos[datos$municipio == inx,]$lon,
                                  datos[datos$municipio == inx,]$lat,
                                  datos[datos$municipio == inx,]$lugar,
                                  datos[datos$municipio == inx,]$municipio,
                                  datos[datos$municipio == inx,]$zona,
                                  datos[datos$municipio == inx,]$color)
  
  names(datos.muni[[inx]]) <- c("lng","lat","lugar","municipio","zona","color")
  
  df <- remove.duplicates(SpatialPoints(data.frame(lng=datos.muni[[inx]]$lng,lat=datos.muni[[inx]]$lat)))
  
  p1.x <- df$lng
  p1.y <- df$lat 
  
  p2.x <- poli[[inx]]$lng
  p2.y <- poli[[inx]]$lat
  
  inx.good <- point.in.polygon(p1.x,p1.y,p2.x,p2.y) == 1
  
  datos.muni[[inx]] <- datos.muni[[inx]][inx.good,]
}

save(lista.muni,poli,datos.muni,file="datos.racionamiento.RData")
