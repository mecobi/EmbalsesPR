
########################################################################
# genera_mapaPR.R - Rutina para generar archivo con datos del mapa de PR
# Elio Ramos 
# CienciaDatosPR
# Departamento de Matematica 
# Universidad de Puerto Rico en Humacao 
########################################################################


library(ggplot2)
library(maptools)
library(spatstat)

gpclibPermit()

mapa.municipio <- readShapePoly("./mapa_pr/mapa_pr")

# algunas correciones 

mapa.municipio$NAME <- gsub("\xf1","ñ",mapa.municipio$NAME)
mapa.municipio$NAME <- gsub("\xfc","ü",mapa.municipio$NAME)

mapa.municipio$NAME <- toupper(mapa.municipio$NAME)  

mapa.municipio.merge <- unionSpatialPolygons(mapa.municipio,
                                             rep(1,length(mapa.municipio)))

poligonos <- mapa.municipio.merge@polygons

poligono.pr       <- coordinates(poligonos[[1]]@Polygons[[2]])
poligono.vieques  <- coordinates(poligonos[[1]]@Polygons[[1]])
poligono.culebra  <- coordinates(poligonos[[1]]@Polygons[[3]])
                           
x.pr <- poligono.pr[-1,1]
y.pr <- poligono.pr[-1,2]

x.vieques <- poligono.vieques[-1,1]
y.vieques <- poligono.vieques[-1,2]

x.culebra <- poligono.culebra[-1,1]
y.culebra <- poligono.culebra[-1,2]

# ventana de observacion en el mapa

w <- owin(poly=list(list(x=rev(x.pr),y=rev(y.pr)),list(x=rev(x.vieques),y=rev(y.vieques)),
                list(x=rev(x.culebra),y=rev(y.culebra))))

mapa.municipio_f <- fortify(mapa.municipio)

mapa.municipio.merge_f <- fortify(mapa.municipio.merge)


save.image(file="mapaPR.RData")
