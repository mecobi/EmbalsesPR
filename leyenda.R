library(leaflet)

colores <- rev(c("darkorange","yellow","blue","green"))
labels  <- c("seguridad","observación","ajuste","control")

mapa <- leaflet() %>% addTiles() %>% addLegend(
  position = 'topright',
  colors = rgb(t(col2rgb(colores))/ 255),
  labels = labels, opacity = 1,
  title = 'Estado del embalse')

mapa <- mapa %>% 

print(mapa)