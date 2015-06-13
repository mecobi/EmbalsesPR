library(leaflet)
mapa <- 
leaflet() %>% addTiles() %>% 
  setView(-66.5,18.25,zoom=10) %>%
  addWMSTiles(
    "http://nowcoast.noaa.gov/wms/com.esri.wms.Esrimap/obs",
    layers="RAS_RIDGE_NEXRAD",
    options = WMSTileOptions(format = "image/png", 
                             transparent = TRUE))
print(mapa)
