
#####################################################
# funcion para normalizar entre nivel minimo y maximo 
#####################################################

normalizaNivel <- function(x,xmin,xmax)
{
  return((x-xmin)/(xmax-xmin))
}

###########################################
# funcion para hacer normalizacion de 0 a 1
###########################################

range01 <- function(x){(x-min(x))/(max(x)-min(x))}


#############################################
# funcion para convertir un vector a numerico
#############################################

convierteNumerico <- function(vector)
{
  return(as.numeric(as.character(vector)))
}


#############################################
## funcion para buscar datos de nivel en USGS
#############################################

buscaNiveles <- function(misiteID,startDate="2014-05-2",endDate="2015-06-05")
{
  minombre <- df$nombre[df$siteID == misiteID]
  url.parte1 <- "http://nwis.waterdata.usgs.gov/pr/nwis/uv/?cb_62616=on&format=rdb&site_no="
  url.parte2 <- paste0(misiteID,"&period=&begin_date=",startDate,"&end_date=",endDate)
  url.final <- paste0(url.parte1,url.parte2)
  datos <- read.table(url.final,sep="\t",header=TRUE)
  names(datos) <- c("agency","site","datetime","codigo","nivel","status")
  fecha.tiempo <- datos$datetime[-1]
  nivel.tiempo <- convierteNumerico(datos$nivel[-1])
  #tendencia <- tail(nivel.tiempo,n=1) - head(nivel.tiempo,n=1)
  tendencia <- mean(diff(nivel.tiempo))
  df.niveles <- data.frame(fecha=tail(fecha.tiempo,n=1),
                           nivel=tail(nivel.tiempo,n=1),
                           tendencia=tendencia,
                           stringsAsFactors = FALSE)
  return(df.niveles)
}

df <- read.csv("embalses.csv",
                           header=TRUE,
                           comment.char="#")

attach(df)

siteID <- paste(siteID)

names(siteID) <- nombre

#save.image("embalsespr.RData")