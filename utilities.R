##############################################
# utilities.R - rutinas utiles para embalsespr
# Elio Ramos 
# CienciaDatosPR
# Departamento de Matemática 
# Universidad de Puerto Rico en Humacao 
##############################################

# hacer busqueda en el servidor de USGS para el dia de hoy 
# los datos que devuelve comienzan a las 12 de la medianoche 
# del dia en curso. En el mapa se muestra la medida mas reciente
# los otras medidad se utilizan para calcular la tendencia. 

fecha1 <- Sys.Date()
fecha2 <- Sys.Date()


# tamaño base de los rectángulos 

mysize <- 4010

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


#########################################################
# funcion para convertir un vector de factores a numerico
#########################################################

convierteNumerico <- function(vector)
{
  return(as.numeric(as.character(vector)))
}

############################################
# funcion para buscar datos de nivel en USGS
############################################

buscaNiveles <- function(misiteID,startDate=fecha1,endDate=fecha2)
{
  minombre <- df$nombre[df$siteID == misiteID]
  incProgress(0.1,detail=toupper(minombre))
  url.parte1 <- "http://nwis.waterdata.usgs.gov/pr/nwis/uv/?cb_62616=on&format=rdb&site_no="
  url.parte2 <- paste0(misiteID,"&period=&begin_date=",startDate,"&end_date=",endDate)
  url.final <- paste0(url.parte1,url.parte2)
  datos <- tryCatch(read.table(url.final,sep="\t",header=TRUE),finally=NULL)
  if(!is.null(datos) > 0)
  {
  names(datos) <- c("agency","site","datetime","codigo","nivel","status")
  fecha.tiempo <- datos$datetime[-1]
  nivel.tiempo <- convierteNumerico(datos$nivel[-1])
  tendencia <- mean(diff(nivel.tiempo))
  df.niveles <- data.frame(fecha=tail(fecha.tiempo,n=1),
                           nivel=tail(nivel.tiempo,n=1),
                           tendencia=tendencia,
                           stringsAsFactors = FALSE)
  }else
  {
    df.niveles <- NULL 
  }
  return(df.niveles)
}

############################################################################################
# para leer archivo con los datos de cada uno de los 11 embalses y colocarlo en un dataframe
############################################################################################

df <- read.csv("embalses.csv",
                           header=TRUE,
                           comment.char="#")

attach(df)

# para convertir los siteIDs de USGS a cadena de caracteres

siteID <- paste(siteID)

names(siteID) <- nombre
