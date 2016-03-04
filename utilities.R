##############################################
# utilities.R - rutinas utiles para embalsespr
# Elio Ramos 
# CienciaDatosPR
# Departamento de Matematica 
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

#####################################################################################
# funcion para generar los "breaks" de tiempos con AM/PM en el eje de x de la grafica
#####################################################################################

genera_break <- function(x)
{
  x.temp <- x 
  dx     <- 4
  if(length(x) > 5)
  {
    inx <- seq(1,length(x),round(length(x)/dx))
  }else
  {
    inx <- seq(1,length(x))
  }
  am <- rep("AM",length(x))
  am[which(x >= 12)] <- "PM"
  x[which(x > 12)] <- x[which(x > 12)] - 12
  x.m <- paste(floor(x), sprintf("%02d",round((x-floor(x))*60)), sep=":")
  res <- gsub("^0:","12:",paste0(x.m,am))
  return(data.frame(breaks24=x.temp[inx],breaks12=res[inx]))
  
}

#####################################################################
# funcion para generar el url que se necesita en portal del USGS para 
# una cierta fecha y codigo de la estacion 
#####################################################################

generaURL <- function(startDate,endDate,misiteID)
{
  #url.parte1 <- "http://nwis.waterdata.usgs.gov/pr/nwis/uv/?cb_62616=on&format=rdb&site_no="
  url.parte1 <- "http://waterdata.usgs.gov/pr/nwis/uv/?format=rdb&site_no="
  url.parte2 <- paste0(misiteID,"&period=&begin_date=",startDate,"&end_date=",endDate)
  url.final <- paste0(url.parte1,url.parte2)
  return(url.final)
}


############################################
# funcion para buscar datos de nivel en USGS
############################################

buscaNiveles <- function(misiteID,startDate=fecha1,endDate=fecha2)
{
  minombre <- df$nombre[df$siteID == misiteID]
  incProgress(0.36,detail=toupper(minombre))
  url.final <- generaURL(startDate,endDate,misiteID)
  #print(url.final)
  #check.url <- url.exists(url.final)
  #print(paste0(minombre," - ",check.url),quote=FALSE)
  datos <- read.table(url.final,sep="\t",header=TRUE)
  #print(tail(datos,n=1))
  
  if(length(datos) > 6)
  {
    #names(datos) <- c("agency","site","datetime","codigo","nivel","status")
    fecha.tiempo <- datos$datetime[-1]
    inx.nivel<- grep("_62616",names(datos))[1] 
    nivel.tiempo <- round(convierteNumerico(datos[-1,inx.nivel]),digits=2)
    print(nivel.tiempo)
    tendencia <- mean(diff(nivel.tiempo),na.rm=TRUE)
    print(tendencia)
    cambio.nivel <- tail(nivel.tiempo,n=1) - nivel.tiempo[1]
    desviacion.estandar <- sd(nivel.tiempo,na.rm=TRUE)
    par(mar=c(0,0,0,0))
    x.data <- hour(fecha.tiempo) + minute(fecha.tiempo)/60
    xx <- genera_break(x.data)
    titulo <- paste0("Nivel de embalse ",toupper(minombre),"\n en las pasadas ",round(max(x.data)-min(x.data))," horas")
    #titulo <- paste0(toupper(minombre),"\n en las pasadas ",round(max(x.data)-min(x.data))," horas"," ",cambio.nivel.cm)
    y.lim1  <- c(min(nivel.tiempo) - 10*desviacion.estandar,max(nivel.tiempo) + 10*desviacion.estandar)
    y.lim2  <- c(min(nivel.tiempo) - desviacion.estandar,max(nivel.tiempo) + desviacion.estandar)
    # grafica con el nivel desde la medianoche hasta el presente
    if(desviacion.estandar < 1.0e-2)
    {
     grafica <- qplot(x.data,nivel.tiempo,geom="line",
                      xlab="Tiempo",ylab="NIVEL [metros]",
                      ylim=y.lim1) 
    }else
    {
      grafica <- qplot(x.data,nivel.tiempo,geom="line",
                       xlab="Tiempo",ylab="NIVEL [metros]",
                       ylim=y.lim2)
    }
    
    grafica <- grafica + scale_x_continuous(breaks=xx$breaks24,labels=paste0(xx$breaks12,xx$am)) + 
                         geom_line(colour="red") + ggtitle(titulo) +  
                         theme(legend.position="none",
                               axis.title.x=element_text(size=rel(1)),
                               axis.title.y=element_text(size=rel(1)),
                               axis.text.x=element_text(size=rel(1.2)),
                               axis.text.y=element_text(size=rel(1.2)),
                               panel.border = element_rect(colour = "black", fill=NA, size=1),
                               plot.title = element_text(size = rel(1.2), colour = "blue"))  
    
    # grafica se almacena en archivo .png para luego usarse en el "popup
    ggsave(paste0("./www/",misiteID,".png"),width=4,height=4)
    dev.off()
    df.niveles <- data.frame(fecha=tail(fecha.tiempo,n=1),
                             nivel=tail(nivel.tiempo,n=1),
                             tendencia=tendencia,
                             cambio.nivel =cambio.nivel,
                             stringsAsFactors = FALSE)
  }else
  {
     #df.niveles <- NULL 
     mihora <- Sys.time()
     hora.local <- hour(mihora) + minute(mihora)/60
     x <- seq(0,hora.local,0.25)
     fecha <- paste(fecha1,paste(floor(x), sprintf("%02d",round((x-floor(x))*60)), sep=":"))
     nivel <- rep( df$desborde[df$siteID == misiteID]+0.1,length(fecha))
     grafica <- qplot(x,nivel,geom="line",
                      xlab="Tiempo",ylab="NIVEL [metros]",
                      ylim=c(-10,10)) 
     grafica <- grafica + 
       geom_line(colour="red") + ggtitle("NO hay datos disponibles") +  
       theme(legend.position="none",
             axis.title.x=element_text(size=rel(1)),
             axis.title.y=element_text(size=rel(1)),
             axis.text.x=element_text(size=rel(1.2)),
             axis.text.y=element_text(size=rel(1.2)),
             panel.border = element_rect(colour = "black", fill=NA, size=1),
             plot.title = element_text(size = rel(1.2), colour = "blue")) 
     ggsave(paste0("./www/",misiteID,".png"),width=4,height=4)
     #dev.off()
     df.niveles <- data.frame(fecha=tail(fecha,n=1),
                              nivel=tail(nivel,n=1),
                              tendencia=-99,
                              cambio.nivel=-99)
  }
  #print(df.niveles)
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
