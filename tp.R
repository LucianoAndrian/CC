
rm(list=ls()) 
setwd("/home/auri/Facultad/Materias/Cambio_climatico/Tp_final/")
library(ncdf4)
library(maps)

# observado practicamente nada en sudamerica

archivo<-"rx5day_i.nc"
path<-"/home/auri/Facultad/Materias/Cambio_climatico/Tp_final"

nc<-nc_open(paste(path,archivo,sep="/"))

nombre_var = names(nc$var)
nombres_dimensiones = names(nc$dim)
datos_dimensiones = 0

for (i in 1:length(nombres_dimensiones)){
  if(i != 2){
    datos_dimensiones[i] = list(ncvar_get(nc,nombres_dimensiones[i]))
    } else { 
      next
    }
}   

names(datos_dimensiones) = nombres_dimensiones

lat = datos_dimensiones$lat
lon = datos_dimensiones$lon

ncatt_get(nc,"time","units") 

#
fechas = as.Date(datos_dimensiones$time/24,origin="2006-01-16")

rx5day = ncvar_get(nc,nombre_var[2]) # dim = lon, lat, tiempo. son 1140 tiempos correspondientes a cada mes
                                      # desde enero de 2006 hhasta 2100. = 95 años * 12 meses


rx5 = array(NA, dim = c(144, 73, 95, 12)) # 3 dim = anios, 4dim = meses

for(j in 1:12){
  for (i in 0:(94)){
    rx5[,,1+i,j] = rx5day[,,j+12*i]
  }
}


DJF = array(NA, dim = c(144, 73, 95, 3))

DJF[,,,1] = rx5[,,,12]
DJF[,,,2:3] = rx5[,,,1:2]

MMA = rx5[,,,3:5]
JJA = rx5[,,,6:8]
SON = rx5[,,,9:11]




apply(pp1[2:34,,,2:4],c(1,2,4),sum)



# esto grafica --> pasar a ggplot2. (CON LA GRILLA 2.5 QUEDA HORRIBLE) es necesario armar data frame (lon, lat, valores)
tiff(filename = "FIG_EJ_CLASE.tiff", res = 300, width=2000, height=1000,pointsize = 10)
par(fig=c(0,1,0,1)) 

image.plot(datos_dimensiones$lon,datos_dimensiones$lat,prueba,
           col=(tim.colors(100)), xlab="LONGITUD", ylab="LATITUD", main="Dia 10 anomalia funcion corriente") 
contour(datos_dimensiones$lon, datos_dimensiones$lat, 
        prueba,col="black", add=TRUE,levels=0,ltw=1.4)
map(database="world2", add=TRUE, col="black", interior=TRUE) 
dev.off()




