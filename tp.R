
rm(list=ls()) 
setwd("/home/auri/Facultad/Materias/Cambio_climatico/Tp_final/")
library(ncdf4)
library(maps)
library(ncdf4)
require(fields)
require(mapdata)
path<-"/home/auri/Facultad/Materias/Cambio_climatico/Tp_final"

#############################################################################################################

# observado de hadex2, (ghcndex practicamente no tiene datos)

nc_o = nc_open(paste(path, "rx5day_obs.nc", sep = "/")) #1901-2010
nombre_var_o = names(nc_o$var)

# desde el tiempo 61 datos en SA

OBS = list()

for( i in 1:12){
  OBS[[i]] = ncvar_get(nc_o, nombre_var_o[i])  
}

# pensar manera menos chota de hacer esto

DJF_O = array(NA, dim = c(length(109:131), length(13:42) , 110, 3))
MAM_O = array(NA, dim = c(length(109:131), length(13:42) , 110, 3))
JJA_O = array(NA, dim = c(length(109:131), length(13:42) , 110, 3))
SON_O = array(NA, dim = c(length(109:131), length(13:42) , 110, 3))

DJF_O[,,,1] = OBS[[12]][109:131, 13:42,] 
DJF_O[,,,2] = OBS[[1]][109:131, 13:42,] 
DJF_O[,,,3] = OBS[[2]][109:131, 13:42,] 

MAM_O[,,,1] = OBS[[3]][109:131, 13:42,]  
MAM_O[,,,2] = OBS[[4]][109:131, 13:42,]  
MAM_O[,,,3] = OBS[[5]][109:131, 13:42,]  

JJA_O[,,,1] = OBS[[6]][109:131, 13:42,]  
JJA_O[,,,2] = OBS[[7]][109:131, 13:42,]  
JJA_O[,,,3] = OBS[[8]][109:131, 13:42,]  

SON_O[,,,1] = OBS[[9]][109:131, 13:42,]  
SON_O[,,,2] = OBS[[10]][109:131, 13:42,]  
SON_O[,,,3] = OBS[[11]][109:131, 13:42,]  

DJF_O = DJF_O[,, 76:105,]
MAM_O = MAM_O[,, 76:105,]
JJA_O = JJA_O[,, 76:105,]
SON_O = SON_O[,, 76:105,]
#############################################################################################################

# historico del modelo

nc_h = nc_open(paste(path, "rx5day_hist.nc", sep = "/")) #1850-2006

nombre_var_h = names(nc_h$var)

rx5day_h = ncvar_get(nc_h, nombre_var_h[2])

rx5_h = array(NA, dim = c(length(109:131), length(13:42) , 156, 12)) # 3 dim = anios, 4dim = meses

for(j in 1:12){
  for (i in 0:(155)){
    rx5_h[,,1+i,j] = rx5day_h[109:131, 13:42, j+12*i]
  }
}



DJF_h = array(NA, dim = c(length(109:131), length(13:42) , 156, 12))

DJF_h[,,,1] = rx5_h[,,,12]
DJF_h[,,,2:3] = rx5_h[,,,1:2]

MAM_h = rx5_h[,,,3:5]
JJA_h = rx5_h[,,,6:8]
SON_h = rx5_h[,,,9:11]

DJF_h = DJF_h[,,125:154,] # ver bien los periodos... puede que esten corridos un año
MAM_h = MAM_h[,,125:154,]
JJA_h = JJA_h[,,125:154,]
SON_h = SON_h[,,125:154,]

#############################################################################################################

# proyeccion 

nc<-nc_open(paste(path, "rx5day_i.nc", sep="/")) #2006-2100

nombres_dimensiones = names(nc$dim)
datos_dimensiones = 0

for (i in 1:length(nombres_dimensiones)){
  if(i != 2){
    datos_dimensiones[i] = list(ncvar_get(nc, nombres_dimensiones[i]))
    } else { 
      next
    }
}   

names(datos_dimensiones) = nombres_dimensiones

lat = datos_dimensiones$lat[13:42]
lon = datos_dimensiones$lon[109:131]

nombre_var = names(nc$var)
rx5day = ncvar_get(nc, nombre_var[2]) # dim = lon, lat, tiempo. son 1140 tiempos correspondientes a cada mes
# desde enero de 2006 hhasta 2100. = 95 años * 12 meses

rx5 = array(NA, dim = c(length(109:131), length(13:42) , 95, 12)) # 3 dim = anios, 4dim = meses

for(j in 1:12){
  for (i in 0:(94)){
    rx5[,,1+i,j] = rx5day[109:131, 13:42, j+12*i]
  }
}

DJF = array(NA, dim = c(length(109:131), length(13:42) , 95, 12))

DJF[,,,1] = rx5[,,,12]
DJF[,,,2:3] = rx5[,,,1:2]
MAM = rx5[,,,3:5]
JJA = rx5[,,,6:8]
SON = rx5[,,,9:11]
#
#DJF = DJF[,,15:44,]
#MAM = MAM[,,15:44,]
#JJA = JJA[,,15:44,]
#SON = SON[,,15:44,]
#############################################################################################################

# mascara  

nc_mask = nc_open(paste(path, "mask.nc", sep="/"))
mask = ncvar_get(nc_mask, "mask")
mask = mask[109:131, 13:42]


#############################################################################################################
# analisis.
# puede que ande.

prueba = apply(SON_h, c(1,2,4), mean) # Esta bien
prueba2 = apply(prueba, c(1,2), mean)

prueba3 = apply(SON_O, c(1,2,4), mean)
prueba4 = apply(prueba3, c(1,2), mean)

# a partir de esto llevar a formato ggplot2 lon lat valores. ver untentando_graficar_nc.R
#bias
prueba5 = (prueba2/prueba4)*100-100
# ver escala, hay valores puntuales muy grandes --> si se puede, saturar escala en ggplot

######## esto grafica --> pasar a ggplot2. (CON LA GRILLA 2.5 QUEDA HORRIBLE) es necesario armar data frame (lon, lat, valores)
prueba = JJA[,,1]*mask
#prueba = DJF_O[,, 95, 1]*mask # pese a que ya esta enmascarado al interpolar hay partes que no quedan bien. --> multiplicar por mask
tiff(filename = "FIG_EJ_CLASE.tiff", res = 300, width=1500, height=1500,pointsize = 10)
par(fig=c(0,1,0,1)) 
image.plot(lon, lat, prueba, col=(tim.colors(100)), xlab="LONGITUD", ylab="LATITUD", main="probando") 
contour(lon, lat, prueba, col="black", add=TRUE, levels=1, ltw=1.4)
map(database="world2", add=TRUE, col="black", interior=TRUE) 
dev.off()
########



#bias del modelo paper carla. modelo/observador*100 -100
#usar los datos observado 
#ver cambios en la proyecciones igual q en el tp4
#para esto ultimo tomar periodos similares 30 anios
#bajar las otras corridas (hay 2 mas?)