
rm(list=ls()) 
setwd("/home/auri/Facultad/Materias/Cambio_climatico/Tp_final/")
library(ncdf4)
library(maps)
library(ncdf4)
require(fields)
require(mapdata)
path<-"/home/auri/Facultad/Materias/Cambio_climatico/Tp_final"

library(ggplot2)
library(maps)
library(readxl)
library(maptools)
library(ggplot2)
library(ggmap)
library(mapproj)
library(grid)
library(gridExtra)
library(akima)
library(gganimate)
library(RColorBrewer)

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

DJF_O = array(NA, dim = c(length(109:131), length(13:42) , 31, 3))
MAM_O = array(NA, dim = c(length(109:131), length(13:42) , 31, 3))
JJA_O = array(NA, dim = c(length(109:131), length(13:42) , 31, 3))
SON_O = array(NA, dim = c(length(109:131), length(13:42) , 31, 3))

DJF_O[,,,1] = OBS[[12]][109:131, 13:42,74:104] 
DJF_O[,,,2] = OBS[[1]][109:131, 13:42, 75:105] 
DJF_O[,,,3] = OBS[[2]][109:131, 13:42, 75:105] 

MAM_O[,,,1] = OBS[[3]][109:131, 13:42, 75:105] 
MAM_O[,,,2] = OBS[[4]][109:131, 13:42, 75:105] 
MAM_O[,,,3] = OBS[[5]][109:131, 13:42, 75:105] 

JJA_O[,,,1] = OBS[[6]][109:131, 13:42, 75:105] 
JJA_O[,,,2] = OBS[[7]][109:131, 13:42, 75:105] 
JJA_O[,,,3] = OBS[[8]][109:131, 13:42, 75:105] 

SON_O[,,,1] = OBS[[9]][109:131, 13:42, 75:105] 
SON_O[,,,2] = OBS[[10]][109:131, 13:42, 75:105] 
SON_O[,,,3] = OBS[[11]][109:131, 13:42, 75:105] 

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

DJF_h = array(NA, dim = c(length(109:131), length(13:42) , 31, 3))

DJF_h[,,,1] = rx5_h[,, 125:155, 12]
DJF_h[,,,2:3] = rx5_h[,, 126:156, 1:2]

MAM_h = rx5_h[,, 126:156, 3:5]
JJA_h = rx5_h[,, 126:156, 6:8]
SON_h = rx5_h[,, 126:156, 9:11]

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

DJF1 = array(NA, dim = c(length(109:131), length(13:42) , 31, 3))
DJF2 = array(NA, dim = c(length(109:131), length(13:42) , 31, 3))

DJF1[,,,1] = rx5[,,14:44,12] #  CORREGIR!!! EL DICIEMBRE DE UN AÑO FORMA PARTE DEL DJF DE ESE MISMOS AÑO... SI SERAS PELOTUDOOO!!!
DJF1[,,,2:3] = rx5[,,14:44,1:2]

DJF2[,,,1] = rx5[,,54:84,12]
DJF2[,,,2:3] = rx5[,,54:84,1:2]

MAM = rx5[,,,3:5]
JJA = rx5[,,,6:8]
SON = rx5[,,,9:11]
#
DJF_1 = DJF1
MAM_1 = MAM[,,15:45,]
JJA_1 = JJA[,,15:45,]
SON_1 = SON[,,15:45,]

DJF_2 = DJF2
MAM_2 = MAM[,,55:85,]
JJA_2 = JJA[,,55:85,]
SON_2 = SON[,,55:85,]
#############################################################################################################

# mascara  

nc_mask = nc_open(paste(path, "mask.nc", sep="/"))
mask = ncvar_get(nc_mask, "mask")
mask = mask[109:131, 13:42]


#############################################################################################################
# analisis.

historico = list(DJF_h, MAM_h, JJA_h, SON_h)  # ojo que aveces toma DJf_h con 4ta dim = 12... no se porque
observado = list(DJF_O, MAM_O, JJA_O, SON_O)
proyeccion_1 = list(DJF_1, MAM_1, JJA_1, SON_1)
proyeccion_2 = list(DJF_2, MAM_2, JJA_2, SON_2)

v = list(historico, observado, proyeccion_1, proyeccion_2)


promedios = v

for(i in 1:4){
  for(j in 1:4){
    suma = apply(v[[i]][[j]], c(1,2,3), sum)
    promedios[[i]][[j]] = apply(suma, c(1,2), mean)
  }
}

# todo esto anda bien



#prueba=apply(MAM_O, c(1,2,3), sum) #acumulado en cada trimestre de cada año
#prueba2 = apply(prueba, c(1,2), mean, na.rm=T) #promedio del acumulado de cada trimestre en todos los años


# a partir de esto llevar a formato ggplot2 lon lat valores. ver untentando_graficar_nc.R
#bias
bias = list()
for(i in 1:4){
  bias[[i]] = (promedios[[1]][[i]]/promedios[[2]][[i]])*100-100
  bias[[i]][which(!is.na(bias[[i]])&bias[[i]]>100)]=100
  #bias[[i]][which(!is.na(bias[[i]])&bias[[i]]<(-100))]=-100     # no hace falta en ningun caso.
  
}

dif_1 = list()
for(i in 1:4){
  dif_1[[i]] = (promedios[[3]][[i]] - promedios[[1]][[i]])
  #dif_1[[i]] =(dif_1[[i]]/promedios[[1]][[i]])*100  #%
  dif_1[[i]][which(!is.na(dif_1[[i]])&dif_1[[i]]>100)]=150
  #dif_1[[i]][which(!is.na(dif_1[[i]])&dif_1[[i]]<(-100))]=-150
}

dif_2 = list()
for(i in 1:4){
  dif_2[[i]] = (promedios[[4]][[i]] - promedios[[1]][[i]])
  dif_2[[i]][which(!is.na(dif_2[[i]])&dif_2[[i]]>100)]=150
  #dif_2[[i]][which(!is.na(dif_2[[i]])&dif_2[[i]]<(-100))]=-150
  
}


###########################################################################################################################################
#  graficar. ggplot no alineado contorno con mapa. se debe a la resolucion??? buscar otros mapas
#  graficar con las funciones de R. buscar para interpolar los valores. raster.
source("mapa.R")
source("mapa2.R")

mapa(bias, "Bias 2005 - 1975","bias","%")
mapa2(dif_1, "Período 2050-2020 respecto de 2005-1975", "dif1", "mm")
mapa2(dif_2, "Período 2090-2060 respecto de 2005-1975", "dif2", "mm")
