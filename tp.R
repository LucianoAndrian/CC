
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

DJF_O = DJF_O[,, 75:105,]
MAM_O = MAM_O[,, 75:105,]
JJA_O = JJA_O[,, 75:105,]
SON_O = SON_O[,, 75:105,]
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



DJF_h = array(NA, dim = c(length(109:131), length(13:42) , 156, 3))

DJF_h[,,,1] = rx5_h[,,,12]
DJF_h[,,,2:3] = rx5_h[,,,1:2]

MAM_h = rx5_h[,,,3:5]
JJA_h = rx5_h[,,,6:8]
SON_h = rx5_h[,,,9:11]

DJF_h = DJF_h[,,126:156,] # ver bien los periodos... puede que esten corridos un año
MAM_h = MAM_h[,,126:156,]
JJA_h = JJA_h[,,126:156,]
SON_h = SON_h[,,126:156,]

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

DJF = array(NA, dim = c(length(109:131), length(13:42) , 95, 3))

DJF[,,,1] = rx5[,,,12]
DJF[,,,2:3] = rx5[,,,1:2]
MAM = rx5[,,,3:5]
JJA = rx5[,,,6:8]
SON = rx5[,,,9:11]
#
DJF_1 = DJF[,,15:45,]
MAM_1 = MAM[,,15:45,]
JJA_1 = JJA[,,15:45,]
SON_1 = SON[,,15:45,]

DJF_2 = DJF[,,55:85,]
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
}

###########################################################################################################################################
#  graficar. ggplot no alineado contorno con mapa. se debe a la resolucion??? buscar otros mapas
#  graficar con las funciones de R. buscar para interpolar los valores. raster.



prueba = bias[[1]]*mask
#prueba = DJF_O[,, 95, 1]*mask # pese a que ya esta enmascarado al interpolar hay partes que no quedan bien. --> multiplicar por mask
tiff(filename = "FIG_EJ_CLASE.tiff", res = 300, width=1500, height=1500,pointsize = 10)
par(fig=c(0,1,0,1)) 
image.plot(lon, lat, prueba, col=(tim.colors(100)), xlab="LONGITUD", ylab="LATITUD", main="probando") 
contour(lon, lat, prueba, col="red", add=TRUE, levels=100, ltw=1.4)
map(database="world2", add=TRUE, col="black", interior=TRUE) 
dev.off()




# pasando a formato ggplot (data frame lon, lat, valores)
prueba = bias[[1]]
# ver escala, hay valores puntuales muy grandes --> si se puede, saturar escala en ggplot

prueba_desarm = array(prueba, dim = 23*30)

prueba = matrix(data = NA, nrow=23*30, ncol = 3)
l=0
while(l<23*30){
  prueba[seq(l:l+23),1]<-lon
  l=l+23
}


for(j in 1:30){
  
  lat_v = array(lat[j],dim=23)
  
  prueba[(23*j-22):(j*23),2]<-lat_v
} 


prueba_desarm[which(is.na(prueba_desarm))]=0

prueba[,3]<-prueba_desarm

anom<-as.data.frame(prueba)

colnames(anom)<-c("lon", "lat", "psi")
data(wrld_simpl)
mymap <- fortify(wrld_simpl)
anom[which(anom$lon>180),][,1]<-anom[which(anom$lon>180),][,1]-360  




#mapa <- get_map(location = c(left = -90, bottom = -58, right=-34, top = 12 ),urlonly = F, source = 'stamen', maptype = "toner", color = "bw",zoom=3)

#library(metR) 
#

#world<- map_data("world")
# ggplot(world, mapping = aes(x = long, y = lat) )+
#   #geom_contour_fill(data = anom,aes(x = lon, y = lat, z = psi),alpha = 0.5)+
#  geom_polygon() +
#  coord_map(projection = "mercator") +
#  coord_cartesian(xlim = c(-110, -30), ylim = c(-60, 35))+
#   geom_contour_fill(data = anom,aes(x = lon, y = lat, z = psi),alpha = 0.5)





#ggplot()+
#ggmap(mapa, extent = "normal")+geom_contour_fill(data = anom,aes(x = lon, y = lat, z = psi),alpha = 0.5)+
  #geom_map(data = mapa, map = mapa, aes(x = long, y = lat, map_id = id),fill = "grey", color = "black",lwd=0.5)
  #geom_contour_fill(data = anom,aes(x = lon, y = lat, z = psi),alpha = 0.05)+
  
  #coord_map()
  #scale_fill_gradientn(name=expression(Psi),limits=c(-100,100),colors = c("royalblue", "white", "red"),space = "Lab")
  #scale_fill_viridis_c()+
  #geom_contour2(data = anom, aes(x = lon, y = lat, z = psi), breaks = 0)+
  #ggtitle(paste("EB2P2 dia", i, ".jpg", sep="")) + theme(plot.title = element_text(hjust=1)) +
  #theme_bw()
#titulo =paste("dia",i,".jpg", sep ="")
#ggsave(titulo,V,width = 35,height = 15 ,units = "cm")