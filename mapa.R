mapa = function(lista, titulo1, nombre, label){
  
  library(maps)
  require(fields)
  require(mapdata)
  library(ggplot2)
  library(metR)
  library(RColorBrewer)
  library(mapproj)
  
  titulo = c("DJF", "MAM", "JJA", "SON")
  for(i in 1:4){
    value = array(lista[[i]]*mask, dim = 23*30)
    data = matrix(data = NA, nrow=23*30, ncol = 3)
    
    l=0
    while(l<23*30){
      data[seq(l:l+23),1]<-lon
      l=l+23
    }
    
    for(j in 1:30){
      lat_v = array(lat[j],dim=23)
      data[(23*j-22):(j*23),2]<-lat_v
    } 
    
    data[,3]<-value
    error<-as.data.frame(data)
    
    colnames(error)<-c("lon", "lat", "rx5")
    
    error[which(error$lon>180),][,1]<-error[which(error$lon>180),][,1]-360  
    
    mapa <- map_data("world", regions = c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname", "Colombia", "Venezuela",
                                          "Bolivia", "Ecuador", "Chile", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", "Nicaragua"), 
                     colour = "black")
    
    g <- ggplot() + theme_minimal()+
      xlab("Longitud") + ylab("Latitud") + 
      theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank())+
      
      geom_tile(data = error, aes(x = lon, y = lat, fill = rx5), alpha=1, na.rm = T)+
      
      scale_fill_gradientn(limits = c(-100,100), name = label,colours = (brewer.pal(n=11,"RdYlBu")), na.value = "white")+
      
      geom_polygon( data = mapa, aes(x = long, y = lat, group = group), fill = NA, color = "black") +#coord_map("stereographic", orientation = c(-35, -56, 0))+
      ggtitle(paste(titulo1, " - " , titulo[i], sep = ""))+
      scale_y_continuous(limits = c(-60, 15)) +
      scale_x_continuous(limits = c(-90, -30))+
      theme(axis.text.y   = element_text(size=14), axis.text.x   = element_text(size=14), axis.title.y  = element_text(size=14),
            axis.title.x  = element_text(size=14), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
            panel.border = element_rect(colour = "black", fill=NA, size=3),
            panel.ontop = TRUE,
            plot.title = element_text(hjust=0.5))
    ggsave(paste("/home/auri/Facultad/Materias/Cambio_climatico/Tp_final/salidas/",nombre, "_", titulo[i], ".jpg",sep =""), plot = g, width = 15, height = 15  , units = "cm")
  }
  
}




