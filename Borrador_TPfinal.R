#Comienzo borrador Trabajo final


# ITEM A - Leer el archivo y extraer info ---------------------------------

require(ncdf4)
library(udunits2)
library(metR)
library(lubridate)
library(ggplot2)

#nc<-nc_open ("/home/clinux01/Escritorio/Lara/Trabajo_Final_Hielo_Marino/icec.sfc.mon.mean.nc") #LINUX
nc<-nc_open ("/home/clinux01/Escritorio/Lara/Trabajo_Final/icec.sfc.mon.mean.nc") #LINUX COMPU JUEVES
#nc <- nc_open("C:/Users/Usuario/Documents/Lara/Trabajo_Final_Hielo_Marino/icec.sfc.mon.mean.nc") #WINDOWS
nc

# WINDOWS archivo <- "C:/Users/Usuario/Documents/Lara/Trabajo_Final_Hielo_Marino/icec.sfc.mon.mean.nc"
archivo <- "/home/clinux01/Escritorio/Lara/Trabajo_Final/icec.sfc.mon.mean.nc" #Ojo cambio nombre carpeta
GlanceNetCDF(archivo)
datos <- ReadNetCDF(archivo, vars = "icec")
hist(datos$icec)
dim(datos) #16405632  4 Es un dataframe



# ITEM B - Selecciono Antártida período 1990-2019 ---------------

antartida <- ReadNetCDF(archivo, vars = "icec",
                        subset = list(lat = c(-60,-88), #va hasta -88
                                      lon = c(0,358))) 

datos_antartida_periodo<- antartida[which(year(antartida$time) %in% 1990:2019),]

# ITEM C - Selecciono las bases de interés. Serie temporal ----------------

datos_esperanza_totales<- ReadNetCDF(archivo, vars = "icec",
                          subset = list(lat = c(-63.396958),
                                        lon = c(360-56.998053)))
datos_davis_totales <- ReadNetCDF(archivo, vars = "icec",
                                  subset = list(lat = c(-68.469637),
                                                lon = c(360-78.790891)))


#Recorto los anios
datos_esperanza_periodo<- datos_esperanza_totales[which(year(datos_esperanza_totales$time) %in% 1990:2019),]
head(datos_esperanza_periodo)

datos_davis_periodo <- datos_davis_totales[which(year(datos_davis_totales$time) %in% 1990:2019),]


#Pruebo con barras. Esperanza
grafico <- ggplot(data= datos_esperanza_periodo, mapping= aes(x=time, y=icec)) +
  geom_bar(stat="identity", fill="#acd8fa") +
  labs(title= "Variación de la concentración del hielo marino Base Esperanza (Argentina)", subtitle= "Periodo 1990-2019", x= "Mes", y= "Hielo marino (%)")

#Para Davis
grafico1 <- ggplot(data= datos_davis_periodo, mapping= aes(x=time, y=icec)) +
  geom_bar(stat="identity", fill="#33a6ff") +
  labs(title= "Variación de la concentración del hielo marino Base Davis (Australia)", subtitle= "Periodo 1990-2019", x= "Mes", y= "Hielo marino (%)")


#Hago una serie con un solo anio. Esperanza
grafico <- ggplot(data= datos_esperanza_periodo[anio==2019], mapping= aes(x=time, y=icec)) +
  geom_bar(stat="identity", fill="#bfacfa") +
  labs(title= "Variación anual de la concentración del hielo marino Base Esperanza (Argentina)", subtitle= "Anio 2019", x= "Mes", y= "Hielo marino (%)")

#Solo 2019 para Davis.
datos_davis_periodo$anio <- year(datos_davis_periodo$time)
grafico <- ggplot(data= datos_davis_periodo[anio==2019], mapping= aes(x=time, y=icec)) +
  geom_bar(stat="identity", fill="#8aeebd") +
  labs(title= "Variación anual de la concentración del hielo marino Base Davis (Australia)", subtitle= "Anio 2019", x= "Mes", y= "Hielo marino (%)")



#Almaceno la info en una tabla .

tabla_esperanza <- datos_esperanza_periodo

tabla_esperanza$lat <- NULL
tabla_esperanza$lon <- NULL

"Base Esperanza, Latitud -63.8079, Longitud 303.75"

colnames(tabla_esperanza) <- c("Base Esperanza", "Latitud -63.8079, Longitud 303.75")

tabla_esperanza$Info[1] <- c("Base Esperanza")

write.csv(tabla_esperanza, file= "datos_esperanza.txt", sep="  ", row.names = F)


#Hago lo mismo para la base Davis
tabla_davis <- datos_davis_periodo
tabla_davis$time <- NULL
colnames(tabla_davis) <- c("Latitud", "Longitud", "Concentracion")

write.table(tabla_davis, file= "datos_davis.txt", sep="  ", row.names = F)
### ponerlo como titulo.




# ITEM D - Climatologia mensual para todo la region ----------------------

#Agrego una columna primero que tenga el mes.
datos_antartida_periodo$mes <- month(datos_antartida_periodo$time)

#Calculo el promedio por mes para cada punto de latitud y longitud
climatologia <- aggregate(datos_antartida_periodo$icec, list(datos_antartida_periodo$mes,datos_antartida_periodo$lat, datos_antartida_periodo$lon), mean)

colnames(climatologia) <- c("Mes", "Latitud", "Longitud", "Climatologia")

#Separo los datos de cada mes. VER DE HACER UN CICLO CON ESTO
enero <- subset.data.frame(climatologia, Mes== 1, select=c(Latitud, Longitud, Climatologia))
febrero <- subset.data.frame(climatologia, Mes== 2, select=c(Latitud, Longitud, Climatologia))
marzo <- subset.data.frame(climatologia, Mes== 3, select=c(Latitud, Longitud, Climatologia))
abril <- subset.data.frame(climatologia, Mes== 4, select=c(Latitud, Longitud, Climatologia))
mayo <- subset.data.frame(climatologia, Mes== 5, select=c(Latitud, Longitud, Climatologia))
junio <- subset.data.frame(climatologia, Mes== 6, select=c(Latitud, Longitud, Climatologia))
julio<- subset.data.frame(climatologia, Mes== 7, select=c(Latitud, Longitud, Climatologia))
agosto <- subset.data.frame(climatologia, Mes== 8, select=c(Latitud, Longitud, Climatologia))
septiembre <- subset.data.frame(climatologia, Mes== 9, select=c(Latitud, Longitud, Climatologia))
octubre <- subset.data.frame(climatologia, Mes== 10, select=c(Latitud, Longitud, Climatologia))
noviembre <- subset.data.frame(climatologia, Mes== 11, select=c(Latitud, Longitud, Climatologia))
diciembre <-subset.data.frame(climatologia, Mes== 12, select=c(Latitud, Longitud, Climatologia))


mapa <- map_data("world")
library(RColorBrewer)
proy<-"stereographic"
orientacion<-c(-90,0,0)

#Grafico enero
g <- ggplot(enero,aes(x=Longitud,y=Latitud)) +theme_bw()
g<-g+ geom_tile(aes(fill=Climatologia),alpha=0.6) #primera capa, datos de isec
g<-g+ scale_fill_gradientn(name=expression("Concentracion [%]"),colours=rev(brewer.pal(9,"RdYlBu")))
g <- g+geom_path(data = mapa, aes(long, lat, group = group),
                 linewidth = 0.4)
g<-g+ labs(title="Concentración de hielo marino Antártida y Océanos circundantes", subtitle = ("Promedio Enero 1990-2019"))
g<-g+ scale_y_continuous(limits = c(-90,0))
g <- g+coord_map(projection = proy,orientation=orientacion, ylim = c(-40, -90))

#Febrero
g <- ggplot(febrero,aes(x=Longitud,y=Latitud)) +theme_bw()
g<-g+ geom_tile(aes(fill=Climatologia),alpha=0.6) #primera capa, datos de isec
g<-g+ scale_fill_gradientn(name=expression("Concentracion [%]"),colours=rev(brewer.pal(9,"RdYlBu")))
g <- g+geom_path(data = mapa, aes(long, lat, group = group),
                 linewidth = 0.4)
g<-g+ labs(title="Concentración de hielo marino Antártida y Océanos circundantes", subtitle = ("Promedio Febrero 1990-2019"))
g<-g+ scale_y_continuous(limits = c(-90,0))
g <- g+coord_map(projection = proy,orientation=orientacion, ylim = c(-40, -90))

#Marzo
g <- ggplot(marzo,aes(x=Longitud,y=Latitud)) +theme_bw()
g<-g+ geom_tile(aes(fill=Climatologia),alpha=0.6) #primera capa, datos de isec
g<-g+ scale_fill_gradientn(name=expression("Concentracion [%]"),colours=rev(brewer.pal(9,"RdYlBu")))
g <- g+geom_path(data = mapa, aes(long, lat, group = group),
                 linewidth = 0.4)
g<-g+ labs(title="Concentración de hielo marino Antártida y Océanos circundantes", subtitle = ("Promedio Marzo 1990-2019"))
g<-g+ scale_y_continuous(limits = c(-90,0))
g <- g+coord_map(projection = proy,orientation=orientacion, ylim = c(-40, -90))

#Abril
g <- ggplot(abril,aes(x=Longitud,y=Latitud)) +theme_bw()
g<-g+ geom_tile(aes(fill=Climatologia),alpha=0.6) #primera capa, datos de isec
g<-g+ scale_fill_gradientn(name=expression("Concentracion [%]"),colours=rev(brewer.pal(9,"RdYlBu")))
g <- g+geom_path(data = mapa, aes(long, lat, group = group),
                 linewidth = 0.4)
g<-g+ labs(title="Concentración de hielo marino Antártida y Océanos circundantes", subtitle = ("Promedio Abril 1990-2019"))
g<-g+ scale_y_continuous(limits = c(-90,0))
g <- g+coord_map(projection = proy,orientation=orientacion, ylim = c(-40, -90))

#Mayo
g <- ggplot(mayo,aes(x=Longitud,y=Latitud)) +theme_bw()
g<-g+ geom_tile(aes(fill=Climatologia),alpha=0.6) #primera capa, datos de isec
g<-g+ scale_fill_gradientn(name=expression("Concentracion [%]"),colours=rev(brewer.pal(9,"RdYlBu")))
g <- g+geom_path(data = mapa, aes(long, lat, group = group),
                 linewidth = 0.4)
g<-g+ labs(title="Concentración de hielo marino Antártida y Océanos circundantes", subtitle = ("Promedio Mayo 1990-2019"))
g<-g+ scale_y_continuous(limits = c(-90,0))
g <- g+coord_map(projection = proy,orientation=orientacion, ylim = c(-40, -90))

#Junio
g <- ggplot(junio,aes(x=Longitud,y=Latitud)) +theme_bw()
g<-g+ geom_tile(aes(fill=Climatologia),alpha=0.6) #primera capa, datos de isec
g<-g+ scale_fill_gradientn(name=expression("Concentracion [%]"),colours=rev(brewer.pal(9,"RdYlBu")))
g <- g+geom_path(data = mapa, aes(long, lat, group = group),
                 linewidth = 0.4)
g<-g+ labs(title="Concentración de hielo marino Antártida y Océanos circundantes", subtitle = ("Promedio junio 1990-2019"))
g<-g+ scale_y_continuous(limits = c(-90,0))
g <- g+coord_map(projection = proy,orientation=orientacion, ylim = c(-40, -90))

#Julio
g <- ggplot(julio,aes(x=Longitud,y=Latitud)) +theme_bw()
g<-g+ geom_tile(aes(fill=Climatologia),alpha=0.6) #primera capa, datos de isec
g<-g+ scale_fill_gradientn(name=expression("Concentracion [%]"),colours=rev(brewer.pal(9,"RdYlBu")))
g <- g+geom_path(data = mapa, aes(long, lat, group = group),
                 linewidth = 0.4)
g<-g+ labs(title="Concentración de hielo marino Antártida y Océanos circundantes", subtitle = ("Promedio Julio 1990-2019"))
g<-g+ scale_y_continuous(limits = c(-90,0))
g <- g+coord_map(projection = proy,orientation=orientacion, ylim = c(-40, -90))

#Agosto
g <- ggplot(agosto,aes(x=Longitud,y=Latitud)) +theme_bw()
g<-g+ geom_tile(aes(fill=Climatologia),alpha=0.6) #primera capa, datos de isec
g<-g+ scale_fill_gradientn(name=expression("Concentracion [%]"),colours=rev(brewer.pal(9,"RdYlBu")))
g <- g+geom_path(data = mapa, aes(long, lat, group = group),
                 linewidth = 0.4)
g<-g+ labs(title="Concentración de hielo marino Antártida y Océanos circundantes", subtitle = ("Promedio agosto 1990-2019"))
g<-g+ scale_y_continuous(limits = c(-90,0))
g <- g+coord_map(projection = proy,orientation=orientacion, ylim = c(-40, -90))

#Septiembre
g <- ggplot(septiembre,aes(x=Longitud,y=Latitud)) +theme_bw()
g<-g+ geom_tile(aes(fill=Climatologia),alpha=0.6) #primera capa, datos de isec
g<-g+ scale_fill_gradientn(name=expression("Concentracion [%]"),colours=rev(brewer.pal(9,"RdYlBu")))
g <- g+geom_path(data = mapa, aes(long, lat, group = group),
                 linewidth = 0.4)
g<-g+ labs(title="Concentración de hielo marino Antártida y Océanos circundantes", subtitle = ("Promedio septiembre 1990-2019"))
g<-g+ scale_y_continuous(limits = c(-90,0))
g <- g+coord_map(projection = proy,orientation=orientacion, ylim = c(-40, -90))

#Octubre
g <- ggplot(octubre,aes(x=Longitud,y=Latitud)) +theme_bw()
g<-g+ geom_tile(aes(fill=Climatologia),alpha=0.6) #primera capa, datos de isec
g<-g+ scale_fill_gradientn(name=expression("Concentracion [%]"),colours=rev(brewer.pal(9,"RdYlBu")))
g <- g+geom_path(data = mapa, aes(long, lat, group = group),
                 linewidth = 0.4)
g<-g+ labs(title="Concentración de hielo marino Antártida y Océanos circundantes", subtitle = ("Promedio Octubre 1990-2019"))
g<-g+ scale_y_continuous(limits = c(-90,0))
g <- g+coord_map(projection = proy,orientation=orientacion, ylim = c(-40, -90))

#Septiembre
g <- ggplot(septiembre,aes(x=Longitud,y=Latitud)) +theme_bw()
g<-g+ geom_tile(aes(fill=Climatologia),alpha=0.6) #primera capa, datos de isec
g<-g+ scale_fill_gradientn(name=expression("Concentracion [%]"),colours=rev(brewer.pal(9,"RdYlBu")))
g <- g+geom_path(data = mapa, aes(long, lat, group = group),
                 linewidth = 0.4)
g<-g+ labs(title="Concentración de hielo marino Antártida y Océanos circundantes", subtitle = ("Promedio Septiembre 1990-2019"))
g<-g+ scale_y_continuous(limits = c(-90,0))
g <- g+coord_map(projection = proy,orientation=orientacion, ylim = c(-40, -90))

#Octubre
g <- ggplot(octubre,aes(x=Longitud,y=Latitud)) +theme_bw()
g<-g+ geom_tile(aes(fill=Climatologia),alpha=0.6) #primera capa, datos de isec
g<-g+ scale_fill_gradientn(name=expression("Concentracion [%]"),colours=rev(brewer.pal(9,"RdYlBu")))
g <- g+geom_path(data = mapa, aes(long, lat, group = group),
                 linewidth = 0.4)
g<-g+ labs(title="Concentración de hielo marino Antártida y Océanos circundantes", subtitle = ("Promedio Octubre 1990-2019"))
g<-g+ scale_y_continuous(limits = c(-90,0))
g <- g+coord_map(projection = proy,orientation=orientacion, ylim = c(-40, -90))

#Noviembre
g <- ggplot(noviembre,aes(x=Longitud,y=Latitud)) +theme_bw()
g<-g+ geom_tile(aes(fill=Climatologia),alpha=0.6) #primera capa, datos de isec
g<-g+ scale_fill_gradientn(name=expression("Concentracion [%]"),colours=rev(brewer.pal(9,"RdYlBu")))
g <- g+geom_path(data = mapa, aes(long, lat, group = group),
                 linewidth = 0.4)
g<-g+ labs(title="Concentración de hielo marino Antártida y Océanos circundantes", subtitle = ("Promedio Noviembre 1990-2019"))
g<-g+ scale_y_continuous(limits = c(-90,0))
g <- g+coord_map(projection = proy,orientation=orientacion, ylim = c(-40, -90))

#Diciembre
g <- ggplot(diciembre,aes(x=Longitud,y=Latitud)) +theme_bw()
g<-g+ geom_tile(aes(fill=Climatologia),alpha=0.6) #primera capa, datos de isec
g<-g+ scale_fill_gradientn(name=expression("Concentracion [%]"),colours=rev(brewer.pal(9,"RdYlBu")))
g <- g+geom_path(data = mapa, aes(long, lat, group = group),
                 linewidth = 0.4)
g<-g+ labs(title="Concentración de hielo marino Antártida y Océanos circundantes", subtitle = ("Promedio Diciembre 1990-2019"))
g<-g+ scale_y_continuous(limits = c(-90,0))
g <- g+coord_map(projection = proy,orientation=orientacion, ylim = c(-40, -90))


# ITEM E - CORRELACION CON INDICE SOI -------------------------------------

datos_soi <- read.table(file= "soi.txt", skip= 43, nrows = 30) #Abro el archivo y recorto los datos del periodo que necesito: 1990-2019.
colnames(datos_soi) <- c("Anio","Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

#Defino una funcion que haga el promedio por estaciones y por anio

promedio_estacional_soi <- function(estacion) {
  promedios <- data.frame()
  if (estacion == "MAM") {
    a <- subset.data.frame(datos_soi, select=c(Anio, Mar, Abr, May))
  } else if (estacion == "JJA") {
    a <- subset.data.frame(datos_soi, select=c(Anio, Jun, Jul, Ago))
  } else if (estacion == "SON") {
    a <- subset.data.frame(datos_soi, select=c(Anio, Sep, Oct, Nov))
  } else if (estacion == "DEF") { #VER ESTO, OJO QUE TOMA LOS DEL MISMO AÑO!!!
    a <- subset.data.frame(datos_soi, select=c(Anio, Dic, Ene, Feb))
    }
  for (i in 1:nrow(a)) {
    valores <- c(a[i,2], a[i,3], a[i,4])
    promedios[i,1]<- a$Anio[i]
    promedios[i,2] <- mean(valores)
    
  }
  return(promedios)
}

#Uso la funcion para cada estacion
MAM_soi <- promedio_estacional_soi("MAM")
colnames(MAM_soi) <- c("Anio","PromEstacion")
JJA_soi <- promedio_estacional_soi("JJA")
colnames(JJA_soi) <- c("Anio","PromEstacion")
SON_soi <- promedio_estacional_soi("SON")
colnames(SON_soi) <- c("Anio","PromEstacion")
DEF_soi <- promedio_estacional_soi("DEF")
colnames(DEF_soi) <- c("Anio","PromEstacion")



#Hago promedio estacional para la Base Esperanza
datos_esperanza_periodo$mes <- month(datos_esperanza_periodo$time)
datos_esperanza_periodo$anio <- year(datos_esperanza_periodo$time)

#MAM
mam <- sort(c(which(datos_esperanza_periodo$mes==3), which(datos_esperanza_periodo$mes==4),which(datos_esperanza_periodo$mes==5))) #posiciones de los meses 3,4 y 5
esperanza_mam <- aggregate(datos_esperanza_periodo$icec[mam], list(datos_esperanza_periodo$anio[mam]), mean)

#JJA (misma idea que antes)
jja <- sort(c(which(datos_esperanza_periodo$mes==6), which(datos_esperanza_periodo$mes==7),which(datos_esperanza_periodo$mes==8))) 
esperanza_jja <- aggregate(datos_esperanza_periodo$icec[jja], list(datos_esperanza_periodo$anio[jja]), mean)

#SON
son <-sort(c(which(datos_esperanza_periodo$mes==9), which(datos_esperanza_periodo$mes==10),which(datos_esperanza_periodo$mes==11))) 
esperanza_son <- aggregate(datos_esperanza_periodo$icec[son], list(datos_esperanza_periodo$anio[son]), mean)

#DEF
esperanza_def 


#Hago las correlaciones
cor_verano_esp <- 
cor_otonio_esp <- cor(MAM_soi[,2], esperanza_mam[,2])
cor_invierno_esp <- cor(JJA_soi[,2], esperanza_jja[,2])
cor_primavera_esp <-cor(SON_soi[,2], esperanza_son[,2])
  
#Armo un dataframe con esa info y lo guardo en formato txt.
season <- c("DEF", "MAM", "JJA","SON")
cor_esp <- c(cor_verano_esp, cor_otonio_esp, cor_invierno_esp, cor_primavera_esp)
correlacion_esperanza <- data.frame("Season"= season, "Coeficiente de correlacion SOI/Concentracion hielo marino Esperanza" = cor)
write.table(correlacion_esperanza, file= "correlacion_soi_hielo.txt", sep="  ", row.names = F)



#Idem a lo anterior pero con la base Davis



