#Comienzo borrador Trabajo final


# ITEM A - Leer el archivo y extraer info ---------------------------------

require(ncdf4)
library(udunits2)
library(metR)
library(lubridate)
library(ggplot2)

nc<-nc_open ("/home/clinux01/Escritorio/Lara/Trabajo_Final_Hielo_Marino/icec.sfc.mon.mean.nc") #LINUX
#nc <- nc_open("C:/Users/Usuario/Documents/Lara/Trabajo_Final_Hielo_Marino/icec.sfc.mon.mean.nc") #WINDOWS
nc

# WINDOWS archivo <- "C:/Users/Usuario/Documents/Lara/Trabajo_Final_Hielo_Marino/icec.sfc.mon.mean.nc"
archivo <- "/home/clinux01/Escritorio/Lara/Trabajo_Final_Hielo_Marino/icec.sfc.mon.mean.nc"
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


#c)
#Para Esperanza


#Pruebo con barras. Esperanza
grafico <- ggplot(data= datos_esperanza_periodo, mapping= aes(x=time, y=icec)) +
  geom_bar(stat="identity", color="#acd8fa") +
  labs(title= "Variación de la concentración del hielo marino Base Esperanza (Argentina)", subtitle= "Periodo 1990-2019", x= "Mes", y= "Hielo marino (%)")

#Para Davis
grafico1 <- ggplot(data= datos_davis_periodo, mapping= aes(x=time, y=icec)) +
  geom_bar(stat="identity", color="#33a6ff") +
  labs(title= "Variación de la concentración del hielo marino Base Davis (Australia)", subtitle= "Periodo 1990-2019", x= "Mes", y= "Hielo marino (%)")


#Almaceno la info en una tabla .

tabla_esperanza <- datos_esperanza_periodo

tabla_esperanza$lat <- NULL
tabla_esperanza$lon <- NULL

"Base Esperanza, Latitud -63.8079, Longitud 303.75"

colnames(tabla_esperanza) <- c("Tiempo", "Concentracion")

header <- c("Base Esperanza", "Latitud -63.8079, Longitud 303.75")
names(tabla_esperanza) <- header

write.csv(tabla_esperanza, file= "datos_esperanza.txt", sep="  ", row.names = F)


#Hago lo mismo para la base Davis
tabla_davis <- datos_davis_periodo
tabla_davis$time <- NULL
colnames(tabla_davis) <- c("Latitud", "Longitud", "Concentracion")

write.table(tabla_davis, file= "datos_davis.txt", sep="  ", row.names = F)
### ponerlo como titulo.
#agregar fecha



# ITEM D - Climatologia mensual para todo la region ----------------------

#Agrego una columna primero que tenga el mes.
datos_antartida_periodo$mes <- month(datos_antartida_periodo$time)

#Calculo el promedio por mes para cada punto de latitud y longitud
climatologia <- aggregate(datos_antartida_periodo$icec, list(datos_antartida_periodo$mes,datos_antartida_periodo$lat, datos_antartida_periodo$lon), mean)

colnames(climatologia) <- c("Mes", "Latitud", "Longitud", "Climatologia")

#Separo los datos de cada mes
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



# Ejemplo de datos
x <- c(1, 2, 3, 4, 5)
y <- c(2, 4, 5, 4, 5)

# Calcular la correlación lineal
correlation <- cor(x, y)

# Imprimir el resultado
print(correlation)





