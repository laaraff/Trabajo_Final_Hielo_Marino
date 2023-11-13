#Comienzo borrador Trabajo final


# ITEM A - Leer el archivo y extraer info ---------------------------------

require(ncdf4)
library(udunits2)
library(metR)
library(lubridate)
library(ggplot2)

#nc<-nc_open ("/home/clinux01/Escritorio/Lara/Trabajo_Final/icec.sfc.mon.mean.nc") #LINUX
nc <- nc_open("C:/Users/Usuario/Documents/Lara/Trabajo_Final_Hielo_Marino/icec.sfc.mon.mean.nc") #WINDOWS
nc

archivo <- "C:/Users/Usuario/Documents/Lara/Trabajo_Final_Hielo_Marino/icec.sfc.mon.mean.nc"
GlanceNetCDF(archivo)
datos <- ReadNetCDF(archivo, vars = "icec")
hist(datos$icec)
dim(datos) #16405632  4 Es un dataframe



# ITEM B - Selecciono Antártida período 1990-2019 ---------------

antartida <- ReadNetCDF(archivo, vars = "icec",
                        subset = list(lat = c(-60,-88), #va hasta -88
                                      lon = c(0,180))) #REVISAR ESTE RECORTE!!!!

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


#Almaceno la info en una tabla .txt NO PONGO LOS TIEMPOS?????

tabla_esperanza <- datos_esperanza_periodo
tabla_esperanza$time <- NULL
colnames(tabla_esperanza) <- c("Latitud", "Longitud", "Concentracion")

write.table(tabla_esperanza, file= "datos_esperanza.txt", sep="  ", row.names = F)

#Hago lo mismo para la base Davis
tabla_davis <- datos_davis_periodo
tabla_davis$time <- NULL
colnames(tabla_davis) <- c("Latitud", "Longitud", "Concentracion")

write.table(tabla_davis, file= "datos_davis.txt", sep="  ", row.names = F)


# ITEM D - Climatologia mensual para todo la region ----------------------

#primero reemplazo la columna time solo por el mes
datos_antartida_periodo$mes <- month(datos_antartida_periodo$time)

climatologia <- aggregate(datos_antartida_periodo$icec, list(datos_antartida_periodo$mes,datos_antartida_periodo$lat, datos_antartida_periodo$lon), mean)

colnames(climatologia) <- c("Mes", "Latitud", "Longitud", "Climatologia mensual")

#si lo quiero ordenar tengo que tener cuidado que no ordena las medias tmb



