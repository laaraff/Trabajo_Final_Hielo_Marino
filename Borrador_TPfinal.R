#Comienzo borrador Trabajo final

require(ncdf4)
nc<-nc_open ("/home/clinux01/Escritorio/Lara/Trabajo_Final/icec.sfc.mon.mean.nc")
nc

library(udunits2)
library(metR)
library(lubridate)

archivo <- "/home/clinux01/Escritorio/Lara/Trabajo_Final/icec.sfc.mon.mean.nc"
GlanceNetCDF(archivo)
datos <- ReadNetCDF(archivo, vars = "icec")
hist(datos$icec)
dim(datos) #16405632  4 Es un dataframe

#Recorto la region

datos_esperanza_totales<- ReadNetCDF(archivo, vars = "icec",
                          subset = list(lat = c(-63.396958),
                                        lon = c(360-56.998053)))
datos_belgrano_totales <- ReadNetCDF(archivo, vars = "icec",
                             subset = list(lat = c(-77.874443),
                                           lon = c(360-34.626943)))

#Recorto los anios
datos_esperanza_periodo<- datos_esperanza_totales[which(year(datos_esperanza_totales$time) %in% 1990:2019),]
head(datos_esperanza_periodo)

datos_belgrano_periodo <- datos_belgrano_totales[which(year(datos_belgrano_totales$time) %in% 1990:2019),]
head(datos_belgrano_periodo)


#Veo que onda la base Davis
datos_davis_totales <- ReadNetCDF(archivo, vars = "icec",
                                 subset = list(lat = c(-68.469637),
                                               lon = c(360-78.790891)))

datos_davis_periodo <- datos_davis_totales[which(year(datos_davis_totales$time) %in% 1990:2019),]


#c)
#Para Esperanza

#Serie temporal. DESCARTADO, SE VE HORRIBLE
#grafico <- ggplot(data= datos_esperanza_periodo, mapping= aes(x=time, y=icec)) +
#  geom_line(color="black") +
#  labs(title= "Evolución del hielo marino Base Esperanza", subtitle= "Periodo 1990-2019", x= "Mes", y= "Hielo marino (%)")

#Pruebo con barras. Esperanza
grafico <- ggplot(data= datos_esperanza_periodo, mapping= aes(x=time, y=icec)) +
  geom_bar(stat="identity", color="#acd8fa") +
  labs(title= "Evolución del hielo marino Base Esperanza", subtitle= "Periodo 1990-2019", x= "Mes", y= "Hielo marino (%)")

#Para Davis
grafico1 <- ggplot(data= datos_davis_periodo, mapping= aes(x=time, y=icec)) +
  geom_bar(stat="identity", color="#33a6ff") +
  labs(title= "Evolución del hielo marino Base Davis", subtitle= "Periodo 1990-2019", x= "Mes", y= "Hielo marino (%)")





#Almaceno la info en una tabla (del item c) FALTA GUARDARLA EN FORMATO TXT





