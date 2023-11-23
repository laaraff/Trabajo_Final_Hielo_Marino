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
datos_esperanza_periodo$anio <- year(datos_esperanza_periodo$time)
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

tabla_esperanza$lat <- NULL  #PONER ESTO EN UN SOLO RENGLON COMO ME DIJO ROCIO MAS ABAJO
tabla_esperanza$lon <- NULL
tabla_esperanza$anio <- NULL
tabla_esperanza$mes <- NULL

#"Base Esperanza, Latitud -63.8079, Longitud 303.75"
write("Base Esperanza, Antaŕtida Argentina. 63°23′54″S 56°59′46″O

      ---------
      ",file="datos_esperanza.txt")

colnames(tabla_esperanza) <- c("Fecha","Concentracion hielo marino (%)")
write.table(tabla_esperanza, file= "datos_esperanza.txt", sep="  ", row.names = F,append = T)


#Hago lo mismo para la base Davis
tabla_davis <- datos_davis_periodo
tabla_davis$lat <- NULL
tabla_davis$lon <- NULL
tabla_davis$anio <- NULL
tabla_davis$mes <- NULL

write("Base Davis (Australia), 68°28′09″S 78°52′11″E
      
      ---------
      ",file="datos_davis.txt")

colnames(tabla_davis) <- c("Fecha","Concentracion hielo marino (%)")
write.table(tabla_davis, file= "datos_davis.txt", sep="  ", row.names = F,append = T)



# ITEM D - Climatologia mensual para todo la region ----------------------

#Agrego una columna primero que tenga el mes.
datos_antartida_periodo$mes <- month(datos_antartida_periodo$time)

#Calculo el promedio por mes para cada punto de latitud y longitud
climatologia <- aggregate(datos_antartida_periodo$icec, list(datos_antartida_periodo$mes,datos_antartida_periodo$lat, datos_antartida_periodo$lon), mean)

colnames(climatologia) <- c("Mes", "Latitud", "Longitud", "Climatologia")

#Grafico un panel con los 12 mapas
mapa <- map_data("world")
library(RColorBrewer)
proy<-"stereographic"
orientacion<-c(-90,0,0)

graficos <- ggplot(climatologia,aes(x=Longitud,y=Latitud)) +theme_bw() +
geom_tile(aes(fill=Climatologia),alpha=0.6) +
  scale_fill_gradientn(name=expression("Concentracion [%]"),colours=rev(brewer.pal(9,"RdYlBu"))) +
  geom_path(data = mapa, aes(long, lat, group = group),
            linewidth = 0.4) +
  scale_y_continuous(limits = c(-90,0)) +
  coord_map(projection = proy,orientation=orientacion, ylim = c(-40, -90)) +
  facet_wrap(.~Mes, ncol=6) +
  labs(x="Longitud", y= "Latitud", fill= "Climatologia Mensual")+theme(axis.text.x=element_text(size=5)) +
  labs(title="Concentración de hielo marino Antártida y Océanos circundantes", subtitle = ("Promedio Mensual 1990-2019"))

#Separo los datos de dos meses para hacer graficos aparte y poder compararlos y verlos mejor en la presentacion
febrero <- subset.data.frame(climatologia, Mes== 2, select=c(Latitud, Longitud, Climatologia))
septiembre <- subset.data.frame(climatologia, Mes== 9, select=c(Latitud, Longitud, Climatologia))


#Febrero
g <- ggplot(febrero,aes(x=Longitud,y=Latitud)) +theme_bw()
g<-g+ geom_tile(aes(fill=Climatologia),alpha=0.6) #primera capa, datos de isec
g<-g+ scale_fill_gradientn(name=expression("Concentracion [%]"),colours=rev(brewer.pal(9,"RdYlBu")))
g <- g+geom_path(data = mapa, aes(long, lat, group = group),
                 linewidth = 0.4)
g<-g+ labs(title="Concentración de hielo marino Antártida y Océanos circundantes", subtitle = ("Promedio Febrero 1990-2019"))
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
  } 
  for (i in 1:nrow(a)) {
    valores <- c(a[i,2], a[i,3], a[i,4])
    promedios[i,1]<- a$Anio[i]
    promedios[i,2] <- mean(valores)
    
  }
  return(promedios)
}

promedio_soi_diciembre <- function(estacion) { #voy a usar esa funcion solo para diciembre, por lo que espero que lo unico ingresado sea "DEF"
  promedios <- data.frame()
  if (estacion=="DEF") {
    a <- subset.data.frame(datos_soi, select=c(Anio, Ene, Feb))
    b <-subset.data.frame(datos_soi, select=c(Anio, Dic))
    for (i in 2:nrow(a)) { #voy a empezar a contar los veranos a partir de 1991
      valores <- c(a[i,2], a[i,3], b[i-1,2])
      promedios[i,1]<- a$Anio[i]
      promedios[i,2] <- mean(valores)
  }
  return(promedios)
  }
}


#Uso la funcion para cada estacion
MAM_soi <- promedio_estacional_soi("MAM")
colnames(MAM_soi) <- c("Anio","PromEstacion")
JJA_soi <- promedio_estacional_soi("JJA")
colnames(JJA_soi) <- c("Anio","PromEstacion")
SON_soi <- promedio_estacional_soi("SON")
colnames(SON_soi) <- c("Anio","PromEstacion")
DEF_soi <- promedio_soi_diciembre("DEF")
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
#Antes recorto los meses de 1990 que no voy a usar, ya que el primer verano que tengo es 1991
datos_esperanza_periodo1<-datos_esperanza_periodo[-c(1:11),]

#Ahora voy a hacer una trampita y asignar cada diciembre al año siguiente para poder hacer mas facil los promedios del verano
datos_esperanza_periodo2 <- datos_esperanza_periodo1 #hago otro dataframe para no 
diciembres <- which(datos_esperanza_periodo2$mes==12)
datos_esperanza_periodo2$anio[diciembres]<-datos_esperanza_periodo2$anio[diciembres]+1

def <-sort(c(diciembres, which(datos_esperanza_periodo2$mes==1),which(datos_esperanza_periodo2$mes==2))) 
esperanza_def <- aggregate(datos_esperanza_periodo2$icec[def], list(datos_esperanza_periodo2$anio[def]), mean)


#Hago las correlaciones
cor_verano_esp <-       #VOY A TENER QUE AGREGAR UN NA.RM=T
cor_otonio_esp <- cor(MAM_soi[,2], esperanza_mam[,2])
cor_invierno_esp <- cor(JJA_soi[,2], esperanza_jja[,2])
cor_primavera_esp <-cor(SON_soi[,2], esperanza_son[,2])
  
#Armo un dataframe con esa info y lo guardo en formato txt.
season <- c("DEF", "MAM", "JJA","SON")
cor_esp <- c(cor_verano_esp, cor_otonio_esp, cor_invierno_esp, cor_primavera_esp)
correlacion_esperanza <- data.frame("Season"= season, "Coeficiente de correlacion SOI/Concentracion hielo marino Esperanza" = cor_esp)
write.table(correlacion_esperanza, file= "correlacion_soi_hielo_esperanza.txt", sep="  ", row.names = F)



#Idem a lo anterior pero con la base Davis
#MAM
mam1 <- sort(c(which(datos_davis_periodo$mes==3), which(datos_davis_periodo$mes==4),which(datos_davis_periodo$mes==5))) #posiciones de los meses 3,4 y 5 OJ
davis_mam <- aggregate(datos_davis_periodo$icec[mam1], list(datos_davis_periodo$anio[mam1]), mean)

#JJA



#SON


#DEF

#Hago las correlaciones, guardo la info en un dataframe y lo guardo en un archivo .txt




#------------------------------------------------------
#Para marcar las estaciones en un mapa (en proceso)
#Hago un mapa para mostrar donde estan las bases con las que estoy trabajando. 
coordenadas <- data.frame(
  latitud = c(-63.8079, -68.46),
  longitud = c(-56.99, 78.86)
)

south_america_map <- map_data("world", continent = "South America")
antarctica_map <- map_data("world", region = "Antarctica")
color= c("blue", "green")


ggplot() +
  geom_polygon(data = south_america_map, aes(x = long, y = lat, group = group), fill = "lightblue", color = "black") +
  geom_polygon(data = antarctica_map, aes(x = long, y = lat, group = group), fill = "lightblue", color = "black") +
  geom_point(data = coordenadas, aes(x = longitud, y = latitud, color = color), size = 3) +
  coord_cartesian(xlim = c(-100, 100), ylim = c(-90, -40)) +  # Definir límites de visualización
  theme_minimal() +
  labs(title = "Ubicación de las bases antárticas trabajadas") +
  scale_color_identity()  #ver esto!!! 
#usar el geom_text para escribir las etiquetas de las bases


