#Cachos de código que voy descartando

#De la Base Belgrano
datos_belgrano_totales <- ReadNetCDF(archivo, vars = "icec",
                                     subset = list(lat = c(-77.874443),
                                                   lon = c(360-34.626943)))
datos_belgrano_periodo <- datos_belgrano_totales[which(year(datos_belgrano_totales$time) %in% 1990:2019),]
head(datos_belgrano_periodo)

#Serie temporal. DESCARTADO, SE VE HORRIBLE
#grafico <- ggplot(data= datos_esperanza_periodo, mapping= aes(x=time, y=icec)) +
#  geom_line(color="black") +
#  labs(title= "Evolución del hielo marino Base Esperanza", subtitle= "Periodo 1990-2019", x= "Mes", y= "Hielo marino (%)")


nc <- nc_open("C:/Users/Usuario/Documents/Lara/Trabajo_Final_Hielo_Marino/solo_antartica.nc") #WINDOWS
nc

for (i in nrow(datos_antartida_periodo)) {
  if (datos_antartida_periodo$icec[i] == -9.96920996838687e+36) {
    datos_antartida_periodo$icec <- NA
  }
}