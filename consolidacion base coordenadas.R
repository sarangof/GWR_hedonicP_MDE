setwd("C:/Users/sarangof/Documents/Datos_Galeria_Inmobiliaria/R")

library("Hmisc")


# CARGAR BASES DE DATOS
nueva <- read.csv2("Nuevo Consolidado_abril.csv",sep=";") #archivo depurado con vivienda nueva
usada <- read.csv2("Usada Consolidada.csv",sep=";") #archivo depurado con vivienda usada
grf <- read.csv("Direcciones georeferenciadas.csv") #archivo con ubicación geográfica
year_nv <- read.csv2("year_nv.csv",sep=";") #archivo con fechas de entrega de cada proyecto


#LIMPIEZA Y ESTANDARIZACIÓN DE VARIABLES RELEVANTES
year_nv$Codproyecto <- as.numeric(year_nv$Codproyecto)
usada$Area.Terraza[usada$Area.Terraza== "No"] <- NA
usada$Area.Terraza[usada$Area.Terraza== "no"] <- NA
usada$Area.Terraza[usada$Area.Terraza== "Si"] <- NA
usada$Area.Terraza <- as.numeric(usada$Area.Terraza)
usada$Estrato <- as.factor(usada$Estrato)
nueva$Estrato <- as.factor(nueva$Estrato)
for (i in (1:nrow(nueva))){ #cambiar el formato de la variable Año
  words <- strsplit(as.character(year_nv$Fecha.Entrega[(year_nv$Codproyecto==nueva$Codproyecto[i])==TRUE]),split="-")
  nueva$Year[i] <- matrix(unlist(words), ncol=2, byrow=TRUE)[2]
}
centroX <-  -75.5906 # Coordenadas centro de Medellin a la altura de La Alpujarra
centroY <-  6.2308


# BASE DE DATOS COMPLETA VIVIENDA NUEVA PARA MDE CON COORDENADAS
coordn <- nueva
coordn[,length(names(nueva))+1] <- 1 #agregar nuevas columnas a la bdd
coordn[,length(names(nueva))+2] <- 1
coordn[,length(names(nueva))+3] <- 1
coordn[,length(names(nueva))+4] <- 1
coordn[,length(names(nueva))+5] <- 99
names(coordn)[(length(names(nueva))+1):(length(names(nueva))+5)] <- paste(cbind("coordX", "coordY", "Proyeccion", "DistCentro", "Piso_nv")) #nombrar las nuevas columnas
for (i in 1:nrow(nueva)){ #agregar coordenadas
  pos <- grf$Codproyecto==nueva$Codproyecto[i]
  coordn[i,length(names(nueva))+1] <- grf$X[pos] 
  coordn[i,length(names(nueva))+2] <- grf$Y[pos]
  coordn[i,length(names(nueva))+3] <- grf$DESCRIPTION[pos]
}


# BASE DE DATOS COMPLETA VIVIENDA USADA PARA MDE CON COORDENADAS
coordu <- usada
coordu[,length(names(usada))+1] <- 1
coordu[,length(names(usada))+2] <- 1
coordu[,length(names(usada))+3] <- 1
coordu[,length(names(usada))+4] <- 1
coordu[,length(names(usada))+5] <- 1
coordu[,length(names(usada))+6] <- 1
names(coordu)[17]<-paste("Year")
names(coordu)[(length(names(usada))+1):(length(names(usada))+5)] <- paste(cbind("coordX", "coordY", "Proyeccion", "DistCentro", "Piso_us"))
for (i in 1:nrow(usada)){ #agregar coordenadas
  pos <- grf$Codproyecto==usada$Codproyecto[i]
  coordu[i,length(names(usada))+1] <- grf$X[pos][1]
  coordu[i,length(names(usada))+2] <- grf$Y[pos][1]
  coordu[i,length(names(usada))+3] <- grf$DESCRIPTION[pos][1]
}


# CREAR BDD DEFINITIVA 
base_us <- coordu[which(coordu$Proyeccion < 4),] #filtrar mal posicionados
base_us$DistCentro <- (((base_us$coordX - centroX)^2 + (base_us$coordY - centroY)^2  )^0.5) #vector de distancias al centro
base_nv <- coordn[which(coordn$Proyeccion < 4), ] #filtrar mal posicionados
base_nv$DistCentro <- (((base_nv$coordX - centroX)^2 + (base_nv$coordY - centroY)^2  )^0.5) #vector de distancias al centro
#crear variable piso
for (j in 1:nrow(base_nv)){ 
  if (all.is.numeric(base_nv$Interior[j]) == TRUE ){
     if (nchar(toString(base_nv$Interior[j]))<=2){
       base_nv$Piso_nv[j] <- toString(base_nv$Interior[j])  
     }
     else if (nchar(toString(base_nv$Interior[j]))==3) {
       base_nv$Piso_nv[j] <- substring(base_nv$Interior[j],1,1)
     }
     else if (nchar(toString(base_nv$Interior[j]))==4) {
       base_nv$Piso_nv[j] <- substring(base_nv$Interior[j],1,2) 
     }
  }
}
for (j in 1:nrow(base_nv)){
  if (base_nv$Tipo[j] == "Casa"){
    base_nv$Piso_nv[j] <- 1
  }
}
base_nv$Piso_nv[base_nv$Piso_nv== 99] <- NA
base_nv$Piso_nv <- as.numeric(base_nv$Piso_nv)
base_nv$Piso_nv[base_nv$Piso_nv > 25] <- NA
#editar variable Baños
for (j in 1:nrow(base_nv)){
  if (all.is.numeric(base_nv$Baños[j]) == FALSE ){
    base_nv$Baños[j] <- NA
  }
}
#adecuar variable Garajes
for (j in 1:nrow(base_nv)){
  if (all.is.numeric(base_nv$NGar[j]) == FALSE ){
    base_nv$NGar[j] <- NA
  }
}

#IMPRIMIR NUEVAS BDD
write.csv2(file="usada_coord.csv", x=base_us)
write.csv2(file="nueva_coord.csv", x=base_nv)