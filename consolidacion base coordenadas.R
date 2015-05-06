#setwd("C:/Users/sarangof/Documents/Datos Galeria Inmobiliaria/R")
setwd("C:/Users/sarangof/Documents/Datos Galería Inmobiliaria/R")

library("Hmisc")

# Cargar bases de datos

nueva <- read.csv2("Nuevo Consolidado_abril.csv",sep=";") #Nuevo Consolidado 2
usada <- read.csv2("Usada Consolidada.csv",sep=";") #Usada Consolidada
grf <- read.csv("Direcciones georeferenciadas.csv")
year_nv <- read.csv2("year_nv.csv",sep=";")
year_nv$Codproyecto <- as.numeric(year_nv$Codproyecto)

setwd("C:/Users/sarangof/Documents/Datos Galería Inmobiliaria")
total <- read.csv2("nuevos y usados.csv",sep=";")
setwd("C:/Users/sarangof/Documents/Datos Galería Inmobiliaria/R")

#Limpieza y estandarización de variables relevantes

usada$Area.Terraza <- as.numeric(usada$Area.Terraza)
usada$Area.Terraza[usada$Area.Terraza== 38] <- 0 #Si el valor es "no", asignare 0 mts2
usada$Area.Terraza[usada$Area.Terraza== "Si"] <-NA 

# Coordenadas centro de Medellin
centroX <-  -75.5906
centroY <-  6.2308

# Agregar variable "piso" a base de nuevos


# Cambiar formato de variable "Año"

for (i in (1:nrow(nueva))){
  words <- strsplit(as.character(year_nv$Fecha.Entrega[(year_nv$Codproyecto==nueva$Codproyecto[i])==TRUE]),split="-")
  nueva$Year[i] <- matrix(unlist(words), ncol=2, byrow=TRUE)[2]
}



# Base de datos completa vivienda nueva para Medellin, con coordenadas.

coordn <- nueva
coordn[,length(names(nueva))+1] <- 1
coordn[,length(names(nueva))+2] <- 1
coordn[,length(names(nueva))+3] <- 1
coordn[,length(names(nueva))+4] <- 1
coordn[,length(names(nueva))+5] <- 99

names(coordn)[length(names(nueva))+1]<-paste("coordX") #probar forma generalizada para llenar estos campos
names(coordn)[length(names(nueva))+2]<-paste("coordY")
names(coordn)[length(names(nueva))+3]<-paste("Proyeccion")
names(coordn)[length(names(nueva))+4]<-paste("DistCentro")
names(coordn)[length(names(nueva))+5 ]<-paste("Piso_nv")


for (i in 1:nrow(nueva)){
  pos <- grf$Codproyecto==nueva$Codproyecto[i]
  coordn[i,length(names(nueva))+1] <- grf$X[pos] 
  coordn[i,length(names(nueva))+2] <- grf$Y[pos]
  coordn[i,length(names(nueva))+3] <- grf$DESCRIPTION[pos]
}



# Base de datos completa vivienda usada para Medellin, con coordenadas.

coordu <- usada
coordu[,length(names(usada))+1] <- 1
coordu[,length(names(usada))+2] <- 1
coordu[,length(names(usada))+3] <- 1
coordu[,length(names(usada))+4] <- 1
coordu[,length(names(usada))+5] <- 1
coordu[,length(names(usada))+6] <- 1
names(coordu)[length(names(usada))+1]<-paste("coordX")
names(coordu)[length(names(usada))+2]<-paste("coordY")
names(coordu)[length(names(usada))+3]<-paste("Proyeccion")
names(coordu)[length(names(usada))+4]<-paste("DistCentro")
names(coordu)[length(names(usada))+5]<-paste("Piso_us")
names(coordu)[17]<-paste("Year")

for (i in 1:nrow(usada)){
  pos <- grf$Codproyecto==usada$Codproyecto[i]
  coordu[i,length(names(usada))+1] <- grf$X[pos][1]
  coordu[i,length(names(usada))+2] <- grf$Y[pos][1]
  coordu[i,length(names(usada))+3] <- grf$DESCRIPTION[pos][1]
}

# Actualizaciones: filtrar mal posicionados, crear vector de distancias, crear variable "piso"

base_us <- coordu[which(coordu$Proyeccion < 4),]
base_us$Estrato <- as.factor(base_us$Estrato)
base_us$DistCentro <- (((base_us$coordX - centroX)^2 + (base_us$coordY - centroY)^2  )^0.5)
base_nv <- coordn[which(coordn$Proyeccion < 4), ] # != "No_localizada" & coordn$Proyeccion != "No_estandarizada"),]
base_nv$DistCentro <- (((base_nv$coordX - centroX)^2 + (base_nv$coordY - centroY)^2  )^0.5)
base_nv$Estrato <- as.factor(base_nv$Estrato)

#base_nv$Interior[
  





# 
# if (grepl("+E-", base_nv$Interior[j]) == TRUE & base_nv$Tipo[j] != "Casa") {
#   if unlist(strsplit(toString(base_nv$Interior[j]),"E-"))[2]
#   
#   base_nv$Piso_nv[j] <- 
# }

# else if (nchar(toString(base_nv$Interior[j]))<=2) {
#   print(base_nv$Interior[j])
#   base_nv$Piso_nv[j] <- toString(base_nv$Interior[j])  
# } else {
#   base_nv$Piso_nv[j] <- 0
# }


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
#   else if (if (grepl("+E-", base_nv$Interior[j]) == TRUE & base_nv$Tipo[j] != "Casa") {
#     #   if unlist(strsplit(toString(base_nv$Interior[j]),"E-"))[2]){
#     
#   }
# }

for (j in 1:nrow(base_nv)){
  if (base_nv$Tipo[j] == "Casa"){
    base_nv$Piso_nv[j] <- 1
  }
}

base_nv$Piso_nv[base_nv$Piso_nv== 99] <- NA
base_nv$Piso_nv <- as.numeric(base_nv$Piso_nv)
base_nv$Piso_nv[base_nv$Piso_nv > 25] <- NA

#base_nv$Piso_nv
  
for (j in 1:nrow(base_nv)){
  if (all.is.numeric(base_nv$Baños[j]) == FALSE ){
    base_nv$Baños[j] <- NA
  }
}

#base_nv$Baños <- as.numeric(base_nv$Baños)

for (j in 1:nrow(base_nv)){
  if (all.is.numeric(base_nv$NGar[j]) == FALSE ){
    base_nv$NGar[j] <- NA
  }
}


write.csv2(file="usada_coord.csv", x=base_us)
write.csv2(file="nueva_coord.csv", x=base_nv)

