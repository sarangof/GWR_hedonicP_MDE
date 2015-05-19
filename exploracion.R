#Create variable summary - means, median, maximum, minimum
#NA table

setwd("C:/Users/sarangof/Documents/Datos_Galeria_Inmobiliaria/R")
base_us <- read.csv2("base_us.csv",sep=";")
base_nv <- read.csv2("base_nv.csv",sep=";")

library(Rcmdr)
library(lmtest)
library(ggplot2)

#DESCRIPTIVO USADOS

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

barrio_mode_us <- Mode(base_us$Barrio)
barrio_tbl_us <- table(base_us$Barrio)

#Estrato
jpeg(file = "C:/Users/sarangof/Documents/Datos_Galeria_Inmobiliaria/R/Graficos exploratorios/Estrato_us.jpg")
barplot(table(base_us$Estrato),xlab="Estrato",ylab="Frecuencia",col=terrain.colors(6),main="Ocurrencias por Estrato vivienda usada")
dev.off()

#A�o
Antig <- cut(base_us$Year,c(1950,1960,1970,1980,1990,2000,2010)) 

jpeg(file = "C:/Users/sarangof/Documents/Datos_Galeria_Inmobiliaria/R/Graficos exploratorios/antig_us.jpg")
barplot(table(Antig),xlab="Estrato",ylab="Frecuencia",col=terrain.colors(6),main="Antig�edad vivienda usada",xaxt="n")
axis(1, at=1:7,labels=c(1950,1960,1970,1980,1990,2000,2010))
dev.off()

#Tipo

jpeg(file = "C:/Users/sarangof/Documents/Datos_Galeria_Inmobiliaria/R/Graficos exploratorios/tipo_us.jpg")
barplot(table(base_us$Tipo),xlab="Tipo",ylab="Frecuencia",col=topo.colors(5),main="Tipo vivienda usada")
dev.off()

#Piso

jpeg(file = "C:/Users/sarangof/Documents/Datos_Galeria_Inmobiliaria/R/Graficos exploratorios/piso_us.jpg")
plot(density(as.numeric(base_us$Piso[base_us$Tipo=="Apto."]),na.rm=TRUE),main="Distribuci�n variable piso (aptos)",cex=2)
dev.off()

#Precio

jpeg(file = "C:/Users/sarangof/Documents/Datos_Galeria_Inmobiliaria/R/Graficos exploratorios/precio_us.jpg")
plot(density(as.numeric(base_us$Precio),na.rm=TRUE),main="Distribuci�n variable Precio",cex=2)
dev.off()
#HACER BIEN


#Area



#Alcobas

jpeg(file = "C:/Users/sarangof/Documents/Datos_Galeria_Inmobiliaria/R/Graficos exploratorios/alcobas_us.jpg")
barplot(table(base_us$Alcobas),xlab="N�mero de alcobas",ylab="Frecuencia",main="Alcobas en vivienda usada",col=cm.colors(7))
dev.off()

#Ba�os

jpeg(file = "C:/Users/sarangof/Documents/Datos_Galeria_Inmobiliaria/R/Graficos exploratorios/ba�os_us.jpg")
barplot(table(base_us$Ba�os),xlab="N�mero de Ba�os",ylab="Frecuencia",main="Ba�os en vivienda usada",col=cm.colors(7))
dev.off()

#Garajes

jpeg(file = "C:/Users/sarangof/Documents/Datos_Galeria_Inmobiliaria/R/Graficos exploratorios/NGar_us.jpg")
barplot(table(base_us$NGar),xlab="N�mero de garajes",ylab="Frecuencia",main="Garajes en vivienda usada",col=terrain.colors(6))
dev.off()

#Estar

#Estudio




#Tablas de porcentajes de variables categ�ricas a resaltar

for (i in c(7,8,10,20,38,39,40)) {
  temp <- prop.table(table(as.matrix(base_us[i]), dnn = c(names(base_us[i]))))
  write.table(temp,"C:/Users/sarangof/Documents/Datos_Galeria_Inmobiliaria/R/Graficos exploratorios/tablas_prop_us.csv",sep=";", eol="\n",append=TRUE,dec=",")
}

summary <- as.data.frame()
media_us<-sapply(base_us, mean, na.rm=TRUE)
mediana_us<-sapply(base_us, median, na.rm=TRUE)
sd_us<-sapply(base_us, sd, na.rm=TRUE)
min_us <- sapply(base_us, min, na.rm=TRUE)
max_us <- sapply(base_us, max, na.rm=TRUE)

#EXPLORATORIO USADOS


#DESCRIPTIVO NUEVOS

Vende_mode_nv <- Mode(Vende)

#EXPLORATORIO NUEVOS


Datos$Numerounico <- 1:657
Datos$Edad1 <- cut(Datos$Edad,c(0,20,30,40,50,60,70,80,90)) 

# Reciprocidad

# Crear otras variables �tiles - estandarizar reciprocidad (similar a Perc1ofre)
Datos$PP2si0 <- Datos$P2si0/12000
Datos$PP2si3000 <- Datos$P2si3000/21000
Datos$PP2si6000 <- Datos$P2si6000/30000
Datos$PP2si9000 <- Datos$P2si9000/39000
Datos$PP2si12000 <- Datos$P2si12000/48000


Datos$Ganancias[Datos$Ganancias==6]=0 #Para que las categor�as est�n ordenadas

# Calcular la reciprocidad media 
rcps <- cbind(Datos$PP2si0,Datos$PP2si3000,Datos$PP2si6000,Datos$PP2si9000,Datos$PP2si12000)
Datos$rcps_m <- rowMeans(rcps,na.rm=TRUE)
Datos$PP1expec <- P1expect/(3*P1confianza+12000)
PP1expec[PP1expec>1]=NA 

attach(Datos) #Use simpler variable names. Be cautious with previous stored data.
#str(Datos) #


sum <- summary(Datos)
write.table(t(sum), "C:/Users/Sara/Documents/CSA/Analisis experimentos/Tablas/mysum.csv", sep=",", eol="\n")

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux, incomparables = NA)))]
}

moda <- sapply(Datos, Mode)
write.table(moda, "C:/Users/Sara/Documents/CSA/Analisis experimentos/Tablas/moda.csv", sep=",", eol="\n")



#Tablas de frecuencia totales
for (i in c(1:153,160)) {
  temp <- prop.table(table(Subregion,as.matrix(Datos[i]), dnn = c("Subregion", names(Datos[i]))))
  write.ftable(ftable(temp),"C:/Users/Sara/Documents/CSA/Analisis experimentos/Tablas/tablas.csv",append=TRUE)
}
for (i in c(1:153,160)) {
  temp <- prop.table(table(Sexo,as.matrix(Datos[i]), dnn = c("Sexo", names(Datos[i]))))
  write.ftable(ftable(temp),"C:/Users/Sara/Documents/CSA/Analisis experimentos/Tablas/tablas.csv",append=TRUE)
}


#Tablas de frecuencia por fila
for (i in c(1:153,160)) {
  temp <- prop.table(table(Subregion,as.matrix(Datos[i]), dnn = c("Subregion", names(Datos[i]))),1)
  write.ftable(ftable(temp),"C:/Users/Sara/Documents/CSA/Analisis experimentos/Tablas/tablasf.csv",append=TRUE)
}
for (i in c(1:153,160)) {
  temp <- prop.table(table(Sexo,as.matrix(Datos[i]), dnn = c("Sexo", names(Datos[i]))),1)
  write.ftable(ftable(temp),"C:/Users/Sara/Documents/CSA/Analisis experimentos/Tablas/tablasf.csv",append=TRUE)
}

#Tablas de frecuencia por columna
for (i in c(1:153,160)) {
  temp <- prop.table(table(Subregion,as.matrix(Datos[i]), dnn = c("Subregion", names(Datos[i]))),2)
  write.ftable(ftable(temp),"C:/Users/Sara/Documents/CSA/Analisis experimentos/Tablas/tablasc.csv",append=TRUE)
}
for (i in c(1:153,160)) {
  temp <- prop.table(table(Sexo,as.matrix(Datos[i]), dnn = c("Sexo", names(Datos[i]))),2)
  write.ftable(ftable(temp),"C:/Users/Sara/Documents/CSA/Analisis experimentos/Tablas/tablasc.csv",append=TRUE)
}

#Tablas por frecuencia absoluta

#Tablas de frecuencia de variables
for (i in c(1:153,160)) {
  temp <- table(as.matrix(Datos[i]), dnn = c(names(Datos[i])))
  write.table(temp,"C:/Users/Sara/Documents/CSA/Analisis experimentos/Tablas/tablasfIndiv.csv",sep=",", eol="\n",append=TRUE)
}

#Tablas de frecuencia totales
for (i in c(1:153,160)) {
  temp <- table(Subregion,as.matrix(Datos[i]), dnn = c("Subregion", names(Datos[i])))
  write.ftable(ftable(temp),"C:/Users/Sara/Documents/CSA/Analisis experimentos/Tablas/tablasfrec.csv",append=TRUE)
}
for (i in c(1:153,160)) {
  temp <- table(Sexo,as.matrix(Datos[i]), dnn = c("Sexo", names(Datos[i])))
  write.ftable(ftable(temp),"C:/Users/Sara/Documents/CSA/Analisis experimentos/Tablas/tablasfrec.csv",append=TRUE)
}


for (i in c(1:153,160)) {
  temp <- table(cut(Edad,c(0,20,30,40,50,60,70,80,90)),as.matrix(Datos[i]), dnn = c("Edad", names(Datos[i])))
  write.ftable(ftable(temp),"C:/Users/Sara/Documents/CSA/Analisis experimentos/Tablas/tablasfrec.csv",append=TRUE)
}
for (i in c(1:153,160)) {
  temp <- table(cut(Edad,c(0,26,90)),as.matrix(Datos[i]), dnn = c("Edad", names(Datos[i])))
  write.ftable(ftable(temp),"C:/Users/Sara/Documents/CSA/Analisis experimentos/Tablas/tablasfrec.csv",append=TRUE)
}
for (i in c(1:153,160)) {
  temp <- table(Niveducativ,as.matrix(Datos[i]), dnn = c("Nivel Educativo", names(Datos[i])))
  write.ftable(ftable(temp),"C:/Users/Sara/Documents/CSA/Analisis experimentos/Tablas/tablasfrec.csv",append=TRUE)
}


setwd("C:/Users/Sara/Documents/CSA/Analisis experimentos") #Set working directory
Datos = read.csv("Datos consolidados CSA version limpia.csv") #Read file
attach(Datos) #Use simpler variable names. Be cautious with previous stored data.
#str(Datos) #
require(grDevices)

#  [1] "Subregion"          "Sesion"             "Numerounico"       
#   [4] "Codigo"             "Fecha"              "Hora"              
#   [7] "Lugar"              "P1confianza"        "P1expect"          
#  [10] "P2si0"              "P2si3000"           "P2si6000"          
#  [13] "P2si9000"           "P2si12000"          "P2expect"          
#  [16] "Pareja"             "Perc1ofre"          "Perc2expec"        
#  [19] "VCMdec"             "VCMexpec"           "Sexo"              
#  [22] "Edad"               "Niveducativ"        "Estadocivil"       
#  [25] "Etnia"              "Anosmunic"          "Munnacim"          
#  [28] "Depnacim"           "Ocupacion"          "Salud"             
#  [31] "Ganancias"          "Gastos"             "Tipvivienda"       
#  [34] "Alcantarillado"     "Aguapotable"        "Gasnatural"        
#  [37] "Telefono"           "Energiaelec"        "Television"        
#  [40] "Internet"           "Basuras"            "Nopersonas"        
#  [43] "Nohijos"            "Escalera"           "Edoedu"            
#  [46] "EdoFA"              "Edosena"            "EdoICBF"           
#  [49] "Edosalud"           "Edootro"            "Edoninguna"        
#  [52] "Riesgo"             "Orgcaridad"         "Orgcomunitaria"    
#  [55] "Orgreligiosa"       "Orgestado"          "Orgetnica"         
#  [58] "Orgcultural"        "Orgasociac"         "Orgsindicato"      
#  [61] "Orgpartido"         "Orgotro"            "Orgninguno"        
#  [64] "Liderorg"           "Satisfvida"         "Confianza"         
#  [67] "Victimrobo"         "Victimdesplaz"      "Victimhomic"       
#  [70] "Victimdesap"        "Victimagres"        "Victimviosex"      
#  [73] "Victimsecues"       "Victimprop"         "Victimextor"       
#  [76] "Victimtortura"      "Victimotro"         "Victimninguno"     
#  [79] "Religion"           "Seguridad"          "Conffamilia"       
#  [82] "Confvecinos"        "Confcomun"          "Confotrarel"       
#  [85] "Confdespl"          "Confdesmov"         "Confprimera"       
#  [88] "Orgullocol"         "Satisfecon"         "Solconflicto"      
#  [91] "Interespol"         "Votpresid"          "Votlocales"        
#  [94] "Partidopol"         "Discrim"            "Razon" 
#  [97] "Confejercito"       "Confjudicial"       "Confgobierno"      
# [100] "Confcongreso"       "Confgobern"         "Confpolicia"       
# [103] "Configlesia"        "Confpresid"         "Confpartidos"      
# [106] "Confalcaldia"       "Confdefens"         "Confempr"          
# [109] "Confprocur"         "Conffiscalia"       "Confbancos"        
# [112] "Confsindicatos"     "Confmedios"         "Confuniver"        
# [115] "Solfamilia"         "Solcomunidad"       "Intervencion"      
# [118] "Discrimdrog"        "Discrimafro"        "Discrimdesmov"     
# [121] "Discrimhomo"        "Discrimalcoh"       "Discrimsida"       
# [124] "Discrimdesplaz"     "Discrimrelig"       "Discrimulibre"     
# [127] "Discrimindig"       "Discrimprost"       "Discrimcarcel"     
# [130] "Conocidossalon"     "Amigos"             "Cuentaahorro"      
# [133] "Segsocial"          "Cuentaotro"         "Partcampana"       
# [136] "Pertenecerorg"      "Pensionarse"        "Votar"             
# [139] "Trabajar"           "Estudiar"           "Casapropia"        
# [142] "Segurosalud"        "Prestamo"           "Afirmconfianza"    
# [145] "Afirmbeneficio"     "Afirmacuerdos"      "Afirmayudaestado"  
# [148] "Afirmsaliradelante" "Afirmcorrupcion"    "Afirmreglas"       
# [151] "Afirmplata"         "Motivaciones"       "Compjuego" 

#____________________________________________________________________________________________________________________________________________
#Creación de variables potencialmente útiles

Ganancias[Ganancias==6]=0 #Para que las categor�?as estén ordenadas

#NAs per variable

NApervar <- function(X1){
  D1 <- is.na(X1) 
  colSums(D1, na.rm = TRUE) 
}
NAs <- NApervar(Datos)


#Convertir variables a factores 
fSexo <- factor(Sexo,levels=c(0,1),labels=c("Hombre","Mujer"))
fRiesgo <- factor(Riesgo,levels=c(0,1),labels=c("Aversion","Disposicion"))
fConfianza <- factor(Confianza,c(0,1),labels=c("Confianza","Precaución"))
fRiesgo <- factor(Riesgo,levels=c(0,1),labels=c("Aversion","Disposicion"))
fTipvivienda <- factor(Tipvivienda,levels=c(1,2,3,4),labels=c("Propia","Renta","Cuarto","Otro"))
fVCMdec <- factor(VCMdec,levels=c(0,1),labels=c("Privada","Grupal"))
fSubregion <- factor(Subregion,levels=c(1,2,3,4,5,6,7,8,9),labels=c("VA", "UR", "BC", "N", "NE", "MM", "OR", "SO", "OCC"))
fGanancias <- factor(Ganancias,levels=c(0,1,2,3,4,5),labels=c("0","<1 sm","1-2 sm","2-3 sm","3-4 sm",">4 sm"))
fNiveducativ <- factor(Niveducativ,levels=c(1,2,3,4,5,6,7,8,9,10),labels=c("Ninguno","Primaria inc.","Primaria comp.","Secundaria incom.","Bachiller","Técnico incomp.","Técnico comp.","Universitario incomp.","Universitario comp.","Posgrado"))
fjoven <- Edad<=26

# Agrupar variables de servicios públicos y beneficios del estado
Servicios <- Datos[,34:41]
bfEstado <- Datos[,45:51]
Organizaciones <- Datos[,53:63]
victim <- Datos[,67:78]
confianzaGrps <- Datos[,81:87] 
confianzaInst <- Datos[,97:114] 
discrimn <- Datos[,118:129] 
oportunidades <- Datos[,132:143] 
afirmaciones <- Datos[,144:151] 

lrg <- length(Datos[,1])

# Crear otras variables útiles - estandarizar reciprocidad (similar a Perc1ofre)
PP2si0 <- P2si0/12000
PP2si3000 <- P2si3000/21000
PP2si6000 <- P2si6000/30000
PP2si9000 <- P2si9000/39000
PP2si12000 <- P2si12000/48000

# Calcular la reciprocidad media 
rcps <- cbind(PP2si0,PP2si3000,PP2si6000,PP2si9000,PP2si12000)
rcps_m <- rowMeans(rcps,na.rm=TRUE)


PP1expec <- P1expect/(3*P1confianza+12000)
Datos$PP1expec <-PP1expec
PP1expec[PP1expec>1]=NA 

#_______________________________________________________________________________________________________________________________________
# Caracter�?sticas socioeconómicas

# Gráficas sobre caracter�?sticas socioecomómicas
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/sociodemo1.png", height=800, bg="white")
op<- par(mfrow=c(3,2),mar=c(3,3,2,1))

#Proporción de hombres y de mujeres
slices <- table(fSexo)
pct <- round(100*slices/sum(slices[]))
lbls <- names(slices)
lbls <- paste(lbls,pct)
lbls <- paste(lbls,"%",sep="")
pie(slices, col=topo.colors(length(lbls)), label=lbls,main="Representantividad de géneros")

# Dispersión de las edades
boxplot(Edad,main="Representatividad de edades",col="pink")

# Pirámide poblacional
tblSE <- (100*table(fSexo,cut(Edad,c(0,20,30,40,50,60,70,80,90)))/min(c(length(fSexo),length(Edad))) ) #Clasifica encuestados según sexo y edad
barplot(tblSE,names=c("<20","20-30","30-40","40-50","50-60","60-70","70-80",">80"),main="Distribución por edad y género",col=cm.colors(2),legend=TRUE,ylab="Porcentaje",xlab="Rango de edad")



slices <- c(sum(Etnia==1,na.rm=TRUE),sum(Etnia==2,na.rm=TRUE),sum(Etnia==3,na.rm=TRUE),sum(Etnia==4,na.rm=TRUE))
lbls <- c("Blanco","Ind�?gena","Mestizo","Afrodescendiente")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(slices,labels = lbls, col=topo.colors(length(lbls)), main="Etnia reportada por los encuestados")

barplot(100*table(fGanancias)/length(fGanancias),main="Ingresos mensuales",col="green",xlab="Salarios m�?nimos mensuales (%)")


slices <- c(sum(fTipvivienda=="Propia",na.rm=TRUE),sum(fTipvivienda=="Renta",na.rm=TRUE),sum(fTipvivienda=="Cuarto",na.rm=TRUE),sum(fTipvivienda=="Otro",na.rm=TRUE))
lbls <- unique(fTipvivienda)
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(table(fTipvivienda),labels=lbls, main="Tipo de vivienda")

par(op)
dev.off()

png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/sociodemo2.png", height=600, bg="white")
op<- par(mfrow=c(2,2),mar=c(3,3,2,1))

barplot(colSums(100*(Servicios/lrg),na.rm=TRUE),horiz=TRUE,las=1,cex.names=0.6,main="Acceso a servicios",xlab="Encuestados que reportan tener tal servicio en su hogar(%).",col=cm.colors(8))
grid()


boxplot(Nohijos,col="Green",main="Número de hijos")

barplot(100*table(Escalera)/length(Escalera),main="Percepción de riqueza",col=rainbow(10),xlab="Posición en la escalera de riqueza",ylab="Porcentaje")

barplot(colSums(100*(bfEstado)/lrg,na.rm=TRUE),horiz=TRUE,las=1,cex.names=0.6,main="Asistencia del estado reportada",ylab="Tipo de asistencia",xlab="Porcentaje",names=c("Ninguna","Otro","Salud","ICBF","Sena","Familias en acción","Educación"),col=topo.colors(7))

par(op)
dev.off()

# Representatividad de sexos por subregión
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/sexsub.png", height=600, bg="white")
tblsubsex <- 100*table(fSubregion,fSexo)
plot(prop.table(tblsubsex),main="Sexo por subregión",xlab="Subregión",col=topo.colors(2)) 
dev.off()

# Edad por sexo
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/Género comparado con e.png", height=600, bg="white")
boxplot(Edad ~ fSexo,col=c("blue","orange"),varwidth=TRUE,main="Género/Edad")
dev.off()

# Edad por subregión
boxplot(Edad ~ fSubregion,col=cm.colors(fSubregion),varwidth=TRUE,main="Edad por subregión")
grid()

#___________________________________________________________________________________________________________________________________________
# Variables de interés


# Confianza en los demás 
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/confianza.png", height=400, bg="white")
slices <- c(sum(Confianza==0,na.rm=TRUE),sum(Confianza==1,na.rm=TRUE))
lbls <- c("Precaución","Confianza")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(slices,labels = lbls, col=topo.colors(length(lbls)), main="Confianza en los demás")
dev.off()

# Avesión al riesgo
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/riesgo.png", height=400, bg="white")
slices <- c(sum(Riesgo==0,na.rm=TRUE),sum(Riesgo==1,na.rm=TRUE))
lbls <- c("No se arriesga", "Se arriesga")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(slices,labels = lbls, col=cm.colors(length(lbls)), main="Riesgo")
dev.off()

# Satisfacción con la vida
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/satisfvida.png", height=400, bg="white")
slices <- c(sum(Satisfvida==1,na.rm=TRUE),sum(Satisfvida==2,na.rm=TRUE),sum(Satisfvida==3,na.rm=TRUE),sum(Satisfvida==4,na.rm=TRUE))
lbls <- c("Muy satisfecho","Algo satisfecho","Algo insatisfecho","Muy insatisfecho")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(slices,labels = lbls, col=cm.colors(length(lbls)), main="Satisfacción con la vida")
dev.off()

# Pertenencia a organizaciones
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/organizaciones.png", height=400, bg="white")
lrgOrg <- length(Organizaciones[,1])
barplot(colSums(100*(Organizaciones)/lrg,na.rm=TRUE),horiz=TRUE,las=1,cex.names=0.6,main="Pertenencia a organizaciones reportada",xlab="Porcentaje",names=c("Caridad","Comunitaria","Religiosa","Participación","Étnica","Cultural/deportiva","Social","Sindicato","Partido","Otro","Ninguno"),col=topo.colors(11))
dev.off()

png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/victimizacion.png", width=1000, bg="white")
barplot(colSums(100*(victim)/lrg,na.rm=TRUE),las=1,cex.names=0.6,main="Victimización reportada",ylab="Porcentaje",names=c("Robo","Desplazamiento","Homicidio de un familiar","Desaparición de un familiar","Agresión f�?sica","Violencia sexual","Secuestro","Daño a su propiedad","Extorsión","Tortura","Otro","Ninguno"),col=topo.colors(11),horiz=FALSE,las=3)
dev.off()

n1= c("su familia", "sus vecinos", "su comunidad","gente de otra religión","desplazados","desmovilizados","gente que conoce por primera vez")

for (i in 1:7){
  png(filename=paste("C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/confgrp",n1[i],".png"), height=300, bg="white")
  barplot(100*table(as.factor(confianzaGrps[,i]))/lrg,names=c("Conf�?a mucho","Conf�?a algo","Conf�?a poco","No conf�?a"),col="green",main=paste("Confianza en",n1[i]),ylab="Porcentaje")
  dev.off()
}


n2= c("el Ejército", "el Sistema Judicial","el Gobierno Nacional", "el Congreso", "la Gobernación de Antioquia", "la Polic�?a", "la Iglesia Católica","el presidente","los Partidos Pol�?ticos", "la Alcald�?a municipal","la defensor�?a","las empresas privadas", "la Procuradur�?a", "la Fiscal�?a General", "los bancos", "los Sindicatos","los medios de comunicación", "las Universidades")

for (i in 1:18){
  png(filename=paste("C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/confinst",n2[i],".png"), width=550, bg="white")
  barplot(100*table(as.factor(confianzaInst[,i]))/lrg,names=c("Conf�?a mucho","Conf�?a algo","Conf�?a poco","No conf�?a","No sabe"),col="pink",main=paste("Confianza en ",n2[i]),ylab="Porcentaje")
  dev.off()
}

# Religión
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/religion.png", height=450, bg="white")
slices <- c(sum(Religion==1,na.rm=TRUE),sum(Religion==2,na.rm=TRUE),sum(Religion==3,na.rm=TRUE),sum(Religion==4,na.rm=TRUE),sum(Religion==5,na.rm=TRUE),sum(Religion==6,na.rm=TRUE))
lbls <- c("Católico","Cristiano","Evangelista","Protestante","Otro","Ninguno")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(slices,labels = lbls, col=topo.colors(length(lbls)), main="Religión reportada",cex=0.8)
dev.off()

# Orgullo de ser colombiano
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/orgullocol.png", height=350, bg="white")
slices <- c(sum(Orgullocol==1,na.rm=TRUE),sum(Orgullocol==2,na.rm=TRUE),sum(Orgullocol==3,na.rm=TRUE),sum(Orgullocol==4,na.rm=TRUE))
lbls <- c("Muy orgulloso","Algo orgulloso","Poco orgulloso","Nada orgulloso")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(slices,labels = lbls, col=cm.colors(length(lbls)), main="Orgullo de ser colombiano")
dev.off()

# Satisfacción económica
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/satifecon.png", height=350, bg="white")
slices <- c(sum(Satisfecon==1,na.rm=TRUE),sum(Satisfecon==2,na.rm=TRUE),sum(Satisfecon==3,na.rm=TRUE),sum(Satisfecon==4,na.rm=TRUE))
lbls <- c("Muy satisfecho","Algo satisfecho","Algo insatisfecho","Muy insatisfecho")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(slices,labels = lbls, col=cm.colors(length(lbls)), main="Satisfacción económica")
dev.off()

# Solución familia
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/solfamilia.png", height=350, bg="white")
slices <- c(sum(Solfamilia==0,na.rm=TRUE),sum(Satisfecon==1,na.rm=TRUE))
lbls <- c("No","S�?")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(slices,labels = lbls, col=cm.colors(length(lbls)), main="¿Ha contribu�?do a solucionar problemas de su familia en el último año?")
dev.off()

# Solución familia
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/solcomunidad.png", height=350, bg="white")
slices <- c(sum(Solcomunidad==0,na.rm=TRUE),sum(Solcomunidad==1,na.rm=TRUE))
lbls <- c("No","S�?")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(slices,labels = lbls, col=cm.colors(length(lbls)), main="¿Ha contribu�?do a solucionar problemas de su comunidad en el último año?")
dev.off()

# Discriminación
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/discrim.png", height=600, bg="white")
barplot(colSums(100*discrimn/lrg,na.rm=TRUE),horiz=TRUE,las=1,names=c("Drogadictos","Afrodescendientes","Desmovilizados","Homosexuales","Alcohólicos","Sida","Desplazados","Otras religiones","Unión libre","Ind�?genas","Prostitutas","Cárcel"),col="purple")
dev.off()

# Número de conocidos en el salón
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/conocidossalon.png", height=400, bg="white")
boxplot(Conocidossalon,main="Número de conocidos en el ejercicio",ylab="Número de personas",col="yellow")
dev.off()

# Número de amigos
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/amigos.png", height=400, bg="white")
boxplot(Amigos[Amigos<100],main="Número de amigos (menores que 100)",ylab="Número de personas",col="red")
dev.off()

# Oportunidades
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/oportunidades.png", height=400, bg="white")
barplot(colSums(100*(oportunidades)/lrg,na.rm=TRUE),las=1,cex.names=0.6,main="Acceso a oportunidades",xlab="Porcentaje",names=c("Cuentas ahorro","Seguiridad social", "Otra cuenta", "Campaña pol�?tica", "Pertenecer organización",	"Pensionarse",	"Votar",	"Trabajar",	"Estudiar", "Casa propia",	"Seguro salud",	"Prestamo"),col=topo.colors(11),horiz=TRUE)
dev.off()

png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/afirmaciones.png", height=400, bg="white")
barplot(colSums(100*(afirmaciones)/lrg,na.rm=TRUE),las=1,cex.names=0.6,main="Afirmaciones clave",xlab="Porcentaje",names=c("Confianza",	"beneficio","acuerdos", "ayudaestado","salir adelante", "Corrupcion","Reglas","Plata"),col=topo.colors(11),horiz=TRUE)
dev.off()

# Motivaciones
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/afirmaciones.png", height=500, bg="white")
slices <- c(sum(Motivaciones==1,na.rm=TRUE),sum(Motivaciones==2,na.rm=TRUE),sum(Motivaciones==3,na.rm=TRUE),sum(Motivaciones==4,na.rm=TRUE),sum(Motivaciones==5,na.rm=TRUE))
lbls <- c("Beneficio propio","Beneficio del grupo", "Confianza en los demás","Incertidumbre", "Emociones")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(slices,labels = lbls, col=cm.colors(length(lbls)), main="Motivaciones en el juego")
dev.off()

# Motivaciones
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/comprjuego.png", height=400, bg="white")
slices <- c(sum(Compjuego==0,na.rm=TRUE),sum(Compjuego==1,na.rm=TRUE))
lbls <- c("No","S�?")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(slices,labels = lbls, col=cm.colors(length(lbls)), main="Comprensión del juego")
dev.off()

png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/juventud.png", height=400, bg="white")
slices <- c(sum(fjoven==0,na.rm=TRUE),sum(fjoven==1,na.rm=TRUE))
lbls <- c("Jóven","Adulto")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(slices,lbls,main="Porcentaje de jóvenes en la muestra",col=topo.colors(2))
dev.off()

#______________________________________________________________________________________________________________________________________
# Variables de respuesta

#Confianza 

# Graficar confianza global
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/conf12.png", height=400, bg="white")
boxplot(Perc1ofre,main="Confianza total",ylab="Dinero enviado por el jugador 1 al jugador 2",col="pink")
dev.off()

# Graficar confianza por subregión
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/conf12subregion.png", height=400, bg="white")
boxplot(Perc1ofre ~fSubregion,main="Confianza por subregión",xlab="Subregion",ylab="Dinero enviado por el jugador 1 al jugador 2 (porcentaje)",cex=0.8,col=rainbow(9))
dev.off()

# Graficar confianza por Lugar
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/conf12lugar.png", width=900, bg="white")
boxplot(Perc1ofre ~as.factor(Lugar),main="Confianza por Municipio",ylab="Dinero enviado por el jugador 1 al jugador 2 (porcentaje)",cex=0.8,las=3,cex.names=0.3,col=rainbow(28),horiz=TRUE)
dev.off()

# Graficar confianza según Juventud
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/conf12joven.png", height=400, bg="white")
boxplot(Perc1ofre ~ fjoven,main="Confianza según edad",ylab="Dinero enviado por el jugador 1 al jugador 2 (porcentaje)",cex=0.8,names=c("Jóvenes","Adultos"),col=topo.colors(2))
dev.off()

# Graficar confianza según Género
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/conf12sexo.png", height=400, bg="white")
boxplot(Perc1ofre ~ fSexo,main="Confianza según género",ylab="Dinero enviado por el jugador 1 al jugador 2 (porcentaje)",cex=0.8,col=cm.colors(2))
dev.off()

# Graficar confianza según nivel educativo
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/conf12niveducativ.png", height=400, bg="white")
boxplot(Perc1ofre ~ fNiveducativ,main="Confianza según nivel educativo",ylab="Dinero enviado por el jugador 1 al jugador 2 (porcentaje)",cex=0.8,col=cm.colors(10),las=3,cex.lab=0.5)
dev.off()

# Reciprocidad

png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/rpcs.png", height=400, bg="white")
boxplot(rcps_m,main="Reciprocidad total media",ylab="Dinero enviado por el jugador 2 al jugador 1 (porcentaje)",col="green")
dev.off()

# Graficar reciprocidad media por región
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/recipsubregion.png", height=400, bg="white")
boxplot(rcps_m ~fSubregion,main="Reciprocidad media por subregión",xlab="Subregion",ylab="Dinero enviado por el jugador 2 al jugador 1 (porcentaje)",cex=0.8,col=rainbow(9))
dev.off()


# Graficar confianza por Lugar
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/rcpslugar.png", width=900, bg="white")
boxplot(rcps_m ~as.factor(Lugar),main="Reciprocidad media por Municipio",ylab="Dinero enviado por el jugador 2 al jugador 1 (porcentaje)",cex=0.8,las=3,cex.names=0.3,col=rainbow(28),horiz=TRUE)
dev.off()

# Graficar reciprocidad según Juventud
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/rcpsjoven.png", height=400, bg="white")
boxplot(rcps_m ~ fjoven,main="Reciprocidad según edad",ylab="Dinero enviado por el jugador 2 al jugador 1 (porcentaje)",cex=0.8,names=c("Jóvenes","Adultos"),col=topo.colors(2))
dev.off()

# Graficar reciprocidad según Género
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/rcpssexo.png", height=400, bg="white")
boxplot(rcps_m ~ fSexo,main="Reciprocidad según género",ylab="Dinero enviado por el jugador 2 al jugador 1 (porcentaje)",cex=0.8,col=cm.colors(2))
dev.off()


# Graficar reciprocidad según nivel educativo
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/rcpsniveducativ.png", width=900, bg="white")
boxplot(rcps_m ~ fNiveducativ,main="Reciprocidad según nivel educativo",ylab="Dinero enviado por el jugador 2 al jugador 1 (porcentaje)",cex=0.8,col=cm.colors(10),las=3,cex.lab=0.5)
dev.off()


# Acción colectiva

png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/vcm.png", height=400, bg="white")
slices <- c(sum(Datos$VCMdec==0,na.rm=TRUE),sum(Datos$VCMdec==1,na.rm=TRUE))
lbls <- c("No contribución","Contribución")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(slices,labels = lbls, col=cm.colors(length(lbls)), main="Contribuciones al MCV")
dev.off()

# Graficar acción colectiva por subregión
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/vcmsubregion.png", height=400, bg="white")
tbp <-  round(prop.table(table(VCMdec,fSubregion),2),2)
bplt <- barplot(tbp, 
                main="Acción colectiva por Subregión", xlab="Subregión", 
                col=topo.colors(2), legend = c("Privada","Colectiva"), 
                beside=TRUE, horiz=TRUE,las=1,args.legend = list(title = "Cuenta", x = "bottomleft", cex = .7,bg="white"),ylab="Subregión")
text(x= tbp+0.03, y= bplt+0.2, labels=as.character(tbp), xpd=TRUE)
dev.off()


# Graficar accion colectiva por Lugar
# png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/vcmlugar.png", height=900, bg="white")
#  tbp <-  round(prop.table(table(VCMdec,as.factor(Lugar)),2),2)
#  bplt <- barplot(tbp, 
#                  main="Acción colectiva por Lugar", xlab="Lugar", 
#                  col=cm.colors(2), legend = c("Privada","Colectiva"), 
#                  beside=TRUE, horiz=TRUE,las=1,args.legend = list(title = "Cuenta", x = "bottomleft", cex = .3,bg="white"))
#  text(x= tbp+0.03, y= bplt+0.2, labels=as.character(tbp), xpd=TRUE)
# dev.off()

# Graficar reciprocidad según Juventud
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/vcmjoven.png", height=400, bg="white")
tbp <-  round(prop.table(table(VCMdec,fjoven),2),2)
bplt <- barplot(tbp, 
                main="Acción colectiva según edad", xlab="Grupos de edad", 
                col=cm.colors(2), legend = c("Privada","Colectiva"), 
                beside=TRUE, horiz=TRUE,las=1,args.legend = list(title = "Cuenta", x = "topright", cex = 1,bg="white"),names=c("Joven","Adulto"))
text(x= tbp+0.03, y= bplt, labels=as.character(tbp), xpd=TRUE)
dev.off()


# Graficar reciprocidad según Género
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/vcmsexo.png", height=400, bg="white")
tbp <-  round(prop.table(table(VCMdec,fSexo),2),2)
bplt <- barplot(tbp, 
                main="Acción colectiva según género", xlab="Género", 
                col=topo.colors(2), legend = c("Privada","Colectiva"), 
                beside=TRUE, horiz=TRUE,las=1,args.legend = list(title = "Cuenta", x = "topright", cex = 1,bg="white"))
text(x= tbp+0.03, y= bplt, labels=as.character(tbp), xpd=TRUE)
dev.off()


# Graficar reciprocidad según nivel educativo
png(filename="C:/Users/Sara/Documents/CSA/Analisis experimentos/descrip/vcmniveducativ.png", width=900, bg="white")
tbp <-  round(prop.table(table(VCMdec,fNiveducativ),2),2)
bplt <- barplot(tbp, 
                main="Acción colectiva según nivel educativo", xlab="Nivel educativo", 
                col=cm.colors(2), legend = c("Privada","Colectiva"), 
                beside=TRUE, horiz=TRUE,las=1,args.legend = list(title = "Cuenta", x = "bottomleft", cex = 0.5,bg="white"))
text(x= tbp+0.03, y= bplt, labels=as.character(tbp), xpd=TRUE)
dev.off()


















# Graficar reciprocidad según monto enviado (mejorar esta gráfica)
lmts <- range(PP2si0,PP2si3000,PP2si6000,PP2si9000,PP2si12000,na.rm=TRUE)
oop <- par(mfrow = c(1,5))

boxplot(PP2si0,ylim=lmts,axes=FALSE,xlab=0)
boxplot(PP2si3000,ylim=lmts,axes=FALSE,xlab=3000)
boxplot(PP2si6000,ylim=lmts,axes=FALSE,xlab=6000)
boxplot(PP2si9000,ylim=lmts,axes=FALSE,xlab=9000)
boxplot(PP2si12000,ylim=lmts,axes=FALSE,xlab=12000)

par(op)

#Acción colectiva

# Graficar acción colectiva global
slices <- c(sum(Datos$VCMdec==0,na.rm=TRUE),sum(Datos$VCMdec==1,na.rm=TRUE))
lbls <- c("No contribución","Contribución")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(slices,labels = lbls, col=topo.colors(length(lbls)), main="Contribuciones al MCV")

#Contribucion según regiones - Arreglar leyenda
barplot(prop.table(table(as.factor(VCMdec),fSubregion),2),main="Contribución según subregión",legend(2.8,0,c("C. Privada","Cuenta grupal")))

#____________________________________________________________________________________________________________________________________________
# Asociaciones (hipótesis)


# Graficar confianza según género
boxplot(Perc1ofre~fSexo, main="Confianza del Jugador 1 según Género", xlab="Género", ylab="Dinero enviado", col=cm.colors(fSexo))


# Porcentaje del dinero ofrecido según percepción de pobreza
boxplot(Perc1ofre ~ as.factor(Escalera),main="Confianza según percepción de pobreza",col=cm.colors(as.factor(Escalera)),ylab="Dinero enviado",xlab="Peldaño en la escalera")

# Graficar acción colectiva según actitud sobre el conflicto
plot(as.factor(VCMdec),as.factor(Solconflicto))

# Jóvenes

# confianza, cooperación y reciprocidad 





boxplot(Perc1ofre ~ fjoven,main="Confianza según factor de juventud",col=cm.colors(as.factor(fjoven)),ylab="Porcentaje enviado",xlab="PJuventud")

prop.table(table(Perc1ofre,fjoven),1)

anova (lm(Perc1ofre ~ fjoven))

prop.table(table(rcps_m ,fjoven),1)

anova (lm(rcps_m  ~ fjoven))

boxplot(rcps_m  ~ fjoven,main="Confianza según factor de juventud",col=cm.colors(as.factor(fjoven)),ylab="Porcentaje enviado",xlab="PJuventud")

prop.table(table(VCMdec ,fjoven),1)

anova (lm(VCMdec  ~ fjoven))

boxplot(VCMdec  ~ fjoven,main="Confianza según factor de juventud",col=cm.colors(as.factor(fjoven)),ylab="Porcentaje enviado",xlab="PJuventud")

# Afirmaciones
tb.af <- (colSums(afirmaciones,na.rm=TRUE)/lrg)*100
names(tb.af)<-c("Plata","Reglas","Corrupcion","Adelante","Estado","Acuerdos","beneficio","confianza")

leyes <- cbind(afirmaciones[,1:3],afirmaciones[,6:7])
leyes <- cbind(tb.af[,1:3],tb.af[6:7])
fconfianza <- leyes[,1]
afbeneficio <- leyes[,2]
afacuerdos <- leyes[,3]
afcorrupcion <- leyes[,4]
afreglas <- leyes[,5]



Part <- Orgninguno==0

solalgo <- Solcomunidad==1 | Solfamilia==1


#