setwd("C:/Users/sarangof/Documents/Datos Galeria Inmobiliaria/R/Nueva")

# Cargar bases de datos

hist <- read.csv2("Nuevos Historico.csv",sep=";")
proy <- read.csv2("Nuevos Proyecto.csv",sep=";")
serv<- read.csv2("Nuevos serv. com..csv",sep=";")


setwd("C:/Users/sarangof/Documents/Datos Galeria Inmobiliaria/R")

hola <- merge(hist,proy,by="Codproyecto")
hola <- merge(hola,serv,by="Codproyecto")
hola1 <- hola[which(hola$Zona.x=="Med. No VIS" | hola$Zona.x=="Med. VIS"), ]

write.csv2(hola1,"nv_pls.csv")