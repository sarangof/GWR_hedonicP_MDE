
data = read.csv2("data.csv")
datos <- subset(data, Estado=="Term.")

#para usados

datos <- subset(dfr2, Zona=="Med. No VIS" | Zona=="Med. VIS" )

proy = read.csv2("Nuevos Proyecto.csv")
hist = read.csv2("Nuevos Historico.csv")
serv = read.csv2("Nuevos Serv. Com..csv")
dfr = merge(hist,proy)
dfr2 = merge(dfr,serv)


datos=read.csv2("Usada Consolidada.csv",sep=";",na.strings=c("NA","NaN", " ",""))
datos=read.csv2("Nueva consolidada.csv",na.strings=c("NA","NaN", " ",""))
datos=read.csv2("Nuevo consolidado 2.csv",na.strings=c("NA","NaN", " ",""))


direc$ciudad={}
for(i in 1:1569){
  if (direc$Zona[i]=="Otros No VIS" | direc$Zona[i]=="Otros VIS")
  {direc$ciudad[i] <- direc$Sub.Zona[i]}
  else 
  {direc$ciudad[i] <- "Medellin"}
}

ter={}
for(i in 1:1906){
  if (datos$Area.Terraza[i]!="No" & datos$Area.Terraza[i]!="NO" & is.na(datos$Area.Terraza[i])==FALSE)
  {ter[i] <- "Si"}
  else 
  {ter[i] <- "No"}
}

direc$ciudad={}
for(i in 1:1569){
  direc$ciudad[i] <- direc$Sub.Zona[i]
}

#facilmente podría "traducir",pero sería bueno entender qué pasó


direc=read.csv2("direccionesMetropol.csv",sep=";",na.strings=c("NA","NaN", " ",""))


myHist <- function(mu){ 
  hist(galton$child,col="blue",breaks=100)  
  lines(c(mu, mu), c(0, 150),col="red",lwd=5)  
  mse <- mean((galton$child - mu)^2)  
  text(63, 150, paste("mu = ", mu))  
  text(63, 140, paste("MSE = ", round(mse, 2))) 
} 
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5)
           
rho = cor(datos$Precio,datos$Area)
y <- (datos$Precio - mean(datos$Precio,na.rm=TRUE)) / sd(datos$Precio,na.rm=TRUE)
x <- (datos$Area - mean(datos$Area,na.rm=TRUE)) / sd(datos$Area,na.rm=TRUE)
rho=cor(x,y,na.rm=TRUE)
myPlot <- function(x, y) {  
  plot(x, y,        
       xlab = "Area, normalizada",       
       ylab = "Precio, normalizado",       
       bg = "lightblue", col = "black", cex = 1, pch = 21,        
       frame = FALSE) 
}
