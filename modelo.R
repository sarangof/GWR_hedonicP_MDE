library(sp)
library(lattice)
library(ggplot2)
library(spgwr)

setwd("C:/Users/sarangof/Documents/Datos_Galeria_Inmobiliaria/R")
usada <- read.csv2("base_us.csv",sep=";")
nueva <- read.csv2("base_nv.csv",sep=";")

map = SpatialPointsDataFrame(data=base_us, coords=cbind(base_us$coordX,base_us$coordY))
colours = c("light blue", "blue", "dark blue", "black")
spplot(map, "Area", cuts=quantile(base_us$Area,na.rm=TRUE), col.regions=colours,cex=0.6)

variables <- cbind("Precio","Area","Baños")

modelGenerator <- function(variables,bdd){
  
  #preparación
  if (bdd=="nueva"){
    prueba <- base_nv[complete.cases(base_nv[,variables]),]
  } else if (bdd=="usada"){
    prueba <- base_us[complete.cases(base_us[,variables]),]
  }
  id <- 2:length(variables)
  exp <- paste("Precio~",paste(variables[id],collapse="+"))
  
  #generar modelo lineal
  linear_model <- lm(data=prueba,exp)
  pred_lm <- linear_model$fitted.values
  res_lm <- linear_model$residuals
  res_lm_abs <- abs(res_lm)
  
  #generar GWR
  map = SpatialPointsDataFrame(data=prueba, coords=cbind(prueba$coordX,prueba$coordY))
  bw = gwr.sel(Precio~Area, data=map, adapt=T)
  #bw = gwr.sel(Precio~Area, data=base_us, coords=cbind(base_us$coordX,base_us$coordY),adapt=FALSE)
  gwr.model = gwr(Precio~Area, data=map, adapt=bw)
  
  #salida: csv del mapa con residuales, valores predichos, debe tener nomenclatura que incluya las variables
  #print summary R2 
  return (head(prueba))
}




lapply(Formulas,function(i)
  lm(as.formula(i),data=DF))

#Solo Area

prueba <- base_us[complete.cases(cbind(base_us$Precio,base_us$Area)),]
map = SpatialPointsDataFrame(data=prueba, coords=cbind(prueba$coordX,prueba$coordY))
bw = gwr.sel(Precio~Area, data=map, adapt=T)
#bw = gwr.sel(Precio~Area, data=base_us, coords=cbind(base_us$coordX,base_us$coordY),adapt=FALSE)
gwr.model = gwr(Precio~Area, data=map, adapt=bw)
gwr.model
map = SpatialPointsDataFrame(data=as.data.frame(gwr.model$SDF), coords=cbind(prueba$coordX,prueba$coordY))
spplot(map, "Area", cuts=quantile(map$Area,na.rm=TRUE), col.regions=colours,cex=0.6,)


lm1 <- lm(prueba$Precio ~ prueba$Area)






write.csv2(base_us,"base_us.csv")
write.csv2(base_nv,"base_nv.csv")

NApervar <- function(X1){
  D1 <- is.na(X1) 
  colSums(D1, na.rm = TRUE) 
}
NAs_nv <- NApervar(base_nv)
NAs_us <- NApervar(base_us)

write.csv2(NAs_nv,"NAs_nv.csv")
write.csv2(NAs_us,"NAs_us.csv")
