library(sp)
library(lattice)
library(ggplot2)
library(spgwr)

setwd("D:/Documents/CUSP/Fall 2015/Applied Data Science/Foundations project")

base_tot <- read.csv2("OIME.csv",sep=",")

modelGenerator <- function(variables){
  #preparación
  temp <- base_tot[complete.cases(base_tot[,variables]),]
  exp <- paste("Precio~",paste(variables[1:length(variables)],collapse="+")) # Expresar en fórmula
  
  #generar modelo lineal
  linear_model <- lm(data=temp,exp)
  pred_lm <- linear_model$fitted.values # Predicción
  res_lm <- linear_model$residuals # Residuales
  res_lm_abs <- abs(res_lm)
  coefs <- as.data.frame(coef(linear_model))[1:length(variables),]
  sz <- length(temp$Precio)
  coef_lm <- data.frame(matrix(0, ncol = length(variables), nrow = sz))
  for (i in 1:length(variables)){
    coef_lm[i] <- rep(coefs[i],sz)   
  } 
  #coordu[,length(names(usada))+1] <- 1
   
  #generar GWR
  map_gwr = SpatialPointsDataFrame(data=temp, coords=cbind(temp$coordX,temp$coordY))
  bw <- gwr.sel(exp, data=map_gwr, adapt=T)
  gwr.model <- gwr(exp, data=map_gwr, adapt=bw)
  pred_gwr <- gwr.model$SDF$pred
  res_gwr <- temp$Precio - pred_gwr
  res_gwr_abs <- abs(res_gwr) # Debo imprimir coeficientes para todo el mundo también
  r2_gwr <- gwr.model$SDF$localR2
  #salida: csv del mapa con residuales, valores predichos, debe tener nomenclatura que incluya las variables
  #print summary R2 
   
  #imprimir resultados
  df_lm <- cbind(temp$Precio,pred_lm,res_lm,res_lm_abs,coef_lm,temp$coordX,temp$coordY)#coef_lm,pred_gwr,res_gwr_abs,r2_gw)
  archivo <- paste("LM","_",toString(variables),"_","oime.csv")
  write.csv2(df_lm,archivo)
  
  coef_gwr <- data.frame(matrix(0, ncol = 1, nrow = sz))
  for (i in 1:length(variables)+1){
    coef_gwr[i] <- as.data.frame(gwr.model$SDF)[i+2]   
  } 
  coef_gwr[1] <- as.data.frame(gwr.model$SDF)[3]  
  names(coef_gwr)[1] <- names(as.data.frame(gwr.model$SDF))[3]
  df_gwr <- cbind(temp$Precio,pred_gwr,res_gwr,res_gwr_abs,r2_gwr,coef_gwr,temp$coordX,temp$coordY)
  archivo_gwr <- paste("GWR","_",toString(variables),"_","oime.csv")
  write.csv2(df_gwr,archivo_gwr)
  
  return(df) #agregar condicional por si sucede que falle el método #imprimir condiciones del método, por qué no.

}

variables <- cbind("Area","Estrato","d_pob_05","D_Green")
prueba <- modelGenerator(variables)
