library(sp)
library(rgdal)
require(broom)

#Base de Usados

lm1 <- lm(data=base_us, Precio ~ Area + as.numeric(Estrato) + Piso + as.numeric(Banos) + NGar )
#lm1 <- lm(data=base_us, Precio ~ Area + Estrato + Piso + Banos + DistCentro + NGar)

interc <- tidy(lm1)[1,2]
coefs <- tidy(lm1)[2:6,2]
matriz_reg <- cbind(base_us$Area, as.numeric(base_us$Estrato), base_us$Piso, as.numeric(base_us$Banos), base_us$NGar)
pred <- rep(interc, length(base_us[,1])) + matriz_reg %*% coefs

res <- base_us$Precio - pred
ab_res <- abs(res)
prop_res <- res / base_us$Precio
abs_prop_res <- abs(prop_res)

#colours = c("green", "yellow", "orange", "red")
map.resids = SpatialPointsDataFrame(data=data.frame(res,ab_res,prop_res,abs_prop_res),coords=cbind(base_us$coordX,base_us$coordY))
#spplot(map.resids, cuts=quantile(res,na.rm=TRUE), col.regions=colours,cex=0.8)

write.csv2(file="resid_usd_lm1.csv", x=cbind(as.numeric(res,digits=2),as.numeric(ab_res),as.numeric(prop_res),as.numeric(abs_prop_res),base_us$coordX,base_us$coordY),na="",row.names=TRUE)

writeOGR(map.resids, dsn = "C:/Users/sarangof/Documents/Datos Galeria Inmobiliaria/R", layer ='res_us_lm1', driver = 'ESRI Shapefile')

# Base de Nuevos

lm1 <- lm(data=base_nv, Precio ~ Area + Piso_nv + DistCentro + as.numeric(NGar) + as.numeric(Estrato) )
#lm1 <- lm(data=base_us, Precio ~ Area + Estrato + Piso + Banos + DistCentro + NGar)

interc <- tidy(lm1)[1,2]
coefs <- tidy(lm1)[2:6,2]
matriz_reg <- cbind(base_nv$Area, base_nv$Piso_nv , base_nv$DistCentro, as.numeric(base_nv$NGar),as.numeric(base_nv$Estrato))
pred <- rep(interc, length(base_nv[,1])) + matriz_reg %*% coefs

res <- base_nv$Precio - pred
ab_res <- abs(res)
prop_res <- res / base_nv$Precio
abs_prop_res <- abs(prop_res)

#colours = c("green", "yellow", "orange", "red")
map.resids = SpatialPointsDataFrame(data=data.frame(res,ab_res,prop_res,abs_prop_res),coords=cbind(base_nv$coordX,base_nv$coordY))
#spplot(map.resids, cuts=quantile(res,na.rm=TRUE), col.regions=colours,cex=0.8)

write.csv2(file="res_nv_lm1.csv", x=cbind(as.numeric(res,digits=2),as.numeric(ab_res),as.numeric(prop_res),as.numeric(abs_prop_res),base_nv$coordX,base_nv$coordY),na="",row.names=TRUE)

writeOGR(map.resids, dsn = "C:/Users/sarangof/Documents/Datos Galeria Inmobiliaria/R", layer ='rs_nv_lm1', driver = 'ESRI Shapefile')



writeOGR(map.resids, dsn = "C:/Users/sarangof/Documents/Datos Galeria Inmobiliaria/R", layer ='Hola', driver = 'ESRI Shapefile')