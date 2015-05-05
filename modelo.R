library(sp)
library(lattice)

map = SpatialPointsDataFrame(data=base_us, coords=cbind(base_us$coordX,base_us$coordY))
colours = c("light blue", "blue", "dark blue", "black")
spplot(map, "Area", cuts=quantile(base_us$Area,na.rm=TRUE), col.regions=colours,cex=0.6)



library(spgwr)

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


#USADA
#Piso, Area, Estrato en vivienda usada

prueba <- base_us[complete.cases(cbind(base_us$Precio,base_us$Area, as.numeric(base_us$Estrato), base_us$Piso, as.numeric(base_us$Baños), base_us$NGar,base_us$coordX,base_us$coordY)),]

#prueba <- base_us[complete.cases(cbind(base_us$Precio,base_us$Area,base_us$Estrato)),]
map = SpatialPointsDataFrame(data=prueba, coords=cbind(prueba$coordX,prueba$coordY))

bw = gwr.sel(Precio~Area   , data=map, adapt=T) #+ as.numeric(Estrato) + Piso + as.numeric(Baños) + NGar
#bw = gwr.sel(Precio~Area, data=base_us, coords=cbind(base_us$coordX,base_us$coordY),adapt=FALSE)
gwr.model = gwr(Precio~Area + as.numeric(Estrato) + Piso + as.numeric(Baños) + NGar , data=map, adapt=bw)
gwr.model
map = SpatialPointsDataFrame(data=as.data.frame(gwr.model$SDF), coords=cbind(prueba$coordX,prueba$coordY))
spplot(map, "Area", cuts=quantile(map$Area,na.rm=TRUE), col.regions=colours,cex=0.6)

lm2 <- lm(prueba$Precio ~ prueba$Area + prueba$Piso + prueba$Estrato)



#NUEVA
#Piso, Area, Estrato en vivienda nueva

lm1 <- lm(data=base_nv, Precio ~ Area + Piso_nv + DistCentro + as.numeric(NGar) + as.numeric(Estrato) )

prueba <- base_nv[complete.cases(cbind(base_nv$Poct14,base_nv$Area)),]
map = SpatialPointsDataFrame(data=prueba, coords=cbind(prueba$coordX,prueba$coordY))
bw = gwr.sel(Precio~Area, data=map, adapt=T)
#bw = gwr.sel(Precio~Area, data=base_us, coords=cbind(base_us$coordX,base_us$coordY),adapt=FALSE)
gwr.model = gwr(Poct14~Area, data=map, adapt=bw)
gwr.model
map = SpatialPointsDataFrame(data=as.data.frame(gwr.model$SDF), coords=cbind(prueba$coordX,prueba$coordY))
spplot(map, "Area", cuts=quantile(map$Area,na.rm=TRUE), col.regions=colours,cex=0.6)

lm2 <- lm(prueba$Precio ~ prueba$Area + prueba$Piso + prueba$Estrato)


lm1 <- lm(data=base_nv, Precio ~ Area + Piso_nv + DistCentro + as.numeric(NGar) + as.numeric(Estrato) )

prueba <- base_nv[complete.cases(cbind(base_nv$Poct14,base_nv$Area)),]
map = SpatialPointsDataFrame(data=prueba, coords=cbind(prueba$coordX,prueba$coordY))
bw = gwr.sel(Precio~Area, data=map, adapt=T)
#bw = gwr.sel(Precio~Area, data=base_us, coords=cbind(base_us$coordX,base_us$coordY),adapt=FALSE)
gwr.model = gwr(Poct14~Area, data=map, adapt=bw)
gwr.model
map = SpatialPointsDataFrame(data=as.data.frame(gwr.model$SDF), coords=cbind(prueba$coordX,prueba$coordY))
spplot(map, "Area", cuts=quantile(map$Area,na.rm=TRUE), col.regions=colours,cex=0.6)


prueba <- base_us[complete.cases(cbind(base_us$Precio,base_us$Area,as.numeric(base_us$NGar),as.numeric(base_us$Estrato),as.numeric(base_us$Year))),]
lm2 <- lm(Precio~Area + as.numeric(NGar) + as.numeric(Estrato) + as.numeric(Year),data=prueba)




#Regresión completa

prueba <- base_us[complete.cases(cbind(base_nv$Poct14,base_nv$Area)),]
prueba <- base_us[complete.cases(cbind(base_us$Precio,base_us$Area,base_us$Estrato)),]
map = SpatialPointsDataFrame(data=prueba, coords=cbind(prueba$coordX,prueba$coordY))
bw = gwr.sel(Precio~Area + Estrato, data=map, adapt=T)
#bw = gwr.sel(Precio~Area, data=base_us, coords=cbind(base_us$coordX,base_us$coordY),adapt=FALSE)
gwr.model = gwr(Precio~Area + Estrato, data=map, adapt=bw)
gwr.model


prueba <- base_us[complete.cases(cbind(base_us$Precio,base_us$Area,base_us$Estrato,base_us$Piso)),]
map = SpatialPointsDataFrame(data=prueba, coords=cbind(prueba$coordX,prueba$coordY))
bw = gwr.sel(Precio~Area + Estrato + Piso, data=map, adapt=T)
#bw = gwr.sel(Precio~Area, data=base_us, coords=cbind(base_us$coordX,base_us$coordY),adapt=FALSE)
gwr.model = gwr(Precio~Area + Estrato + Piso, data=map, adapt=bw)

prueba <- base_us[complete.cases(cbind(base_us$Precio,base_us$Area,base_us$Estrato,base_us$Piso,base_us$Baños)),]
map = SpatialPointsDataFrame(data=prueba, coords=cbind(prueba$coordX,prueba$coordY))
bw = gwr.sel(Precio~Area + Estrato + Piso + Baños, data=map, adapt=T)
#bw = gwr.sel(Precio~Area, data=base_us, coords=cbind(base_us$coordX,base_us$coordY),adapt=FALSE)
gwr.model = gwr(Precio~Area + Estrato + Piso + Baños, data=map, adapt=bw)

prueba <- base_us[complete.cases(cbind(base_us$Precio,base_us$Area,base_us$Estrato,base_us$Piso,base_us$Baños,base_us$Tipo)),]
map = SpatialPointsDataFrame(data=prueba, coords=cbind(prueba$coordX,prueba$coordY))
bw = gwr.sel(Precio~Area + Estrato + Piso + Baños + Tipo, data=map, adapt=T)
#bw = gwr.sel(Precio~Area, data=base_us, coords=cbind(base_us$coordX,base_us$coordY),adapt=FALSE)
gwr.model = gwr(Precio~Area + Estrato + Piso + Baños + Tipo, data=map, adapt=bw)


map = SpatialPointsDataFrame(data=as.data.frame(gwr.model$SDF), coords=cbind(prueba$coordX,prueba$coordY))
spplot(map, "Area", cuts=quantile(map$Area,na.rm=TRUE), col.regions=colours,cex=0.6)

spplot(gwr.model$SDF, "Area", cuts=quantile(gwr.model$SDF$Area),col.regions=colours, cex=0.7)

# Do data change over space?

library(ggplot2)
ggplot(base_us, aes(x = Area, y = Precio, color = Estrato),main="Precio vs. area por estrato") + 
  geom_point() + geom_smooth(method = "lm")

ggplot(base_nv, aes(x = Area, y = Poct14, color = Estrato),main="Precio vs. area por estrato") + 
  geom_point() + geom_smooth(method = "lm")

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
