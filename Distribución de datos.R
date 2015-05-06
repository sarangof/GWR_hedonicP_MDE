library(ggmap)
library(mapproj)

grf <- read.csv("Direcciones georeferenciadas.csv")
geo <- grf[which(grf$DESCRIPTION != "No_localizada" & grf$DESCRIPTION != "No_estandarizada"),]


map <- get_map(
     location = "Medellin" # google search string
     , zoom = 13 # larger is closer
     , maptype = "hybrid" # map type hybrid, watercolor
 )

p <- ggmap(map)
p <- p + labs(title = "Mapa de Medellin")
print(p)

p <- p + geom_point(data = geo, aes(x = X, y = Y), size = 1,col="blue") #hjust = -0.2

# legend positioning, removing grid and axis labeling
p <- p + theme( legend.position = "none" # remove legend
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.text = element_blank(),
                axis.title = element_blank(),
                axis.ticks = element_blank()
                )
p <- p + labs(title = "Distribucion de datos existentes")
print(p)
