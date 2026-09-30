#Ayudantia N°2 Estadística Espacial
# Datos de Temblores: https://earthquake.usgs.gov/earthquakes/feed/v1.0/csv.php

library(tidyverse)
library(dplyr)
library(sf) 
library(sp) # coordinates
library(leaflet) # Libreria para mapas interactivos georefenciados
#library(mapview)
library(readr) # Leer Csv
library(tmap) # Libreria de mapas georefeciados

setwd("/Users/danielsaavedramorales/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Desktop/Doctorado_Estadistica/2013_2/Ayudantia_Espacial/Ayudantias/Ayudantia_2/")
 
terremotos <- read_csv("Earthquake_all_month.csv")

terremotos <- terremotos %>%
  filter(mag > 0)

# Define una paleta de colores
col_pal <- colorFactor(palette = "plasma", domain = terremotos$mag, reverse = TRUE)
legend_values <- c(0,1,2,3,4,5,6)

# Crear el mapa Leaflet con la escala de color
leaflet(terremotos) %>%
  addTiles() %>% 
  addCircles(lng = ~longitude, lat = ~latitude, color = ~col_pal(mag), radius = ~mag * 10000) %>%
  addLegend(position = "bottomright", pal = col_pal, values = legend_values, title = "Magnitude")


terremotos2 = terremotos
# Primero le damos las coordenadas que tenemos. La función es la siguiente:
# Cramos un objeto del tipo coordinates
coordinates(terremotos2) = ~longitude + latitude 

# Además se establece el formato de las coordenadas.

proj4string(terremotos2) <-CRS("+proj=longlat +datum=WGS84")
# CRS: Coordinate referencial system

tmap_leaflet(tm_shape(terremotos2) +
               tm_dots("mag", style = "cont"))



### Ajuste de modelos

library(nlme)

# trae una funcón que permite estimar con mínimos cuadrados generalizados.

modelo1 = lm(mag ~ depth, data = terremotos)

summary(modelo1)

modelo1 = gls(mag ~ depth, data = terremotos)
summary(modelo1)

modelo2 = gls(mag ~ depth + longitude + latitude, data = terremotos)
summary(modelo2)

modelo3 <- gls(mag ~ latitude + longitude + depth + gap, data = terremotos)
summary(modelo3)

terremotos_aux <- terremotos %>% 
  select(mag,depth,gap,longitude,latitude) %>% 
  na.omit()

modelo3 <- gls(mag ~ latitude + longitude + depth + gap, data = terremotos_aux)
summary(modelo3)

AIC(modelo1, modelo2, modelo3)
#gap Data Type Decimal Typical Values [0.0, 180.0]
#Description
#The largest azimuthal gap between azimuthally adjacent stations (in degrees). 
#In general, the smaller this number, the more reliable is the calculated horizontal position of the earthquake. 
#Earthquake locations in which the azimuthal gap exceeds 180 degrees typically have large location and depth uncertainties.


# ¿esto implica que las coordenadas espaciales  son útiles para modelar?
# Esto no quiere decir que haya una especie de correlación espacial.

# Aunque no fuesen significativas, no puede concluirse que hay o no correlación espacial por esto.
# Son cosas distintas.

# pAra incluir las localizaciones en general vamos a utilizar la matriz de correlación.

# La función gls trae el argumento correlation, al cual se le agregan los modelos de correlación.

# Modelo con coordenadas esféricas asociadas
terremotos3 <- terremotos %>% 
            transmute(mag,depth,gap, x = 6371*sin((latitude + 90)*pi/180)*cos(longitude*pi/180), 
            y = 6371*sin(longitude*pi/180)*sin((latitude + 90)*pi/180), 
            z = 6371*cos((longitude + 90)*pi/180)) %>% 
            na.omit()

modelo4 <- gls(mag ~ x + y + z+ depth + gap, data = terremotos3)
summary(modelo4)
AIC(modelo1, modelo2, modelo3, modelo4)

library(gstat)

terremotos4 = terremotos3
coordinates(terremotos4) = ~x + y + z
# no trend:

proj4string(terremotos4) <-CRS("+proj=longlat +datum=WGS84")
# CRS: Coordinate referencial system

tmap_leaflet(tm_shape(terremotos4) +
               tm_dots("mag", style = "cont"))

#
# 
library(dplyr)

coords <- terremotos3 %>% 
  select(x,y,z)
distancias <- dist(coords)
d_max <- max(distancias)
phi <- -d_max/log(0.05)


V1 = variogram(mag~depth,data =  terremotos4)
plot(V1, plot.numbers= TRUE)
V1.fit = fit.variogram(V1, model = vgm(psill=2,model="Exp",range=1000, nugget=1))  
V1.fit
V1.vgm = vgm(psill=V1.fit$psill[2],model="Exp",range=V1.fit$range[2], nugget=V1.fit$psill[1])
plot(V1,V1.fit, xlim=c(0, phi), ylim=c(0, 2.6))

V2 = variogram(mag~depth,data =  terremotos4)
plot(V2)
V2.fit = fit.variogram(V2, model = vgm(psill=2,model="Gau",range=1000, nugget=1))  
V2.fit
V2.vgm = vgm(psill=V2.fit$psill[2],model="Gau",range=V2.fit$range[2], nugget=V2.fit$psill[1])
plot(V2,V2.fit, xlim=c(0, phi), ylim=c(0, 2.6))


#Interpolación kriging
# First, make a rectangular grid over your `SpatialPolygonsDataFrame`
grd <- makegrid(terremotos2, n = 1000)
colnames(grd) <- c("Longitude", "Latitude")


capitals <- read_csv("country-capital-lat-long-population.csv")
grd2 <- grd %>% 
  transmute( x = 6371*sin((Latitude + 90)*pi/180)*cos(Longitude*pi/180), 
            y = 6371*sin(Longitude*pi/180)*sin((Latitude + 90)*pi/180), 
            z = 6371*cos((Longitude + 90)*pi/180)) %>% 
  na.omit()

gridded(grd2) = ~x + y + z


interpolacion_kriging <- krige(mag~depth, terremotos4 , grd2, model = V2.vgm)

# Muestra los resultados del modelo
ibrary(sp)
data(meuse)
coordinates(meuse) = ~x+y
data(meuse.grid)
gridded(meuse.grid) = ~x+y
m <- vgm(.59, "Sph", 874, .04)
# ordinary kriging:
x <- krige(log(zinc)~1, meuse, meuse.grid, model = m)
spplot(x["var1.pred"], main = "ordinary kriging predictions")
spplot(x["var1.var"],  main = "ordinary kriging variance")
# simple kriging:
x <- krige(log(zinc)~1, meuse, meuse.grid, model = m, beta = 5.9)
# residual variogram:
m <- vgm(.4, "Sph", 954, .06)
# universal block kriging:
x <- krige(log(zinc)~x+y, meuse, meuse.grid, model = m, block = c(40,40))
spplot(x["var1.pred"], main = "universal kriging predictions")
