setwd("/Users/danielsaavedramorales/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Desktop/Doctorado_Estadistica/2013_2/Ayudantia_Espacial/Ayudantias/Ayudantia_2/")

library(tidyverse) |> suppressPackageStartupMessages()
no2 <- read_csv(system.file("external/no2.csv", 
                            package = "gstat"), show_col_types = FALSE)
library(sf)
# Linking to GEOS 3.11.1, GDAL 3.6.2, PROJ 9.1.1; sf_use_s2() is TRUE
crs <- st_crs("EPSG:32632")
st_as_sf(no2, crs = "OGC:CRS84", coords = 
           c("station_longitude_deg", "station_latitude_deg")) |>
  st_transform(crs) -> no2.sf



### 3 Formas de Plotearlo
#1.-Cargar los estados de Alemania
read_sf("de_nuts1.gpkg") |> st_transform(crs) -> de


ggplot() + geom_sf(data = de) + 
  geom_sf(data = no2.sf, mapping = aes(col = NO2))+
  scale_color_viridis_c(option = "plasma")

#2.- Mediante leaflet
library(leaflet) # Libreria para mapas interactivos georefenciados
# Define una paleta de colores
col_pal <- colorFactor(palette = "plasma", domain = no2$NO2, reverse = TRUE)
#legend_values <- c(0,1,2,3,4,5,6)

# Crear el mapa Leaflet con la escala de color
leaflet(no2) %>%
  addTiles() %>% 
  addCircles(lng = ~station_longitude_deg, lat = ~station_latitude_deg, color = ~col_pal(NO2), radius = ~NO2 * 100)
 # addLegend(position = "bottomright", pal = col_pal, values = legend_values, title = "Magnitude")

#3.- MEdiante tmap
library(tmap) # Libreria de mapas georefeciados
library(sp)
no2_2 = no2
# Primero le damos las coordenadas que tenemos. La función es la siguiente:
# Cramos un objeto del tipo coordinates
coordinates(no2_2) = ~station_longitude_deg + station_latitude_deg 

# Además se establece el formato de las coordenadas.
proj4string(no2_2) <-CRS("+proj=longlat +datum=WGS84")
# CRS: Coordinate referencial system
tmap_leaflet(tm_shape(no2_2) +
               tm_dots("NO2", style = "cont"))

### Modelos Lineales


### Ajuste de modelos

library(nlme)

# trae una funcón que permite estimar con mínimos cuadrados generalizados.

modelo1 = lm(NO2 ~ 1, data = no2)

summary(modelo1)

modelo1 = gls(NO2 ~ 1, data = no2)
summary(modelo1)

modelo2 = gls(NO2 ~ station_altitude, data = no2)
summary(modelo2)

modelo3 <- gls(NO2 ~ station_altitude + station_longitude_deg + station_latitude_deg , data = no2)
summary(modelo3)

AIC(modelo1, modelo2, modelo3)
# ¿esto implica que las coordenadas espaciales  son útiles para modelar?
# Esto no quiere decir que haya una especie de correlación espacial.

# Aunque no fuesen significativas, no puede concluirse que hay o no correlación espacial por esto.
# Son cosas distintas.

# pAra incluir las localizaciones en general vamos a utilizar la matriz de correlación.

# La función gls trae el argumento correlation, al cual se le agregan los modelos de correlación.



#Creamos una Grilla
library(stars) |> suppressPackageStartupMessages()
st_bbox(de) |>
  st_as_stars(dx = 10000) |>
  st_crop(de) -> grd
grd


library(gstat)
i <- idw(NO2~1, no2.sf, grd)
# [inverse distance weighted interpolation]
ggplot() + geom_stars(data = i, 
                      aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = st_cast(de, "MULTILINESTRING")) + 
  geom_sf(data = no2.sf)


V1 = variogram(NO2~1, no2.sf)
plot(V1,plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(V1$dist)))

V1.fit = fit.variogram(V1, model = vgm(psill=1,model="Exp",range=100000, nugget=1))  
V1.fit
plot(V1,V1.fit, plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(V1$dist)))

V2 = variogram(NO2~1, no2.sf)
plot(V2,plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(V2$dist)))

V2.fit = fit.variogram(V2, model = vgm(psill=1,model="Sph",range=100000, nugget=1))  
V2.fit
plot(V2,V2.fit, plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(V2$dist)))


distancias <- spDists(no2_2, longlat = TRUE)
d_max <- max(distancias)
phi <- d_max/3*1000

V3.fit = vgm(psill=25,model="Exp",range=phi, nugget=0)  
V3.fit
plot(V2,V3.fit, plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(V2$dist)))

V4.fit = fit.variogram(V2, model = vgm(psill=1,model="Mat",range=100000, nugget=1))   
V4.fit
plot(V2,V4.fit, plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(V2$dist)))


k1<- krige(NO2~1, no2.sf, grd, V1.fit)
k2<- krige(NO2~1, no2.sf, grd, V2.fit)
k3<- krige(NO2~1, no2.sf, grd, V3.fit)
k4<- krige(NO2~1, no2.sf, grd, V4.fit)


ggplot() + geom_stars(data = k1, aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = st_cast(de, "MULTILINESTRING")) + 
  geom_sf(data = no2.sf) +
  coord_sf(lims_method = "geometry_bbox")+
  labs(title = "K1 Exp Nugget = 0, Psill = 16.25, Range = 52345")


ggplot() + geom_stars(data = k2, aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = st_cast(de, "MULTILINESTRING")) + 
  geom_sf(data = no2.sf) +
  coord_sf(lims_method = "geometry_bbox")+
  labs(title = "K2 Sph Nugget = 0, Psill = 14.48, Range = 87596")

ggplot() + geom_stars(data = k3, aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = st_cast(de, "MULTILINESTRING")) + 
  geom_sf(data = no2.sf) +
  coord_sf(lims_method = "geometry_bbox")+
  labs(title = "K3 Exp Nugget = 0, Psill = 25, Range = 278951 (5%)")

ggplot() + geom_stars(data = k4, aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = st_cast(de, "MULTILINESTRING")) + 
  geom_sf(data = no2.sf) +
  coord_sf(lims_method = "geometry_bbox")+
  labs(title = "K4 Mat Nugget = 0, Psill = 16.25, Range = 52345 Kappa 0.5")




a <- aggregate(no2.sf["NO2"], by = de, FUN = mean)
b1 <- krige(NO2~1, no2.sf, de,  V1.fit)
b2 <- krige(NO2~1, no2.sf, de,  V2.fit)

b1$sample <- a$NO2
b1$kriging <- b1$var1.pred

b2$sample <- a$NO2
b2$kriging <- b1$var1.pred


b1 |> select(sample, kriging) |> 
  pivot_longer(1:2, names_to = "var", values_to = "NO2") -> c1
c1$var <- factor(c1$var, levels = c("sample", "kriging"))
ggplot() + 
  geom_sf(data = c1, mapping = aes(fill = NO2)) + 
  geom_sf(data = no2.sf, mapping = aes(col = NO2))+
  facet_wrap(~var) +
  scale_fill_gradientn(colors = sf.colors(25))+
  scale_color_viridis_c(option = "plasma")
  


b2 |> select(sample, kriging) |> 
  pivot_longer(1:2, names_to = "var", values_to = "NO2") -> c2
c2$var <- factor(c2$var, levels = c("sample", "kriging"))
ggplot() + geom_sf(data = c2, mapping = aes(fill = NO2)) + facet_wrap(~var) +
  scale_fill_gradientn(colors = sf.colors(20))


SE <- function(x) sqrt(var(x)/length(x))
a <- aggregate(no2.sf["NO2"], de, SE)
b1$sample <- a$NO2
b1$kriging <- sqrt(b1$var1.var)
b1 |> select(sample, kriging) |> 
  pivot_longer(1:2, names_to = "var", 
               values_to = "Standard_error") -> b2
b2$var <- factor(b2$var, levels = c("sample", "kriging"))
ggplot() +
  geom_sf(data = b2, mapping = aes(fill = Standard_error)) +
  facet_wrap(~var, as.table = FALSE) + 
  scale_fill_gradientn(colors = sf.colors(20))

