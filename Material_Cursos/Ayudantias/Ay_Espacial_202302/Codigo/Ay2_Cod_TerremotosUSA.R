
setwd("/Users/danielsaavedramorales/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Desktop/Doctorado_Estadistica/2013_2/Ayudantia_Espacial/Ayudantias/Ayudantia_2/")


library(tidyverse) |> suppressPackageStartupMessages()
terremotos <- read_csv("Earthquake_all_month.csv")

terremotos <- terremotos %>%
  filter(mag > 0)

library(sf)
# Linking to GEOS 3.11.1, GDAL 3.6.2, PROJ 9.1.1; sf_use_s2() is TRUE
crs <- st_crs("EPSG:32632")
st_as_sf(terremotos, crs = "OGC:CRS84", coords = 
           c("longitude", "latitude")) |>
  st_transform(crs) -> terremotos.sf

read_sf("cb_2017_us_state_20m/cb_2017_us_state_20m.shp") |> st_transform(crs) -> USA
USA <-
  USA %>%
  filter(!(STUSPS %in% c('AK', 'HI', 'PR')))



### 3 Formas de Plotearlo
#1.-Cargar los estados de Alemania


ggplot() +
  geom_sf(data = terremotos.sf, mapping = aes(col = mag))+
  scale_color_viridis_c(option = "plasma")

#2.- Mediante leaflet
library(leaflet) # Libreria para mapas interactivos georefenciados
# Define una paleta de colores
col_pal <- colorFactor(palette = "plasma", domain = terremotos$mag, reverse = TRUE)
legend_values <- c(0,1,2,3,4,5,6)

# Crear el mapa Leaflet con la escala de color
leaflet(terremotos) %>%
  addTiles() %>% 
  addCircles(lng = ~longitude, lat = ~latitude, color = ~col_pal(mag), radius = ~mag * 100)%>%
  addLegend(position = "bottomright", pal = col_pal, values = legend_values, title = "Magnitude")

#3.- MEdiante tmap
library(tmap) # Libreria de mapas georefeciados
terremotos_2 = terremotos
# Primero le damos las coordenadas que tenemos. La función es la siguiente:
# Cramos un objeto del tipo coordinates
library(sp)
coordinates(terremotos_2) = ~longitude + latitude 

# Además se establece el formato de las coordenadas.
proj4string(terremotos_2) <-CRS("+proj=longlat +datum=WGS84")
# CRS: Coordinate referencial system
tmap_leaflet(tm_shape(terremotos_2) +
               tm_dots("mag", style = "cont"))




# Calcula los puntos de A que están cerca de los polígonos de B
terremotosUsa.sf <- st_intersection(terremotos.sf, USA)
tmap_leaflet(tm_shape(terremotosUsa.sf) +
               tm_dots("mag", style = "cont"))

### Modelos Lineales


### Ajuste de modelos
terremotosUsa = st_transform(terremotosUsa.sf, st_crs(4326))
terremotosUsa$latitude= st_coordinates(terremotosUsa)[,2]
terremotosUsa$longitude= st_coordinates(terremotosUsa)[,1]
library(nlme)

# trae una funcón que permite estimar con mínimos cuadrados generalizados.

modelo1 = lm(mag ~ depth, data = terremotosUsa)

summary(modelo1)

modelo1 = gls(mag ~ depth, data = terremotosUsa)
summary(modelo1)

modelo2 = gls(mag ~ depth + longitude + latitude, data = terremotosUsa)
summary(modelo2)

modelo3 <- gls(mag ~ latitude + longitude + depth + gap, data = terremotosUsa)
summary(modelo3)

terremotosUsa_aux <- terremotosUsa %>% 
  select(mag,depth,gap,longitude,latitude) %>% 
  na.omit()

modelo3 <- gls(mag ~ latitude + longitude + depth + gap, data = terremotosUsa_aux)
summary(modelo3)

AIC(modelo1, modelo2, modelo3)


#Creamos una Grilla de USA

library(stars) |> suppressPackageStartupMessages()
st_bbox(USA) |>
  st_as_stars(dx = 10000) |>
  st_crop(USA) -> grd
grd

st_as_sf(terremotosUsa_aux, crs = "OGC:CRS84", coords = 
           c("longitude", "latitude")) |>
  st_transform(crs) -> terremotos.sf_aux

terremotosUSA.sf_aux <- terremotos.sf_aux[ ! st_is_empty( terremotos.sf_aux ) , ]



library(gstat)
i <- idw(mag ~ 1, terremotos.sf_aux, grd)
# [inverse distance weighted interpolation]
ggplot() + geom_stars(data = i, 
                      aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = st_cast(de, "MULTILINESTRING")) + 
  geom_sf(data = terremotos.sf_aux)


V1 = variogram(mag ~ depth, terremotos.sf_aux)
plot(V1,plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(V1$dist)))

V1.fit = fit.variogram(V1, model = vgm(psill=5,model="Exp",range=500000, nugget=1))  
V1.fit
plot(V1,V1.fit, plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(V1$dist)))

V2 = variogram(mag ~ depth, terremotos.sf_aux)
plot(V2,plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(V2$dist)))

V2.fit = fit.variogram(V2, model = vgm(psill=5,model="Sph",range=5000000, nugget=10))  
V2.fit
plot(V2,V2.fit, plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(V2$dist)))


distancias <- spDists(terremotos.sf_aux, longlat = TRUE)
d_max <- max(distancias)
phi <- d_max/3*1000

V3.fit = vgm(psill=25,model="Exp",range=phi, nugget=0)  
V3.fit
plot(V2,V3.fit, plot.numbers = TRUE, xlab = "distance h [m]",
     ylab = expression(gamma(h)),
     xlim = c(0, 1.055 * max(V2$dist)))


k1<- krige(NO2~1, no2.sf, grd, V1.fit)
k2<- krige(NO2~1, no2.sf, grd, V2.fit)
k3<- krige(NO2~1, no2.sf, grd, V3.fit)


ggplot() + geom_stars(data = k1, aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = st_cast(de, "MULTILINESTRING")) + 
  geom_sf(data = no2.sf) +
  coord_sf(lims_method = "geometry_bbox")+
  labs(title = "K1")


ggplot() + geom_stars(data = k2, aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = st_cast(de, "MULTILINESTRING")) + 
  geom_sf(data = no2.sf) +
  coord_sf(lims_method = "geometry_bbox")+
  labs(title = "K2")

ggplot() + geom_stars(data = k3, aes(fill = var1.pred, x = x, y = y)) + 
  xlab(NULL) + ylab(NULL) +
  geom_sf(data = st_cast(de, "MULTILINESTRING")) + 
  geom_sf(data = no2.sf) +
  coord_sf(lims_method = "geometry_bbox")+
  labs(title = "K3")




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
