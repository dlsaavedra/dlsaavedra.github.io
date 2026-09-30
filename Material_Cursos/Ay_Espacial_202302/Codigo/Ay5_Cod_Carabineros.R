setwd("/Users/danielsaavedramorales/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Desktop/Doctorado_Estadistica/2013_2/Ayudantia_Espacial/Ayudantias/Ayudantia_5/")
## https://www.ide.cl/index.php/instalaciones-y-edificaciones    Portal Datos 


# Viejas librer?as
library(sf)
library(tidyverse); library(mapview)
# Nuevas librer?as
library(spdep)
library(spatstat); ## Utilizada en patrones de puntos

Datos_Carabineros = st_read("Cuarteles_Carabineros//")
Chile = st_read("../Ayudantia_1/Comunas/", "comunas")
Santiago_Provincia = Chile %>% dplyr::filter( Region == "Región Metropolitana de Santiago")
#Santiago_Provincia = Chile %>% dplyr::filter( Provincia  == "Santiago")#%>%filter(!(Comuna %in% c("Lo Barnechea")))


Santiago_Provincia <- st_transform(Santiago_Provincia, crs = st_crs(Datos_Carabineros))
Datos_Santiago_Provincia_Carabineros <- st_intersection(Datos_Carabineros,
                                                        Santiago_Provincia)
Datos_Santiago_Provincia_Carabineros[Datos_Santiago_Provincia_Carabineros$TIPO_DE_UN == "COMISARÍA",]$TIPO_DE_UN = "COMISARIA"
mapview(Datos_Santiago_Provincia_Carabineros, popup = TRUE, zcol = "TIPO_DE_UN")

#Op2
Datos_Santiago_Provincia_Carabineros %>%
  ggplot(aes(color = factor(TIPO_DE_UN), size = 1)) +
  geom_sf() +
  scale_fill_viridis_c(option = "plasma") +
  theme(legend.position = "right",
        strip.background = element_blank())


datos_sf <- st_transform(Datos_Santiago_Provincia_Carabineros, crs = st_crs("+init=EPSG:32719"))
datos_sf <- datos_sf[, c("TIPO_DE_UN", "geometry")]
Santiago_Centro_poly <- st_transform(Santiago_Provincia, crs = st_crs("+init=EPSG:32719"))
Santiago_Centro_poly <- Santiago_Centro_poly[, c("geometry")]

ppp_data <- as.ppp(datos_sf)
Santiago_Centro_poly_ppp <- as.owin(Santiago_Centro_poly)
plot(ppp_data)





## A?adamos el mapa al plot de los Atroplleos en santiago

par(mfrow = c(1,1))
Window(ppp_data) = Santiago_Centro_poly_ppp
plot(ppp_data)

par(mfrow = c(1,2))
ppp_data$marks = factor(ppp_data$marks)
plot(ppp_data$marks)
plot(split(ppp_data))

par(mfrow = c(1,1))
plot(split(ppp_data)$"COMISARIA")


?Gest
ppp_dataG = Gest(ppp_data, correction = "all")
plot(ppp_dataG) # Notar que hay una gran diferencia entre te?rico y emp?rico
# Podemos escoger una "correccion" para atarcar diferentes problematicas.
# Se recomienda usar correction = "all". Grafica todas las G corregidas segun
# las G te?ricas.

# Esto es como la funcion de distribuci?n de una poisson con cierto parametro lambda
# G(r) = 1 - exp(-lambda*pi*r^2)

# El valor real o te?rico se ve color celeste. El resto de valores
# son los emp?ricos.

# para establecer que hay aleatoriedad en los datos debemos esperar que
# las curvas escalonadas se parezcan a la te?rica.

# Mediante experimentos de montecarlo creamos una banda de confianza que nos
# permita probar mediante un intervalo de credibilidad si las curvas
# caen o no dentro de la curva

# Para realizar esto utilizamos la funcion envelope(). Se permutar?n los datos
# y se calcular?n los estad?sticos.bootstrap

?envelope
n = 100
p = 0.05

ppp_dataG_mc = envelope(ppp_data, fun = "Gest",
                        nsim = n, rank = p*n + 1)

plot(ppp_dataG_mc)


### Estimador F


?Fest
ppp_dataF = Fest(ppp_data, correction = "all")
plot(ppp_dataF) 

ppp_dataF_mc = envelope(ppp_data, fun = "Fest",
                        nsim = n, rank = p*n + 1)

plot(ppp_dataF_mc)


#### Estimador J

?Jest
ppp_dataJ = Jest(ppp_data, correction = "all")
plot(ppp_dataJ) 

ppp_dataJ_mc = envelope(ppp_data, fun = "Jest",
                        nsim = n, rank = p*n + 1)

plot(ppp_dataJ_mc, ylim = c(0,5))



## An?lisis de aleatoriedad espacial completa
# Calculemos las estadisticas K y L

?Kest

ppp_dataK = Kest(ppp_data, correction = "all")
plot(ppp_dataK) 

ppp_dataK_mc = envelope(ppp_data, fun = "Kest",
                        nsim = n, rank = p*n + 1)

plot(ppp_dataK_mc)



?Lest

ppp_dataL = Lest(ppp_data, correction = "all")
plot(ppp_dataL) 


ppp_dataL_mc = envelope(ppp_data, fun = "Lest",
                        nsim = n, rank = p*n + 1)

plot(ppp_dataL_mc)


#### Función Densidad
?density
## Graficar densidad
plot(density(ppp_data, sigma = 1000, kernel = "gaussian"))

par(mfrow = c(2,2))
plot(density(ppp_data, sigma = 1000, kernel = "gaussian"))
plot(density(ppp_data, sigma = 1000, kernel = "epanechnikov"))
plot(density(ppp_data, sigma = 1000, kernel = "quartic"))
plot(density(ppp_data, sigma = 1000, kernel = "disc"))

?bw.diggle
###
par(mfrow = c(1,1))
b <- bw.diggle(ppp_data);b
plot(b, ylim=c(-5, 5), main="Cross validation for hickories")
par(mfrow = c(1,2))
plot(density(ppp_data, sigma = b, kernel = "gaussian"))
plot(density(ppp_data, b, kernel = "gaussian"))

par(mfrow = c(2,2))
plot(density(ppp_data, sigma = b, kernel = "gaussian"))
plot(density(ppp_data, sigma = b, kernel = "epanechnikov"))
plot(density(ppp_data, sigma = b, kernel = "quartic"))
plot(density(ppp_data, sigma = b, kernel = "disc"))


### Kernels
url = "https://i.stack.imgur.com/Huf4o.png"
ggplot() +
  annotate(
    ggpath::GeomFromPath,
    x = 0,
    y = 0,
    path = url,
    width = 1
  ) +
  theme_minimal()


par(mfrow = c(1,1))
## 3D
persp(density(ppp_data, sigma = b, kernel = "gaussian"), theta = 100, phi = 10)

library(plotly)
# volcano is a numeric matrix that ships with R
D = density(ppp_data, sigma = b, kernel = "gaussian")$v
fig <- plot_ly(z = ~D)
fig <- fig %>% add_surface()
fig


