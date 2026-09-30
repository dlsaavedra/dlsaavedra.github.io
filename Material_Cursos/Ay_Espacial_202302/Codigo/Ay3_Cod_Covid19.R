setwd("/Users/danielsaavedramorales/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Desktop/Doctorado_Estadistica/2013_2/Ayudantia_Espacial/Ayudantias/Ayudantia_3/")

# Viejas librer?as
library(tidyverse);library(leaflet); library(mapview)
# Nuevas librer?as
library(spdep); library(gstat)

library(RCurl)

#Chile = st_read("C:/Users/Trinkpad L480/Desktop/Hernan/2020-2/Estadistica Espacial/Ayudantias/Ayudantia1/Comunas", "comunas")
Chile = st_read("../Ayudantia_1/Comunas/", "comunas")
Santiago = Chile %>% dplyr::filter( Region == "Región Metropolitana de Santiago"  )

### Extraemos los datos de casos covid acumulados en el tiempo
url = getURL("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto1/Covid-19.csv")
Datos_covid = read_csv(url)

head(Datos_covid)

Datos_covid_RM = Datos_covid %>% filter(Region == "Metropolitana") %>%
  rename(cod_comuna = `Codigo comuna`) %>%
  mutate(cod_comuna = as.numeric(cod_comuna))

# Calcular la suma de las columnas desde la 4 en adelante
Datos_covid_RM_final <- Datos_covid_RM %>%
  rename(Acumulado = `09-01-2023`) %>%
  mutate("Tasa_prop" = Acumulado/Poblacion * 100000)%>%
  select(c(1,2,3,4,5), Acumulado,"Tasa_prop" ,"Tasa")

#Juntamos los datos de las comunas con los casos covid
Data_final = Santiago %>%
  left_join(Datos_covid_RM_final, by = "cod_comuna")

head(Data_final)


### Opciones de visualización ###
# Op1
mapview(Data_final["Tasa_prop"])

#Op2
Data_final %>%
  ggplot(aes(fill = Tasa_prop)) +
  geom_sf() +
  scale_fill_viridis_c(option = "plasma") +
  theme(legend.position = "right",
        strip.background = element_blank()) +
  labs(fill = "Tasa de contagio ",
       title = "Tasa de contagio cada 100.000 habitantes")




## Generación de Vecinos de cada área------

library(spdep)
library(sp)
Data_final_sp = as_Spatial(Data_final)
Data_final_sp$Comuna.x # La posicion del vector es la identificacion
# Cada vez que querramos usar funciones de liberias sp__ debemos asegurarnos
# de que el objeto sea tipo as_Spatial

## Primero genreamos los vecinos
help(poly2nb)
vecinos1 = poly2nb(Data_final_sp) 
# Toma un objeto que tenga poligonos y lo transforma en un objeto de tipo vecinos
vecinos1
summary(vecinos1)
# Link number distribution es cu?ntas comunas hay con X vecinos.

plot(Data_final_sp)
plot(vecinos1, coordinates(Data_final_sp), add = TRUE)


vecinos2 = poly2nb(Data_final_sp, queen = F)
vecinos2
# queen	
#if TRUE, a single shared boundary point meets the contiguity condition, if FALSE,
#more than one shared point is required; note that more than one shared boundary point does not necessarily mean a shared boundary line
plot(Data_final_sp)
plot(vecinos2, coordinates(Data_final_sp), add = TRUE)


### Vecinos por K vecinos m?s cercanos (KNN) -----
help(knearneigh)
vecinos3 = knn2nb(knearneigh(coordinates(Data_final_sp), k = 2)) # k = #vecinos
vecinos3
plot(Data_final_sp)
plot(vecinos3, coordinates(Data_final_sp), add = TRUE)


## Calculamos la matriz  de Adyacencia W  

# nb2listw recibe los vecinos y el style
help(nb2listw)
# Style = W Filas estandarizadas
W_1_w = nb2listw(vecinos1, style = "W")
W_1_w$weights
# Matriz de pesos:
matrixW_1_w = listw2mat(W_1_w)
matrixW_1_w

## Matriz binaria
W_1_b = nb2listw(vecinos1, style = "B")
matrixW_1_b = listw2mat(W_1_b)
matrixW_1_b


### Pesos según distancia

# Calcualmos distancias:
dists = nbdists(vecinos1, coordinates(Data_final_sp))
# Calcula las distancias de las comunas a cada uno de sus vecinos.
inverse_dists = lapply(dists, function(x) 1/x)
W_1_d = nb2listw(vecinos1, glist= inverse_dists, style = "W")
matrixW_1_d = listw2mat(W_1_d)
matrixW_1_d


### Vecinos de orden superior
help(nblag)
vecinos3_lags <- nblag(vecinos3, maxlag=3)
names(vecinos3_lags) <- c("first", "second", "third")
res <- sapply(vecinos3_lags, function(x) table(card(x)))
mx <- max(unlist(sapply(vecinos3_lags, function(x) card(x))))
nn <- length(vecinos3_lags)
res1 <- matrix(0, ncol=(mx+1), nrow=nn)
rownames(res1) <- names(res)
colnames(res1) <- as.character(0:mx)
for (i in 1:nn) res1[i, names(res[[i]])] <- res[[i]]
res1


### Pruebas de correlación espacial para tados areales ------
#### I de Moran -----
help(moran)
moran.test(Data_final_sp$Tasa_prop, listw = W_1_w)

# Sus valores van entre -1 y 1. 
# Estadistico cercano a 1 -> Corr espacial positiva
# Estadistico cercano a -1 -> Corr espacial negativa
# Estadistico cercano a 0 -> Sin Corr espacial

# Como Moran = -0.036881693 , se  podr?a decir que no hay correlacion espcial.
# Si hay vecinos, estos tendrín valor parecido.
# H0: NO existe correlacIÓn espacial -> p value = 0.5853 -> NO Hay para descartar correlaci?n espacial

# Prueba no param?trica de la prueba de MOran -----

# Consiste en hacer las permutaciones
moran.mc(Data_final_sp$Tasa_prop, listw = W_1_w, nsim = 1000)

plot(moran.mc(Data_final_sp$Tasa, listw = W_1_w, nsim = 1000))



#### C de Gearys ------

# Los valores del estad?stico van entre 0 y 2, y se interpreta al rev?s
# que la correlaci?n usual.

# Correlaci?n cercana a 0  -> COrrelaci?n positiva
# Correlaci?n cercana a 2 -> Correlaci?n negativa
# Correlaci?n cercana a 1 -> Sin correlaci?n
help(geary)
geary.test(Data_final_sp$Tasa_prop, listw = W_1_w)
# Consiste en hacer las permutaciones
geary.mc(Data_final_sp$Tasa_prop, listw = W_1_w, nsim = 1000)
plot(geary.mc(Data_final_sp$Tasa, listw = W_1_w, nsim = 1000))


# Nuevamente valor-p Alto. No existe evidencia para descartar la no correlaci?n espacial (iid)


### Importante: La elecci?n de llos W cambia la prueba de hip?tesis.

moran.mc(Data_final_sp$Tasa_prop, listw = W_1_w, nsim = 1000)
moran.mc(Data_final_sp$Tasa_prop, listw = W_1_b, nsim = 1000)
moran.mc(Data_final_sp$Tasa_prop, listw = W_1_d, nsim = 1000)

geary.mc(Data_final_sp$Tasa_prop, listw = W_1_w, nsim = 1000)
geary.mc(Data_final_sp$Tasa_prop, listw = W_1_b, nsim = 1000)
geary.mc(Data_final_sp$Tasa_prop, listw = W_1_d, nsim = 1000)

#### Crear los correlogramas (Coeficientes (Moran/Geary) con vecinos de orden superior)

moran_corr <- sp.correlogram(W_1_w$neighbours, Data_final_sp$Tasa_prop, order=5, method="I",
                       zero.policy=TRUE)
print(moran_corr)
plot(moran_corr)

geary_corr <- sp.correlogram(W_1_w$neighbours, Data_final_sp$Tasa_prop, order=5, method="C",
                       zero.policy=TRUE)
print(geary_corr)
plot(geary_corr)


############
#### Crear Moran local
# creates a local moran output

help(localmoran)
moran_local <- localmoran(x = Data_final_sp$Tasa_prop,
                    listw = nb2listw(vecinos1, style = "W"))
#
#Name     Description
#Ii       local moran statistic
#E.Ii     expectation of local moran statistic
#Var.Ii   variance of local moran statistic
#Z.Ii     standard deviate of local moran statistic
#Pr()     p-value of local moran statistic

# maps the results
library("tmap")
tm_shape(cbind(Data_final_sp, moran_local)) + tm_fill(col = "Ii", style = "quantile",
                              title = "local moran statistic")

mapview(cbind(Data_final_sp, moran_local)["Ii"])


#### Crear geary local

geary_local <- data.frame("Ii" = localC(x = Data_final_sp$Tasa_prop,
                    listw = nb2listw(vecinos1, style = "W")))

tm_shape(cbind(Data_final_sp, geary_local)) + tm_fill(col = "Ii", style = "quantile",
                                                title = "local geary statistic")

mapview(cbind(Data_final_sp, geary_local)["Ii"])
