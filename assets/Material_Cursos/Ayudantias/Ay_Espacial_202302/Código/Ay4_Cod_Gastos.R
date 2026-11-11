setwd("/Users/danielsaavedramorales/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Desktop/Doctorado_Estadistica/2013_2/Ayudantia_Espacial/Ayudantias/Ayudantia_4/")
## https://www.bcn.cl/portal/ Portal Datos 


# Viejas librer?as
library(tidyverse);library(leaflet); library(mapview)
# Nuevas librer?as
library(spdep); library(gstat)

Chile = st_read("../Ayudantia_1/Comunas/", "comunas")
Ohiggins = Chile %>% dplyr::filter( Region == "Región del Libertador Bernardo O'Higgins")
url_Educ = "datos_Gasto_Educ_2022_RegionVI.csv"
url_Patente = "datos_Ingresos_Patentes_2022_RegionVI.csv"

Datos_G_Educ= read_csv(url_Educ)
Datos_G_Educ_final <- Datos_G_Educ %>%
  rename(`Comuna` = `Unidad territorial` )%>%
  rename(`Gasto_2022` = `2022`)
Datos_G_Educ_final$Gasto_2022 = as.integer(Datos_G_Educ_final$Gasto_2022)/1000000
Datos_G_Educ_final$Gasto_2022[]

Datos_G_Patente= read_csv(url_Patente)
Datos_G_Patente_final <- Datos_G_Patente %>%
  rename(`Comuna` = `Unidad territorial` )%>%
  rename(`Patente_2022` = `2022`)
Datos_G_Patente_final$Patente_2022 = as.integer(Datos_G_Patente_final$Patente_2022)/1000000


Datos_G_Patente_final$Comuna[19] = 'Marchigüe'
Datos_G_Patente_final$Comuna[15] = "Quinta de Tilcoco"
Datos_G_Educ_final$Comuna[19] = 'Marchigüe'
Datos_G_Educ_final$Comuna[15] = "Quinta de Tilcoco"

#Juntamos los datos de las comunas con La información
Data_final = Ohiggins %>%
  left_join(Datos_G_Educ_final, by = "Comuna") %>%
  left_join(Datos_G_Patente_final, by = "Comuna")


### Opciones de visualización ###
# Op1
mapview(Data_final, zcol = "Gasto_2022", popup = TRUE)

#Op2
Data_final %>%
  ggplot(aes(fill = Gasto_2022)) +
  geom_sf() +
  scale_fill_viridis_c(option = "plasma") +
  theme(legend.position = "right",
        strip.background = element_blank()) +
  labs(fill = "Gasto educativos MM ",
       title = "Gasto educativos MM")


## Generación de Vecinos de cada área------

library(spdep)
library(sp)
#Data_final_sp = as_Spatial(Data_final)
Data_final_filter = Data_final[complete.cases(Data_final$Gasto_2022), ]
Data_final_sp = as_Spatial(Data_final_filter)
Data_final_sp$Comuna # La posicion del vector es la identificacion
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

####
library(spatialreg)
library(viridis)
limites = seq(-20, 20, by = 3)
colores <- viridis(41)

lm0 <- lm(Gasto_2022 ~ 1, data=Data_final_sp)
summary(lm0)

lm1 <- lm(Gasto_2022 ~ Patente_2022, data=Data_final_sp)
summary(lm1)
AIC(lm0,lm1)
# Plot Residual
Data_final_filter$lm1 = as.double(lm1$residuals)
mapview(Data_final_filter, zcol = "lm1", popup = TRUE, col.regions = colores, at = limites)



eigens = eigenw(W_1_w)
sar0 = spautolm(Gasto_2022~ 1, data=Data_final_sp, listw=W_1_w, family="SAR", method="eigen",
         control=list(pre_eig= eigens))
summary(sar0)
# Plot Residual
Data_final_filter$sar0 = as.double(sar0$fit$residuals)
mapview(Data_final_filter, zcol = "sar0", popup = TRUE, col.regions = colores, at =limites )
# Plot signal_trend
Data_final_filter$sar0_trend = as.double(sar0$fit$signal_trend)
mapview(Data_final_filter, zcol = "sar0_trend", popup = TRUE, col.regions = colores, at =limites )
# Plot signal_stochastic
Data_final_filter$sar0_stochastic = as.double(sar0$fit$signal_stochastic)
mapview(Data_final_filter, zcol = "sar0_stochastic", popup = TRUE, col.regions = colores, at =limites )



sar1 = spautolm(Gasto_2022 ~ Patente_2022, data=Data_final_sp, listw=W_1_w, family="SAR", method="eigen",
                control=list(pre_eig= eigens))
summary(sar1)
# Plot Residual
Data_final_filter$sar1 = as.double(sar1$fit$residuals)
mapview(Data_final_filter, zcol = "sar1", popup = TRUE, col.regions = colores, at =limites )
# Plot signal_trend
Data_final_filter$sar1_trend = as.double(sar1$fit$signal_trend)
mapview(Data_final_filter, zcol = "sar1_trend", popup = TRUE, col.regions = colores, at =limites )
# Plot signal_stochastic
Data_final_filter$sar1_stochastic = as.double(sar1$fit$signal_stochastic)
mapview(Data_final_filter, zcol = "sar1_stochastic", popup = TRUE, col.regions = colores, at =limites )


help(spautolm)

car0 = spautolm(Gasto_2022~ 1, data=Data_final_sp, listw=W_1_w, family="CAR", method="eigen",
                control=list(pre_eig= eigens))
summary(car0)
# Plot Residual
Data_final_filter$car0 = as.double(car0$fit$residuals)
mapview(Data_final_filter, zcol = "car0", popup = TRUE, col.regions = colores, at =limites )
# Plot signal_trend
Data_final_filter$car0_trend = as.double(car0$fit$signal_trend)
mapview(Data_final_filter, zcol = "car0_trend", popup = TRUE, col.regions = colores, at =limites )
# Plot signal_stochastic
Data_final_filter$car0_stochastic = as.double(car0$fit$signal_stochastic)
mapview(Data_final_filter, zcol = "car0_stochastic", popup = TRUE, col.regions = colores, at =limites )

car1 = spautolm(Gasto_2022~ Patente_2022, data=Data_final_sp, listw=W_1_w, family="CAR", method="eigen",
                control=list(pre_eig= eigens))
summary(car1)
# Plot Residual
Data_final_filter$car1 = as.double(car1$fit$residuals)
mapview(Data_final_filter, zcol = "car1", popup = TRUE, col.regions = colores, at =limites )
# Plot signal_trend
Data_final_filter$car1_trend = as.double(car1$fit$signal_trend)
mapview(Data_final_filter, zcol = "car1_trend", popup = TRUE, col.regions = colores, at =limites )
# Plot signal_stochastic
Data_final_filter$car1_stochastic = as.double(car1$fit$signal_stochastic)
mapview(Data_final_filter, zcol = "car1_stochastic", popup = TRUE, col.regions = colores, at =limites )


## Analisisi de la matriz de adyacencia utilizada

eigens_b = eigenw(W_1_b)

sar1_b = spautolm(Gasto_2022 ~ Patente_2022, data=Data_final_sp, listw=W_1_b, family="SAR", method="eigen",
                control=list(pre_eig= eigens_b))
summary(sar1_b)
# Plot Residual
Data_final_filter$sar1_b = as.double(sar1_b$fit$residuals)
mapview(Data_final_filter, zcol = "sar1_b", popup = TRUE, col.regions = colores, at =limites )
# Plot signal_trend
Data_final_filter$sar1_b_trend = as.double(sar1_b$fit$signal_trend)
mapview(Data_final_filter, zcol = "sar1_b_trend", popup = TRUE, col.regions = colores, at =limites )
# Plot signal_stochastic
Data_final_filter$sar1_b_stochastic = as.double(sar1_b$fit$signal_stochastic)
mapview(Data_final_filter, zcol = "sar1_b_stochastic", popup = TRUE, col.regions = colores, at =limites )


eigens_d = eigenw(W_1_d)

sar1_d = spautolm(Gasto_2022 ~ Patente_2022, data=Data_final_sp, listw=W_1_d, family="SAR", method="eigen",
                  control=list(pre_eig= eigens_d))
summary(sar1_d)
# Plot Residual
Data_final_filter$sar1_d = as.double(sar1_d$fit$residuals)
mapview(Data_final_filter, zcol = "sar1_d", popup = TRUE, col.regions = colores, at =limites )
# Plot signal_trend
Data_final_filter$sar1_d_trend = as.double(sar1_d$fit$signal_trend)
mapview(Data_final_filter, zcol = "sar1_d_trend", popup = TRUE, col.regions = colores, at =limites )
# Plot signal_stochastic
Data_final_filter$sar1_d_stochastic = as.double(sar1_d$fit$signal_stochastic)
mapview(Data_final_filter, zcol = "sar1_d_stochastic", popup = TRUE, col.regions = colores, at =limites )

sar1$lambda
sar1_b$lambda
sar1_d$lambda


AIC(sar1,sar1_b,sar1_d,car1,lm1)
