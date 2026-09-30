# Viejas librer?as
library(sf)
library(mapview)
library(mclust)

load("Ayudantia6/Datos_Atropellos_Santiago_Centro.RData")

Data$index = 1:length(Data$Siniestros)
mapview(Data, popup = TRUE, zcol = "Siniestros")


# Ver las primeras filas del objeto sf con coordenadas UTM
expanded_df <- data.frame()

sf_obj = Data
# Iterar sobre cada fila del objeto sf
for (i in 1:nrow(sf_obj)) {
  count <- sf_obj$Siniestros[i]
  geom <- sf_obj$geometry[i]
  coords <- st_coordinates(geom)
  
  # Crear un data.frame con las coordenadas repetidas según el conteo
  temp_df <- data.frame(
    index = rep(i, count),
    x = rep(coords[1], count),
    y = rep(coords[2], count)
  )
  
  # Agregar al data.frame de resultados
  expanded_df <- rbind(expanded_df, temp_df)
}

#plot(expanded_df)


# Ajustar el modelo de mezcla de gaussianas a las características del géiser
fit <- Mclust(expanded_df[-1])

# Resumen del modelo ajustado
summary(fit)

# Visualización de la clasificación de los datos según el modelo ajustado
plot(fit, what = "classification")

# Visualización de los valores de BIC para diferentes números de componentes
plot(fit, what = "BIC")

expanded_df$class = fit$classification

# Convertir el data.frame a un objeto sf
df_sf <- st_as_sf(expanded_df, 
                  coords = c("x", "y"), 
                  crs = 4326)  # EPSG 4326 es el sistema de coordenadas para GPS (WGS84)


mapview(df_sf, popup = TRUE, zcol = "class")
