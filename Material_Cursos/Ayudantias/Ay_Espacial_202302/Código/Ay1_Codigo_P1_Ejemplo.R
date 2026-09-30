library(sf) # Paquete de geoestadistica (sucesor de sp)
library(tidyverse)
library(ggplot2)

# Crear un data frame con algunas coordenadas de puntos
datos_puntos <- data.frame(
  nombre = c("Punto A", "Punto B", "Punto C"),
  longitud = c(-70.6483, -70.6525, -70.6519),
  latitud = c(-33.4569, -33.4578, -33.4602)
)

# Convertir el data frame a un objeto sf de puntos
puntos_sf <- st_as_sf(datos_puntos, coords = c("longitud", "latitud"))

# Visualizar los puntos en un mapa
plot(puntos_sf, pch = 16, col = "blue", main = "Puntos Georreferenciados")


# Ejemplo -----------------------------------------------------------------

# Load the "world" dataset from the ggplot2 package
volcano <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv")
world <- map_data("world")
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "lightgray", size = 0.1
  ) 
volcano_sf <- st_as_sf(volcano, coords = c("longitude", "latitude"), crs = 4326)
volcano_sf %>%
  ggplot() +
  geom_sf(color = "black", fill = "lightgray", size = 0.1
  )


ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "white", fill = "lightgray", size = 0.1
  ) +
  geom_point(
    data = volcano,
    aes(longitude, latitude, color = primary_volcano_type),
    alpha = 0.7
  ) +
  theme_void() +
  labs(title="Volcano Locations")
# theme(legend.position = "none")
