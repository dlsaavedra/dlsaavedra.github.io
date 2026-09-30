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




setwd("/Users/danielsaavedramorales/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Desktop/Doctorado_Estadistica/2013_2/Ayudantia_Espacial/Ayudantias/Ayudantia_1/" )
# Cargar Datos de Estaciones ----------------------------------------------
datos_csv <- read.csv("Estaciones_actuales_Metro_de_Santiago..csv")
puntos_estaciones <- st_as_sf(datos_csv, coords = c("X", "Y"), crs = 4326)

puntos_estaciones %>%
  ggplot() +
  geom_sf()

# Leer Shapes Estaciones de Metro --------------------------------------------------------------
# Load the example dataset
estaciones <- read_sf("Estaciones_actuales_Metro_de_Santiago_/Estaciones_actuales_Metro_de_Santiago..shp") 
# Print the first few rows of the dataset
print(head(estaciones))

#Operador Pip 
estaciones %>%
  ggplot() +
  geom_sf()

# Leer Shapes Lineas de Metro --------------------------------------------------------------
# Load the example dataset
metro <- read_sf("Lineas_de_metro_de_Santiago_/L%C3%ADneas_de_metro_de_Santiago..shp") 
# Print the first few rows of the dataset
print(head(metro))

#Operador Pip 9
metro %>%
  ggplot() +
  geom_sf()

Comunas <- read_sf("Comunas/comunas.shp") %>%
  filter(codregion ==13 )

ggplot() +
  geom_sf(data = Comunas, color = "black")+
  geom_sf(data = estaciones, color = "red") +
  geom_sf(data = metro, color = "blue") +
  scale_fill_viridis_d() +  # Escala de colores para los atributos
  theme_minimal()
