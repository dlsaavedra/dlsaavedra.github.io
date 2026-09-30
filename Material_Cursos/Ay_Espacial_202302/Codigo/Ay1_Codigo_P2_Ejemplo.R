# Instalar paquetes -------------------------------------------------------
#install.packages("sf")
#install.packages("devtools")
#devtools::install_github("thomasp85/transformr")
library(sf) # Paquete de geoestadistica (sucesor de sp)
library(tidyverse)
library(ggplot2) 
library(socviz) # Datos de muerte por opioides y elecciones presidencial usa 2016
library(gganimate) # Animación Plot
library(transformr)


# Leer Shapes --------------------------------------------------------------
setwd("/Users/danielsaavedramorales/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Desktop/Doctorado_Estadistica/2013_2/Ayudantia_Espacial/Ayudantias/Ayudantia_1/" )
# Load the example dataset
usa <- read_sf("cb_2017_us_state_20m/cb_2017_us_state_20m.shp") 
# Print the first few rows of the dataset
print(head(usa))

# Descripción Data --------------------------------------------------------

# Filtrar Alaska, Hawai y Puerto Rico
usa_48 <-
  usa %>%
  filter(!(STUSPS %in% c('AK', 'HI', 'PR')))

#Operador Pip 
usa_48 %>%
  ggplot() +
  geom_sf()

# Crear el gráfico utilizando ggplot y geom_sf
plot <- ggplot() +
  geom_sf(data = usa_48)
print(plot)


# Elecciones USA 2016 -----------------------------------------------------

# We will use data from the socviz package which has opioid death rates by states over time
data(election)
head(election[,c("st", "party")])

election_f <-
  election %>%
  rename(STUSPS = st)

election_usa_48 <- left_join(usa_48, election_f, by = "STUSPS")


election_usa_48 %>%
  filter(party == 'Republican') %>%
  #filter(party == 'Democratic') %>%
  ggplot(aes(fill = total_vote)) +
  geom_sf() +
  scale_fill_viridis_c(option = "plasma") +
  theme(legend.position = "bottom",
        strip.background = element_blank()) +
  labs(fill = "Total Vote ",
       title = "Total Vote by State, USA Election 2016")  


election_usa_48 %>%
  ggplot(aes(fill = party)) +
  geom_sf() +
  scale_fill_manual(values = c("Republican" = "blue", "Democratic" = "red"))+  # Definir colores manualmente +
  theme(legend.position = "bottom",
        strip.background = element_blank()) +
  labs(fill = "Total Vote ",
       title = "Total Vote by State, USA Election 2016") 
