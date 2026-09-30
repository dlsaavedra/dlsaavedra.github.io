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
  #filter(party == 'Republican') %>%
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


# Elecciones Chile --------------------------------------------------------
# Load the example dataset
library(readxl)
library(stringi)
  
Comunas <- read_sf("Comunas/comunas.shp") 
# Print the first few rows of the dataset
print(head(chile))

Comunas_Santiago <- 
  Comunas %>%
  filter(codregion ==13 )
  
# Graficar 
Comunas_Santiago %>%
  ggplot() +
  geom_sf(fill = "lightblue", color = "black")+
  labs(title = "Regiones de Chile") +
  theme_minimal()

#Cargar resultados por comuna
datos_elecciones <- read_excel("13_Resultados_Mesa_Alcaldes_TER.xlsx")

# Calcular el total de votos por comuna y candidato
resultados_comuna_candidato <- datos_elecciones %>%
  group_by(Comuna, Candidato, Pacto) %>%
  summarize(total_votos = sum(`Votos TER`))

Total_votos <- resultados_comuna_candidato %>%
  group_by(Comuna) %>%
  summarize(total_votos_comuna = sum(total_votos))
# Identificar al candidato con mayor votación por comuna
ganadores_comuna <- resultados_comuna_candidato %>%
  group_by(Comuna) %>%
  filter(total_votos == max(total_votos))

ganadores_comuna <- left_join(ganadores_comuna, Total_votos, by = "Comuna")

Comunas_Santiago_F <- Comunas_Santiago %>%
  mutate(Comuna = stri_trans_general(Comuna, "Latin-ASCII") %>%
           toupper())



election_santiago <- left_join(Comunas_Santiago_F, ganadores_comuna, by = 'Comuna')

election_santiago %>%
  ggplot(aes(fill = Pacto)) +
  geom_sf() +
  #scale_fill_manual(values = c("M" = "blue", "F" = "red"))  # Definir colores manualmente +
  theme(legend.position = "bottom",
      strip.background = element_blank()) +
  labs(fill = "Pacto Político ",
       title = "Pacto Político del alcalde elegido") +
  theme_minimal()

election_santiago %>%
  ggplot(aes(fill = total_votos_comuna)) +
  geom_sf() +
  scale_fill_viridis_c(option = "plasma") +
  #scale_fill_manual(values = c("M" = "blue", "F" = "red"))  # Definir colores manualmente +
  theme(legend.position = "bottom",
        strip.background = element_blank()) +
  labs(fill = "Votos emitidos en la comuna ",
       title = "Votos emitidos en la Región Metropolitana") +
  theme_minimal()


election_santiago %>%
  ggplot(aes(fill = total_votos/total_votos_comuna)) +
  geom_sf() +
  scale_fill_viridis_c(option = "plasma") +
  #scale_fill_manual(values = c("M" = "blue", "F" = "red"))  # Definir colores manualmente +
  theme(legend.position = "bottom",
        strip.background = element_blank()) +
  labs(fill = "Porcentaje de votos ",
       title = "Porcentaje de votos del alcalde elegido ") +
  theme_minimal()



# Opiates -----------------------------------------------------------------
# We will use data from the socviz package which has opioid death rates by states over time
data(opiates)
head(opiates[,c("adjusted", "abbr")])
opiates_f <-
  opiates %>%
  select(-state) %>%
  rename(STUSPS = abbr) %>%
  complete(STUSPS, year) #Agrega filas de los estados que no tienen información en cierto año.


opiates_usa_48 <- left_join(usa_48, opiates_f, by = "STUSPS")

opiates_usa_48 %>%
  filter(year > 1998) %>%
  ggplot(aes(fill = adjusted)) +
  geom_sf() +
  facet_wrap(~year, ncol = 4) +
  scale_fill_viridis_c(option = "plasma") +
  theme(legend.position = "bottom",
        strip.background = element_blank()) +
  labs(fill = "Death rate per 100,000 population ",
       title = "Opiate Related Deaths by State, 2000-2014")  

# Crear la animación (Error)
library(gganimate)
opiates_usa_48 %>%
  filter(year > 1998) %>%
  ggplot(aes(fill = adjusted)) +
  geom_sf() +
  scale_fill_viridis_c(option = "plasma") +
  labs(fill = "Death rate per 100,000 population ",
       title = "Opiate Related Deaths by State, {frame_time}") +
  #transition_time(as.integer(year)) +
  ease_aes('bounce-in-out')

