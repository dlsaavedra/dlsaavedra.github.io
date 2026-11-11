library(ggplot2) 
library(tidyverse)
library(gganimate)
library(directlabels)
library(png)
library(transformr)
library(grid)



df = data.frame(A=sample(1:75, 50, replace=TRUE),
                B=sample(1:100, 50, replace=TRUE),
                stringsAsFactors = FALSE)
ggplot(df, aes(A, B)) +
  geom_line() +
  transition_reveal(A) +
  labs(title = 'A: {frame_along}')


# Example GGanimate -------------------------------------------------------

library(gapminder)
head(gapminder)
p <- ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = country)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")
p
p + transition_time(year) +
  labs(title = "Year: {frame_time}")


library(sf)
library(ggplot2)
library(gganimate)

# Supongamos que tienes un conjunto de datos llamado 'datos_poligonos'
# Asegúrate de que 'datos_poligonos' tenga una columna 'year' que indica el año de cada polígono
setwd("/Users/danielsaavedramorales/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Desktop/Doctorado_Estadistica/2013_2/Ayudantia_Espacial/Ayudantias/Ayudantia_1/" )
# Load the example dataset
usa <- read_sf("cb_2017_us_state_20m/cb_2017_us_state_20m.shp") 
# Crear la animación
library(tidyverse)
usa_48 <-
  usa %>%
  filter(!(STUSPS %in% c('AK', 'HI', 'PR')))

state_areas <-
  usa %>%
  mutate(area = st_area(.)) %>%
  st_set_geometry(NULL) %>%
  select(NAME, area) %>%
  arrange(desc(area)) 
state_areas

usa_48 %>%
  ggplot() +
  geom_sf()
library(socviz)
data(opiates)
head(opiates[,c("adjusted", "abbr")])

opiates <-
  opiates %>%
  select(-state) %>%
  rename(STUSPS = abbr) %>%
  complete(STUSPS, year)

usa_48_P <- left_join(usa_48, opiates, by = "STUSPS")
head(usa_48)

# Filtrar datos solo para el estado de California (STUSPS == "CA")
filtered_data <- usa_48_P %>%
  filter(STUSPS == "AL")#%>% filter(year < 2001)

filtered_data %>%
  filter(year > 1998) %>%
  ggplot(aes(fill = adjusted)) +
  geom_sf() +
  facet_wrap(~year, ncol = 4) +
  scale_fill_viridis_c(option = "plasma") +
  theme(legend.position = "bottom",
        strip.background = element_blank()) +
  labs(fill = "Death rate per 100,000 population ",
       title = "Opiate Related Deaths by State, 2000-2014") 


library(gganimate)
filtered_data %>%
  filter(year < 2008) %>%
  ggplot() +
  geom_sf(aes(fill = adjusted, geometry = geometry)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(fill = "Death rate per 100,000 population ",
       title = "Opiate Related Deaths by State, {frame_time}") +
  transition_time(as.integer(year))+
  ease_aes('bounce-in-out')




  
st_geometry(filtered_data)
df <- quakes %>%
  st_as_sf(coords = c("long", "lat"))

## No CRS
st_crs(df)

## Runs fine
ggplot(df) +
  geom_sf(aes(color = mag)) +
  transition_time(stations)

## Apply a CRS
df_crs <- quakes %>%
  st_as_sf(coords = c("long", "lat"),
           crs = 4326)

st_crs(df_crs)
## Gets the error
ggplot(df_crs) +
  geom_sf(aes(color = mag)) +
  transition_time(stations)
