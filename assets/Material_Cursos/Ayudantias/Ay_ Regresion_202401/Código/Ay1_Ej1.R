# Instalar paquetes----
#install.packages("openxlsx")
#install.packages("dplyr")
#install.packages("GGally")
#install.packages("ggplot2")
# Cargar paquetes ----
library(openxlsx)
library(dplyr)
library(GGally)
library(ggplot2)
# Leer Base de datos ----
## Especificar la ruta del archivo XLSX----


# Ejemplo 1 ####
getwd() # Directorio de trabajo
ruta_archivo <- "Ayudantía1/Ay1_Datos.xlsx"

## Leer el archivo XLSX%>%----
datos <- read.xlsx(ruta_archivo, "Promedio2022_N8")
## Analizar tipo de variables
str(datos)
## Ver los datos leídos----
head(datos)
summary(datos)
## Cambiamos el caracter erroneo----
datos$LET_CUR <- as.factor(gsub("�", "Ñ", datos$LET_CUR))
datos$EDAD_ALU <- as.factor(datos$EDAD_ALU)
datos$GEN_ALU <- as.factor(datos$GEN_ALU)
summary(datos)

### Función pip ----
#Con el operador %>%, cada paso toma el resultado del paso anterior como entrada.
datos2 <- datos %>%
  mutate(LET_CUR = ifelse(LET_CUR == "�", "N2", as.character(LET_CUR)))%>%
  mutate(LET_CUR = as.factor(LET_CUR))%>%
  mutate(EDAD_ALU = as.factor(EDAD_ALU))%>%
  mutate(GEN_ALU = as.factor(GEN_ALU))

summary(datos2)

## Histogramas-----
hist(datos$PROM_GRAL)
hist(datos$ASISTENCIA)

## Grafico de Dispersión ------
plot(datos$ASISTENCIA, datos$PROM_GRAL,
     xlab = "ASISTENCIA",
     ylab = "PROM_GRAL",
     main = "Gráfico de dispersión entre ASISTENCIA y PROM_GRAL")

plot(as.numeric(as.character(datos$EDAD_ALU)), datos$PROM_GRAL,
     xlab = "EDAD_ALU",
     ylab = "PROM_GRAL",
     main = "Gráfico de dispersión entre EDAD_ALU y PROM_GRAL")


## Multiples gráficos de dispersion -----
# Seleccionar todas las columnas deseadas

columnas_a_graficar <- datos[, c("LET_CUR", "EDAD_ALU",
                                 "PROM_GRAL","ASISTENCIA")]

pairs(columnas_a_graficar)

# ggplot como opción más completa.
ggpairs(datos)
ggpairs(datos,cardinality_threshold =17)

# Grupo
cursos <- datos$LET_CUR

# Número de grupos
l <- length(unique(cursos))

pairs(datos[, c("EDAD_ALU","PROM_GRAL","ASISTENCIA")],
      col = hcl.colors(l, "Temps")[cursos])

ggpairs(datos[, c("EDAD_ALU","PROM_GRAL","ASISTENCIA")],
        cardinality_threshold =17, aes(color = datos$LET_CUR, alpha = 0.5),
        upper = list(continuous = wrap("cor", size = 2.5)))

# Crear el gráfico de dispersión con tendencia y separados por edad

ggplot(data = datos, aes(#color = EDAD_ALU,
  x = ASISTENCIA,
  y = PROM_GRAL)) +
  geom_point() +  # Puntos de dispersión
  geom_smooth(method = "lm", se = FALSE) +  # Líneas de tendencia
  labs(x = "ASISTENCIA", y = "PROM_GRAL", title = "Gráfico de dispersión de Promedios")


