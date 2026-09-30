#install.packages("GGally")
#install.packages("ggplot2")
#install.packages("readxl")
#install.packages("plotly")

library(readxl)
library(GGally)
library(ggplot2)
library(plotly)
# Ejemplo 1 ####
# Segunda Base de datos ----
## Leer el archivo XLSX%>%----

getwd() # Directorio de trabajo
ruta_archivo <- "Ayudantía1/Ay1_Datos.xlsx"


datos <- read_excel(ruta_archivo, "Promedio_2021_N8_NM1")

## Ver los datos leídos----
head(datos)
str(datos)
summary(datos)
datos$EDAD_ALU <- as.factor(datos$EDAD_ALU)
summary(datos)
## Histogramas-----
hist(as.numeric(as.character(datos$EDAD_ALU)))
hist(datos$PROM_GRAL_8vo)
hist(datos$PROM_GRAL_1ero, freq = FALSE, breaks = 50)
lines(density(datos$PROM_GRAL_1ero),
      col = "red", add = T)

## Grafico de Dispersión ------
plot(datos$PROM_GRAL_8vo, datos$PROM_GRAL_1ero,
     xlab = "PROM_GRAL_8vo",
     ylab = "PROM_GRAL_1ero",
     main = "Gráfico de dispersión entre
     PROM_GRAL_8vo y PROM_GRAL_1ero",
     xlim = c(2,7),
     ylim = c(2,7))

plot(runif(1000), runif(1000))

plot(datos$PROM_GRAL_8vo, datos$PROM_GRAL_1ero,
     col = datos$EDAD_ALU,
     xlab = "PROM_GRAL_8vo",
     ylab = "PROM_GRAL_1ero",
     main = "Gráfico de dispersión entre PROM_GRAL_8vo y PROM_GRAL_1ero")
# Agregar leyenda con etiquetas de colores
legend("bottomright", legend = unique(datos$EDAD_ALU),
       col = unique(datos$EDAD_ALU), pch = 16, title = "Etiqueta de colores")


# Crear el gráfico de dispersión con tendencia
ggplot(data = datos, aes(x = PROM_GRAL_8vo,
                         y = PROM_GRAL_1ero)) +
  geom_point() +  # Puntos de dispersión
  geom_smooth(method = "lm", se = FALSE) +
  # Líneas de tendencia
  labs(x = "PROM_GRAL_8vo", y = "PROM_GRAL_1ero",
       title = "Gráfico de dispersión de Promedios")

## Calcular intercepto y pendiente -----
X = datos$PROM_GRAL_8vo
Y = datos$PROM_GRAL_1ero

S_xx = sum((X-mean(X))**2)
S_xy = sum((Y-mean(Y))*(X-mean(X)))

b1 = S_xy/S_xx
b1
b0 = mean(Y) - b1*mean(X)
b0
Y_fit = b0 + b1*X
residuos = Y - Y_fit

sum(residuos**2)
sum((mean(Y) - Y_fit)**2)
sum((mean(Y)- Y)**2)

### Utilizando la función lm -----
model = lm(Y ~ X)
model$coefficients
sum(model$residuals**2)
sum((mean(Y) - model$fitted.values)**2)
sum((mean(Y) - Y)**2)

summary(model)



# Crear el gráfico de dispersión con tendencia y separados por edad
ggplot(data = datos, aes(x = PROM_GRAL_8vo,
                         y = PROM_GRAL_1ero, color = EDAD_ALU)) +
  geom_point() +  # Puntos de dispersión
  geom_smooth(method = "lm", se = FALSE) +  # Líneas de tendencia
  labs(x = "PROM_GRAL_8vo", y = "PROM_GRAL_1ero", title = "Gráfico de dispersión de Promedios")


# Regresión con categorías (mencionar solamente la idea)
model = lm(PROM_GRAL_1ero ~ PROM_GRAL_8vo +
             EDAD_ALU + PROM_GRAL_8vo*EDAD_ALU, data = datos)
model$coefficients
table(datos$EDAD_ALU)

summary(model)



# Crear el gráfico de dispersión con tendencia filtrados en notas mayores a 5.5
ggplot(data = datos[datos$PROM_GRAL_8vo > 5.5, ], aes(x = PROM_GRAL_8vo,
                                                      y = PROM_GRAL_1ero, color = EDAD_ALU)) +
  geom_point() +  # Puntos de dispersión
  geom_smooth(method = "lm", se = FALSE) +  # Líneas de tendencia
  labs(x = "PROM_GRAL_8vo", y = "PROM_GRAL_1ero", title = "Gráfico de dispersión de Promedios")

model = lm(PROM_GRAL_1ero ~ PROM_GRAL_8vo +
             EDAD_ALU + PROM_GRAL_8vo*EDAD_ALU, data = datos[datos$PROM_GRAL_8vo > 5.5, ])
summary(model)


# Crear el gráfico de dispersión con tendencia filtrados en notas menores a 5.5
ggplot(data = datos[datos$PROM_GRAL_8vo <= 5.5, ], aes(x = PROM_GRAL_8vo,
                                                       y = PROM_GRAL_1ero, color = EDAD_ALU)) +
  geom_point() +  # Puntos de dispersión
  geom_smooth(method = "lm", se = FALSE) +  # Líneas de tendencia
  labs(x = "PROM_GRAL_8vo", y = "PROM_GRAL_1ero", title = "Gráfico de dispersión de Promedios")

model = lm(PROM_GRAL_1ero ~ PROM_GRAL_8vo +
             EDAD_ALU + PROM_GRAL_8vo*EDAD_ALU, data = datos[datos$PROM_GRAL_8vo <= 5.5, ])
summary(model)

# Ejercicio 1 -----
dato1 = read_excel('Ayudantia2/Ay2_datos_SIMCE_N4_2018.xls')
dato2 = read_excel('Ayudantia2/Ay2_datos_SIMCE_N6_2018.xls')
dato3 = read_excel('Ayudantia2/Ay2_datos_SIMCE_N8_2019.xls')
dato4 = read_excel('Ayudantia2/Ay2_datos_SIMCE_NM2_2018.xls')
str(dato1)
str(dato2)
str(dato3)
str(dato4)

colnames(dato1)[colnames(dato1) == "2018"] = "N4"
colnames(dato2)[colnames(dato2) == "2018"] = "N6"
colnames(dato3)[colnames(dato3) == "2019"] = "N8"
colnames(dato4)[colnames(dato4) == "2018"] = "NM2"


# Combinar las tres bases de datos por la columna "Unidad territorial"
datos_combinados <- dato1 %>%
  merge(dato2, by = "Unidad territorial") %>%
  merge(dato3, by = "Unidad territorial") %>%
  merge(dato4, by = "Unidad territorial")

datos = datos_combinados[, c("Unidad territorial", "N4",
                             "N6", "N8", "NM2")]

colnames(datos)[colnames(datos) == "Unidad territorial"] =
  "Comuna"
## Ver los datos leídos----
head(datos)
summary(datos)
datos$N4 <- as.numeric(datos$N4)
datos$N6 <- as.numeric(datos$N6)
datos$N8 <- as.numeric(datos$N8)
datos$NM2 <- as.numeric(datos$NM2)
summary(datos)
## Histogramas-----
hist(datos$N4)
hist(datos$N6)
hist(datos$N8)
hist(datos$NM2)

## Grafico de Dispersión ------
plot(datos$N4, datos$N6,
     xlab = "Notas N4",
     ylab = "Notas N6 ",
     main = "Gráfico de dispersión Simce Matemática")


pairs(datos[, 2:5])

# Crear el gráfico de dispersión con tendencia
ggplot(data = datos, aes(x = N4,
                         y = N6)) +
  geom_point() +  # Puntos de dispersión
  geom_smooth(method = "lm", se = FALSE) +  # Líneas de tendencia
  labs(x = "Notas N4", y = "Notas N6", title = "Gráfico de dispersión Simce Matemática")

m = lm(N6 ~N4, data = datos)
summary(m)

# Crear el gráfico de dispersión con tendencia
ggplot(data = datos, aes(x = N4,
                         y = N8)) +
  geom_point() +  # Puntos de dispersión
  geom_smooth(method = "lm", se = FALSE) +  # Líneas de tendencia
  labs(x = "Notas N4", y = "Notas N8", title = "Gráfico de dispersión Simce Matemática")

m = lm(N8 ~N4, data = datos)
summary(m)

# Crear el gráfico de dispersión con tendencia
ggplot(data = datos, aes(x = N4,
                         y = NM2)) +
  geom_point() +  # Puntos de dispersión
  geom_smooth(method = "lm", se = FALSE) +  # Líneas de tendencia
  labs(x = "Notas N4", y = "Notas NM2", title = "Gráfico de dispersión Simce Matemática")

m = lm(NM2 ~N4, data = datos)
summary(m)

## Plotly ----

plot_ly(data = datos, x = ~N4, y = ~N8,
        text = ~Comuna)



# Pregunta Bonus ####
ruta_archivo <- "Ayudantía1/Ay1_Datos_Curso.xlsx"

## Leer el archivo XLSX%>%----
datos <- read_excel(ruta_archivo)

## Ver los datos leídos----
str(datos)
head(datos)
summary(datos)
ggpairs(datos)

datos_naomit = na.omit(datos)
ggpairs(datos_naomit)
