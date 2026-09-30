
load("Ayudantia9/datos_pinguinos")

datos$especies_sexo <- interaction(datos$sexo, datos$especies)
# Inicializar el gráfico
plot(datos$longitud_pico_cm, datos$masa_Kg, col = datos$especies_sexo, pch = 19,
     xlab = "Longitud del Pico (cm)", ylab = "Peso Corporal (Kg)",
     main = "Peso Corporal vs Longitud del Pico por Especie")

# Añadir leyenda
niveles_especies_sexo <- levels(datos$especies_sexo)
legend("topleft", legend = niveles_especies_sexo, col = 1:length(niveles_especies_sexo),
       pch = 19, lwd = 2, cex = 0.5)



## Ajustar el modelo de regresión lineal con misma pendiente diferente intercepto
modelo_penguins1 <- lm(masa_Kg ~ longitud_pico_cm + especies_sexo , data = datos)

## Ajustar el modelo de regresión lineal con diferente pendiente misma intercepto
modelo_penguins2 <- lm(masa_Kg ~ longitud_pico_cm*especies_sexo - especies_sexo , data = datos)

## Ajustar el modelo de regresión lineal con diferente pendiente e intercepto
modelo_penguins3 <- lm(masa_Kg ~ longitud_pico_cm*especies_sexo  , data = datos)

AIC(modelo_penguins1, modelo_penguins2, modelo_penguins3) # Menor AIC mejor
BIC(modelo_penguins1, modelo_penguins2, modelo_penguins3) # Menor BIC mejor

# Resumen del modelo
summary(modelo_penguins1)

# Realizar un análisis de varianza para el modelo
anova(modelo_penguins1)

# Predecir valores ajustados
datos$predicted_body_mass <- predict(modelo_penguins1)

# Inicializar el gráfico
plot(datos$longitud_pico_cm, datos$masa_Kg, col = datos$especies_sexo, pch = 19,
     xlab = "Longitud del Pico (cm)", ylab = "Peso Corporal (Kg)",
     main = "Peso Corporal vs Longitud del Pico por Especie")

# Añadir líneas de regresión ajustadas para cada especie

niveles_especies_sexo = levels(datos$especies_sexo)
for (i in 1:length(niveles_especies_sexo)) {
  nivel_especies_sexo <- niveles_especies_sexo[i]
  subset_penguins <- datos[datos$especies_sexo == nivel_especies_sexo, ]
  lines(subset_penguins$longitud_pico_cm, subset_penguins$predicted_body_mass, col = i, lwd = 2)
}

# Añadir leyenda
legend("topleft", legend = niveles_especies_sexo, col = 1:length(niveles_especies_sexo),
       pch = 19, lwd = 2, cex = 0.5)

plot(datos$predicted_body_mass,modelo_penguins1$residuals, col = datos$especies_sexo)

library(lmtest)
shapiro.test(modelo_penguins1$residuals)
dwtest(modelo_penguins1)
bptest(modelo_penguins1)
