load("Ayudantia9/datos_pinguinos")


# Inicializar el gráfico
plot(datos$longitud_pico_cm, datos$masa_Kg, col = datos$especies, pch = 19,
     xlab = "Longitud del Pico (cm)", ylab = "Peso Corporal (Kg)",
     main = "Peso Corporal vs Longitud del Pico por Especie")
points(datos$longitud_pico_cm[datos$sexo == "male"],
       datos$masa_Kg [datos$sexo == "male"], pch = 2, col = "blue")
# Añadir leyenda
niveles_especies <- levels(datos$especies)
legend("topleft", legend = c(niveles_especies, "male"), col = c(1:length(niveles_especies), "blue"),
       pch = 19, lwd = 2, cex = 0.7)



## Ajustar el modelo de regresión lineal con misma pendiente diferente intercepto
modelo_penguins1 <- lm(masa_Kg ~ longitud_pico_cm + especies + sexo , data = datos)

## Ajustar el modelo de regresión lineal con diferente pendiente misma intercepto
modelo_penguins2 <- lm(masa_Kg ~ longitud_pico_cm*especies + longitud_pico_cm*sexo - especies - sexo , data = datos)

## Ajustar el modelo de regresión lineal con diferente pendiente e intercepto
modelo_penguins3 <- lm(masa_Kg ~ longitud_pico_cm*especies + longitud_pico_cm*sexo  , data = datos)

AIC(modelo_penguins1, modelo_penguins2, modelo_penguins3) # Menor AIC mejor
BIC(modelo_penguins1, modelo_penguins2, modelo_penguins3) # Menor BIC mejor

# Resumen del modelo
summary(modelo_penguins2)

# Realizar un análisis de varianza para el modelo
anova(modelo_penguins1)

# Predecir valores ajustados
datos$predicted_body_mass <- predict(modelo_penguins2)

# Inicializar el gráfico
plot(datos$longitud_pico_cm, datos$masa_Kg, col = datos$especies, pch = 19,
     xlab = "Longitud del Pico (cm)", ylab = "Peso Corporal (Kg)",
     main = "Peso Corporal vs Longitud del Pico por Especie")

# Añadir líneas de regresión ajustadas para cada especie

niveles_especies = levels(datos$especies)
for (i in 1:length(niveles_especies)) {
  nivel_especies <- niveles_especies[i]
  subset_penguins <- datos[datos$especies == nivel_especies, ]
  subset_penguins_male = subset_penguins[subset_penguins$sexo == "male", ]
  subset_penguins_female = subset_penguins[subset_penguins$sexo == "female" ,]
  lines(subset_penguins_male$longitud_pico_cm, subset_penguins_male$predicted_body_mass,
        col = i, lwd = 3, lty = 1)
  lines(subset_penguins_female$longitud_pico_cm, subset_penguins_female$predicted_body_mass,
        col = i, lwd = 1, lty = 4)

  }

# Añadir leyenda
legend("topleft", legend = niveles_especies, col = 1:length(niveles_especies),
       pch = 19, lwd = 2, cex = 0.5)

plot(datos$predicted_body_mass,modelo_penguins1$residuals, col = datos$especies)

library(lmtest)
shapiro.test(modelo_penguins2$residuals)
dwtest(modelo_penguins2)
bptest(modelo_penguins2)
