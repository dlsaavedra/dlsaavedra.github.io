library(openxlsx)
## ## ## ## ### ### ### ### ##

#Ejemplo 1 ###################

## Leer los datos #####
HC_data <- read.xlsx("Ayudantia3/Life_Expectancy_Data_NA.xlsx")











## Visualizar Datos------
str(HC_data)
head(HC_data)
summary(HC_data)
pairs(HC_data[3:10])

plot(HC_data$Schooling, HC_data$Life.expectancy)
plot(HC_data$BMI, HC_data$Life.expectancy)
plot(HC_data$Alcohol, HC_data$Life.expectancy)
plot(HC_data$Total.expenditure, HC_data$Life.expectancy)

### Summary & Histograma -----
hist(HC_data$Life.expectancy, freq = FALSE, breaks = 30)
hist(HC_data$Schooling, freq = FALSE, breaks = 30)
hist(HC_data$BMI, freq = FALSE, breaks = 30)
hist(HC_data$Alcohol, freq = FALSE, breaks = 30)
hist(HC_data$Total.expenditure, freq = FALSE, breaks = 30)


?abline



## Ajustar lm ----
model_lm <- lm(Life.expectancy ~ Schooling, data = HC_data)
summary(model_lm)
## Graficar los datos con la linea ajustada
plot(HC_data$Schooling, HC_data$Life.expectancy)
abline(a = model_lm$coefficients[1],
       b = model_lm$coefficients[2], col = "blue", lwd = 5)

X = HC_data$Schooling
Y = HC_data$Life.expectancy
n = length(X)
Y_fit = model_lm$fitted.values
## Recta Media, Bandas de Confianza ----

# Graficar los puntos
plot(X, Y, xlab = "X", ylab = "Y", main = "Regresión Lineal Simple", pch = 16)

# Agregar la recta de regresión
abline(model_lm, col = "blue")

# Obtener el intervalo de confianza de la recta media
int_conf <- predict(model_lm, interval = "confidence", level = 0.95)

# Agregar el intervalo de confianza de la recta media
lines(X, int_conf[, "lwr"], col = "red", lty = 2)  # Línea inferior
lines(X, int_conf[, "upr"], col = "red", lty = 2)  # Línea superior

# Obtener el intervalo de predicción
int_pred <- predict(model_lm, interval = "prediction", level = 0.95)

# Agregar el intervalo de predicción
lines(X, int_pred[, "lwr"], col = "green", lty = 2)  # Línea inferior
lines(X, int_pred[, "upr"], col = "green", lty = 2)  # Línea superior

# Agregar leyenda
legend("topleft", legend = c("Recta de regresión", "Intervalo de confianza", "Intervalo de predicción"),
       col = c("blue", "red", "green"), lty = c(1, 2, 2), cex = .5)


## Análisis de Residuos
residuos <- residuals(model_lm)
plot(ts(residuos))
hist(residuos, breaks = 50, xlab = "Residuos", main = "Histograma de Residuos")
qqnorm(residuos)
qqline(residuos)

#acf(residuos)
