library(openxlsx)
## ## ## ## ### ### ### ### ##

# Leer los datos #####
data = read.xlsx('Ayudantia5/Ay5_Datos_Curso.xlsx')



# 0 Visualizar Datos------
str(data)
head(data)
summary(data)
data = na.omit(data)
# Obtener el número de columnas en el conjunto de datos
num_cols <- ncol(data)

# Configurar el diseño del gráfico
par(mfrow=c(ceiling(sqrt(num_cols)), ceiling(sqrt(num_cols))))
par(mfrow=c(1, 1))

# Iterar sobre todas las columnas y crear histogramas
for (i in 1:num_cols) {
  hist(data[,i], main = paste("Histograma de", names(data)[i]), breaks = 14,
       xlab = names(data)[i], col = "skyblue", border = "black")
}
hist(data$I3, breaks = 20)
# 1 Asociación -----

pairs(data)
pairs(data, verInd = 6:6)
plot(data$Tarea.N3, data$I3)
# 2 Regresión Lineal

model_lm = lm(I3 ~ Tarea.N3 - 1, data = data)
summary(model_lm)

## Graficar los datos con la linea ajustada
par(mfrow=c(1, 1))
plot(data$Tarea.N3, data$I3, pch = 16)
abline(a = 0,
       b = model_lm$coefficients[1], col = "blue", lwd = 5)

X = data$Tarea.N3
Y = data$I3
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

predict(model_lm, data.frame(Tarea.N3 = 6.5),interval = "prediction")

# Grafico de Residuos -------
res = model_lm$residuals
mean(res)

plot(res)

# Grafico de Residuos vs valores ajustados -----
plot(model_lm$fitted.values, model_lm$residuals,
     xlab = "Valores Ajustados", ylab = "Residuos",
     main = "Gráfico de Residuos vs. Valores Ajustados")
abline(h = 0, col = "red", lty = 2)

# Calcular los residuos estandarizados
residuos_studenizados <- rstandard(model_lm)
# Crear un gráfico de dispersión de los residuos estandarizados vs. los valores ajustados
plot(fitted(model_lm), residuos_studenizados,
     xlab = "Valores Ajustados", ylab = "Residuos Studentizados",
     main = "Gráfico de dispersión de Residuos Studentizados vs. Valores Ajustados")
abline(h = 0, col = "red", lty = 2)


qqnorm(res)
qqline(res)
shapiro.test(res) # H0 Dist. Normal

install.packages("lmtest")
library(lmtest)
# Realizar el test de Durbin-Watson
resultado_dw <- dwtest(model_lm)

acf(res)
# Imprimir el resultado
print(resultado_dw)  # H0 Correlacion = 0


# Modelo 2 ----
model_lm = lm(I3 ~ Tarea.N3 - 1, data = data)
summary(model_lm)

## Graficar los datos con la linea ajustada
par(mfrow=c(1, 1))
plot(data$Tarea.N3, data$I3, pch = 16)
abline(a = 0,
       b = model_lm$coefficients[1], col = "blue", lwd = 5)

X = data$Tarea.N3
Y = data$I3
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

predict(model_lm, data.frame(Tarea.N3 = 6.5),interval = "prediction")

# Grafico de Residuos -------
res = model_lm$residuals
mean(res)

plot(res)

# Grafico de Residuos vs valores ajustados -----
plot(model_lm$fitted.values, model_lm$residuals,
     xlab = "Valores Ajustados", ylab = "Residuos",
     main = "Gráfico de Residuos vs. Valores Ajustados")
abline(h = 0, col = "red", lty = 2)

# Calcular los residuos estandarizados
residuos_studenizados <- rstandard(model_lm)
# Crear un gráfico de dispersión de los residuos estandarizados vs. los valores ajustados
plot(fitted(model_lm), residuos_studenizados,
     xlab = "Valores Ajustados", ylab = "Residuos Studentizados",
     main = "Gráfico de dispersión de Residuos Studentizados vs. Valores Ajustados")
abline(h = 0, col = "red", lty = 2)


qqnorm(res)
qqline(res)
shapiro.test(res) # H0 Dist. Normal

#install.packages("lmtest")
library(lmtest)
# Realizar el test de Durbin-Watson
resultado_dw <- dwtest(model_lm)

# Imprimir el resultado
print(resultado_dw)  # H0 Correlacion = 0






model_lm = lm(I2 ~ Tarea.N2-1, data = data)
summary(model_lm)

## Graficar los datos con la linea ajustada
par(mfrow=c(1, 1))
plot(data$Tarea.N2, data$I2, pch = 16)
abline(a = 0,
       b = model_lm$coefficients[1], col = "blue", lwd = 5)


model_lm = lm(I1 ~ Tarea.N1 - 1, data = data)
summary(model_lm)

## Graficar los datos con la linea ajustada
par(mfrow=c(1, 1))
plot(data$Tarea.N2, data$I2, pch = 16)
abline(a = 0,
       b = model_lm$coefficients[1], col = "blue", lwd = 5)
