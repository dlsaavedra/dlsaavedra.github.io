install.packages("haven")
library(haven)
# DATA -----
#https://www.kaggle.com/datasets/new-york-city/nyc-east-river-bicycle-crossings
data <- read.csv("original_Ay6_bicycle-counts.csv")
summary(data)
str(data)
#Limpieza de datos----
data$Precipitation[data$Precipitation == "0.47 (S)"] = "0.47"
data$Precipitation[data$Precipitation == "T"] = "0"
data$Precipitation = as.numeric(data$Precipitation)
str(data)

data_ok = data[-1:-3][-6:-8][-4]
pairs(data_ok)
cnames= colnames(data_ok)
cnames[1] = "Max_T_F"
cnames[2] = "Min_T_F"
cnames[3] = "Precipitaciones"
colnames(data_ok) = cnames

write.csv(data_ok, "Ay6_bicicletas_Manhattan.csv")

n = length(X)




data_ok <- read.csv("Ay6_bicicletas_Manhattan.csv")
pairs(data_ok[-1])
hist(data_ok$Manhattan.Bridge)
cor(data_ok[-1])
#Plot especificos----
#XB = data_ok$Brooklyn.Bridge

plot(data_ok$Max_T_F, data_ok$Manhattan.Bridge)
plot(data_ok$Min_T_F, data_ok$Manhattan.Bridge)

X = data_ok$Max_T_F
Y = data_ok$Manhattan.Bridge
n = length(X)
# Ajuste regresión----
modelT = lm(Y~X)
summary(modelT)

modelB = lm(Y~X - 1)
summary(modelB)
mean(modelB$residuals)

## Graficar los datos con la linea ajustada----
par(mfrow=c(1, 1))
plot(X, Y, xlim=c(0,100), ylim = c(0,8000))
abline(a = modelT$coefficients[1],
       b = modelT$coefficients[2], col = "blue", lwd = 5)

abline(a =0,
       b = modelB$coefficients[1], col = "red", lwd = 5)
## Recta Media, Bandas de Confianza ----

# Graficar los puntos
plot(X, Y, xlab = "X", ylab = "Y", main = "Regresión Lineal Simple", pch = 16)

# Agregar la recta de regresión----
abline(modelB, col = "blue", lwd = 4)

# Obtener el intervalo de confianza de la recta media
int_conf <- predict(modelB, interval = "confidence", level = 0.95)
# Agregar el intervalo de confianza de la recta media
lines(X, int_conf[, "lwr"], col = "red", lty = 2)  # Línea inferior
lines(X, int_conf[, "upr"], col = "red", lty = 2)  # Línea superior

# Obtener el intervalo de predicción
int_pred <- predict(modelT, interval = "prediction", level = 0.95)

# Agregar el intervalo de predicción
lines(X, int_pred[, "lwr"], col = "green", lty = 2)  # Línea inferior
lines(X, int_pred[, "upr"], col = "green", lty = 2)  # Línea superior

# Agregar leyenda
legend("topleft", legend = c("Recta de regresión", "Intervalo de confianza", "Intervalo de predicción"),
       col = c("blue", "red", "green"), lty = c(1, 2, 2), cex = .5)


predict(modelT, newdata = data.frame(X = 32), interval = "prediction")
predict(modelB, newdata = data.frame(X = 32), interval = "prediction")

# Grafico de Residuos -------
res = modelT$residuals
mean(res)

plot(res)
residuos_est = rstandard(modelT)

# Grafico de Residuos estandarizados vs valores predictores -----
plot(X, residuos_est,
     xlab = "Temperatura High F°", ylab = "Residuos",
     main = "Gráfico de Residuos vs. Valores Ajustados",
     ylim = c(-2.5,2.5))
abline(h = 0, col = "red", lty = 2)

# Calcular las bandas de confianza para los residuos estandarizados
banda_superior <- qt(.975, n-3)
banda_inferior <- -qt(.975, n-3)

# Agregar las bandas de confianza al gráfico
lines(range(X), c(banda_superior, banda_superior), lty = 2, col = "green")  # Banda superior
lines(range(X), c(banda_inferior, banda_inferior), lty = 2, col = "green")  # Banda inferior

# Grafico de Residuos estandarizados vs valores ajustados -----
plot(modelT$fitted.values, residuos_est,
     xlab = "Valores Ajustados", ylab = "Residuos",
     main = "Gráfico de Residuos vs. Valores Ajustados",
     ylim = c(-2.5,2.5))
abline(h = 0, col = "red", lty = 2)
# Agregar las bandas de confianza al gráfico
lines(range(modelT$fitted.values), c(banda_superior, banda_superior), lty = 2, col = "green")  # Banda superior
lines(range(modelT$fitted.values), c(banda_inferior, banda_inferior), lty = 2, col = "green")  # Banda inferior

outlier = which(abs(residuos_est)> 1.5)

# Graficar los puntos
plot(X, Y, xlab = "X", ylab = "Y", main = "Regresión Lineal Simple", pch = 16)
# Colorear los puntos con el índice guardado en index
points(X[outlier], Y[outlier], col = "red", pch = 20)
# Añadir leyenda
legend("topleft", legend = "Puntos coloreados (index)", pch = 20, col = "red", cex = .7)

# H0 Dist. Normal
qqnorm(res)
qqline(res)
shapiro.test(res)

# Autocorrelación H0 rho = 0
library(lmtest)
# Realizar el test de Durbin-Watson
resultado_dw <- dwtest(modelT)
resultado_dw

# Homocedasticidad
#Test de Breusch-Pagan  H0 Homocedasticidad
bp_test <- bptest(modelT)
print(bp_test)

# Puntos extremos
# Calcular el leverage de cada observación
leverage <- hatvalues(modelB)
plot(X, leverage)
# Puntos de influence

# Calcular la distancia de Cook medir el cambio en b_hat

cook_dist <- cooks.distance(modelT)
which.max(cooks.distance(modelT))
cooks.distance(modelT)[which.max(cooks.distance(modelT))]
# Crear el gráfico
plot(X,cook_dist, type = "p", pch = 20, col = "blue",
     main = "Gráfico de Distancia de Cook",
     xlab = "Temperatura", ylab = "Distancia de Cook",
     ylim = c(0,1))

points(X[outlier], cook_dist[outlier], col = "red", pch = 20)
abline(h = pf(.95,2,n-2), col = "red", lty = 2)
# Añadir línea de referencia
plot(X,cook_dist, type = "p", pch = 20, col = "blue",
     main = "Gráfico de Distancia de Cook",
     xlab = "Temperatura", ylab = "Distancia de Cook")
abline(h = 4 / length(cook_dist), col = "red", lty = 2)


# Calcular DFFITS  cambio en Y_hat
dffits = dffits(modelT)

plot(X,dffits, type = "p", pch = 20, col = "blue",
     main = "Gráfico de DFFITS",
     xlab = "Temperatura", ylab = "DFFITS",
     ylim = c(-.4, .2))
abline(h = 2 * sqrt(2/ length(dffits)), col = "red", lty = 2)
abline(h = -2 * sqrt(2/ length(dffits)), col = "red", lty = 2)

points(X[outlier], dffits[outlier], col = "red", pch = 20)

# Calcular DFBETAS  cambio en b_hat_j
dfbetas = dfbetas(modelT)

plot(X,dfbetas[,1], type = "p", pch = 20, col = "blue",
     main = "Gráfico de DFBETAS Intercepto",
     xlab = "Temperatura", ylab = "DFBETAS Intercepto",
     ylim = c(-.3, .3))
abline(h = 2 * sqrt(1/ length(dffits)), col = "red", lty = 2)
abline(h = -2 * sqrt(1/ length(dffits)), col = "red", lty = 2)

points(X[outlier], dfbetas[outlier,1], col = "red", pch = 20)


plot(X,dfbetas[,2], type = "p", pch = 20, col = "blue",
     main = "Gráfico de DFBETAS PENDIENTE",
     xlab = "Temperatura", ylab = "DFBETAS X",
     ylim = c(-.3, .3))
abline(h = 2 * sqrt(1/ length(dffits)), col = "red", lty = 2)
abline(h = -2 * sqrt(1/ length(dffits)), col = "red", lty = 2)

points(X[outlier], dfbetas[outlier,2], col = "red", pch = 20)

# Calcular COVRATIO
#Valores de COVRATIOi > 1 indican que la í-esima observación aumenta
#la precisión de los estimadores.
#Valores de COVRATIOi < 1 indican que la i  ́esima observaci  ́on dañ
#a la precisión de los estimadores.

covratio = covratio(modelT)

plot(X,covratio, type = "p", pch = 20, col = "blue",
     main = "Gráfico de COVRATIO",
     xlab = "Temperatura", ylab = "COVRATIO",
     ylim = c(0.97, 1.03))
abline(h = 1, col = "red", lty = 2)
abline(h = 1 - 3*2/length(covratio), col = "green", lty = 2)
abline(h = 1 + 3*2/length(covratio), col = "green", lty = 2)

points(X[outlier], covratio[outlier], col = "red", pch = 20)


## BONUS
plot(modelT,which=1:6)


