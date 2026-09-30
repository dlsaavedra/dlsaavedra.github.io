##install.packages("haven")
library(haven)
# DATA -----
#https://www.kaggle.com/datasets/gregorut/videogamesales/data
data <- read.csv("Ayudantia7/vgsales_original.csv")
data = data[data$Platform == "XOne",][-c(3,5,6,10)]
colnames(data) = c("Ranking", "Nombre", "Año", "Ventas_NA",
                   "Ventas_EU", "Ventas_JP", "Ventas_Global")
write.csv(data, "Ayudantia7/Ay7_vgsales_XOne.csv")

data = read.csv("Ayudantia7/Ay7_vgsales_XOne.csv")
summary(data)
str(data)
A = data[3:5]
table(A[1])
table(A[2])
table(A[3])

#data_ok = log(data[7:10] + 0.0001)
data_ok = data[4:8]
summary(data_ok)
str(data_ok)
pairs(data_ok)
cor(data_ok)

#Outlier Wii 2006 (Wii Sport)
hist(log(data_ok$Ventas_NA))
hist(log(data_ok$Ventas_Global))

#Plot especificos----

plot(data_ok$Ventas_NA, data_ok$Ventas_Global)
plot(data_ok$Ventas_EU, data_ok$Ventas_Global)



Y = data_ok$Ventas_Global
X = data_ok$Ventas_NA
# Calcula los quintiles
#quintilesY <- quantile(Y, probs = seq(0, 1, 0.25))
#quintilesY
#quintilesX <- quantile(X, probs = seq(0, 1, 0.25))
#quintilesX
# Identifica los índices para eliminar el primer y quinto quintil
#indices_mantener <- which(Y > quintilesY[2] & X > quintilesX[2])
#X = log(data_ok$EU_Sales[indices_mantener])
#Y = log(data_ok$NA_Sales[indices_mantener])

n = length(X)

plot(X, Y,
     xlab = "Ventas_NA", ylab = "Ventas_Global")

# Ajuste regresión----
model_lm = lm(Y ~ X )
summary(model_lm)
mean(model_lm$residuals)

model_lm2 = lm(Y ~ X - 1)
summary(model_lm2)
mean(model_lm2$residuals)


## Graficar los datos con la linea ajustada----
par(mfrow=c(1, 1))
plot(X, Y,
     #xlim = c(0, 0.3), ylim = c(0, 0.3),
     xlab = "EU_Sales", ylab = "NA_Sales")
abline(a = model_lm$coefficients[1],
       b = model_lm$coefficients[2], col = "blue", lwd = 3)

abline(a =0,
       b = model_lm2$coefficients[1], col = "red", lwd = 3)
## Recta Media, Bandas de Confianza ----

# Graficar los puntos
plot(X, Y, xlab = "Ventas_NA", ylab = " Ventas_Global ",
     #xlim = c(-1, 10), ylim = c(-1,10),
     main = "Regresión Lineal Simple", pch = 16)

# Agregar la recta de regresión----
abline(model_lm, col = "blue", lwd = 4)

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


model_lm = lm(Y ~ X)
predict(model_lm, newdata = data.frame(X = 1), interval = "prediction")



# Grafico de Residuos -------
plot(model_lm)

res = model_lm$residuals
mean(res)

plot(res)
residuos_est = rstandard(model_lm)

# Grafico de Residuos estandarizados vs valores predictores -----
plot(X, residuos_est,
     xlab = "Ventas_NA", ylab = "Residuos",
     #ylim = c(-2.5,2.5),
     main = "Gráfico de Residuos vs. Valores Ajustados")
abline(h = 0, col = "red", lty = 2)

# Calcular las bandas de confianza para los residuos estandarizados
banda_superior <- qt(.975, n-3)
banda_inferior <- -qt(.975, n-3)

# Agregar las bandas de confianza al gráfico
lines(range(X), c(banda_superior, banda_superior), lty = 2, col = "green")  # Banda superior
lines(range(X), c(banda_inferior, banda_inferior), lty = 2, col = "green")  # Banda inferior

# Grafico de Residuos estandarizados vs valores ajustados -----
plot(model_lm$fitted.values, residuos_est,
     xlab = "Valores Ajustados", ylab = "Residuos",
   #  ylim = c(-2.5,2.5),
     main = "Gráfico de Residuos vs. Valores Ajustados")
abline(h = 0, col = "red", lty = 2)
# Agregar las bandas de confianza al gráfico
lines(range(model_lm$fitted.values), c(banda_superior, banda_superior), lty = 2, col = "green")  # Banda superior
lines(range(model_lm$fitted.values), c(banda_inferior, banda_inferior), lty = 2, col = "green")  # Banda inferior

outlier = which(abs(residuos_est)> qt(.975, n-3))

# Graficar los puntos
plot(X, Y, xlab = "X", ylab = "Y", main = "Regresión Lineal Simple", pch = 16)
# Colorear los puntos con el índice guardado en index
points(X[outlier], Y[outlier], col = "red", pch = 20)
# Añadir leyenda
legend("topleft", legend = "Puntos extraños", pch = 20, col = "red", cex = .7)

# H0 Dist. Normal
qqnorm(res)
qqline(res)
shapiro.test(res[0:5000])
#Anderson-Darling normality test
library(nortest)
ad.test(model_lm$residuals)


# Autocorrelación H0 rho = 0
library(lmtest)
# Realizar el test de Durbin-Watson
resultado_dw <- dwtest(model_lm)
resultado_dw

# Homocedasticidad
#Test de Breusch-Pagan  H0 Homocedasticidad
bp_test <- bptest(model_lm)
print(bp_test)

# Puntos extremos
# Calcular el leverage de cada observación
leverage <- hatvalues(model_lm)
plot(X, leverage)
# Puntos de influence

# Calcular la distancia de Cook medir el cambio en b_hat

cook_dist <- cooks.distance(model_lm)
which.max(cooks.distance(model_lm))
cooks.distance(model_lm)[which.max(cooks.distance(model_lm))]
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

points(X[outlier], cook_dist[outlier], col = "red", pch = 20)
# Calcular DFFITS  cambio en Y_hat
dffits = dffits(model_lm)

plot(X,dffits, type = "p", pch = 20, col = "blue",
     main = "Gráfico de DFFITS",
     xlab = "Temperatura", ylab = "DFFITS",
     ylim = c(-.4, .2))
abline(h = 2 * sqrt(2/ length(dffits)), col = "red", lty = 2)
abline(h = -2 * sqrt(2/ length(dffits)), col = "red", lty = 2)

points(X[outlier], dffits[outlier], col = "red", pch = 20)

# Calcular DFBETAS  cambio en b_hat_j
dfbetas = dfbetas(model_lm)

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

covratio = covratio(model_lm)

plot(X,covratio, type = "p", pch = 20, col = "blue",
     main = "Gráfico de COVRATIO",
     #ylim = c(0.97, 1.03),
     xlab = "Temperatura", ylab = "COVRATIO")
abline(h = 1, col = "red", lty = 2)
abline(h = 1 - 3*2/length(covratio), col = "green", lty = 2)
abline(h = 1 + 3*2/length(covratio), col = "green", lty = 2)

points(X[outlier], covratio[outlier], col = "red", pch = 20)


## Sacamos los quintiles
X = data_ok$Ventas_NA
Y = data_ok$Ventas_Global
# Identifica los índices para eliminar el primer y quinto quintil
indices_mantener <- which(Y > quantile(Y, probs = .25) & Y < quantile(Y, probs = .75) &
                            X > quantile(X, probs = .25) & X < quantile(X, probs = .75))
X = data_ok$Ventas_NA[indices_mantener]
Y = data_ok$Ventas_Global[indices_mantener]
plot(X, Y,
     #xlim = c(0, 0.3), ylim = c(0, 0.3),
     xlab = "Ventas_NA", ylab = "Ventas_Global")


#X = X[-outlier]
#Y = Y[-outlier]
model_lm = lm(Y ~ X )
summary(model_lm)
mean(model_lm$residuals)

## Graficar los datos con la linea ajustada----
par(mfrow=c(1, 1))
plot(X, Y,
     #xlim = c(0, 0.3), ylim = c(0, 0.3),
     xlab = "EU_Sales", ylab = "NA_Sales")
abline(a = model_lm$coefficients[1],
       b = model_lm$coefficients[2], col = "blue", lwd = 3)

res = model_lm$residuals
residuos_est = rstandard(model_lm)

# Grafico de Residuos estandarizados vs valores predictores -----
plot(X, residuos_est,
     xlab = "EU_Sales", ylab = "Residuos",
     #ylim = c(-2.5,2.5),
     main = "Gráfico de Residuos vs. Valores Ajustados")
abline(h = 0, col = "red", lty = 2)



# Grafico de Residuos estandarizados vs valores ajustados -----
plot(model_lm$fitted.values, residuos_est,
     xlab = "Valores Ajustados", ylab = "Residuos",
     #ylim = c(-2.5,2.5),
     main = "Gráfico de Residuos vs. Valores Ajustados")
abline(h = 0, col = "red", lty = 2)
# Agregar las bandas de confianza al gráfico
lines(range(model_lm$fitted.values), c(banda_superior, banda_superior), lty = 2, col = "green")  # Banda superior
lines(range(model_lm$fitted.values), c(banda_inferior, banda_inferior), lty = 2, col = "green")  # Banda inferior

outlier = which(abs(residuos_est)> qt(.975, n-3))

# Graficar los puntos
plot(X, Y, xlab = "X", ylab = "Y", main = "Regresión Lineal Simple", pch = 16)
# Colorear los puntos con el índice guardado en index
points(X[outlier], Y[outlier], col = "red", pch = 20)
# Añadir leyenda
legend("topleft", legend = "Puntos extraños", pch = 20, col = "red", cex = .7)

# H0 Dist. Normal
qqnorm(res)
qqline(res)
shapiro.test(res)
#Anderson-Darling normality test
library(nortest)
ad.test(model_lm$residuals)


# Autocorrelación H0 rho = 0
library(lmtest)
# Realizar el test de Durbin-Watson
resultado_dw <- dwtest(model_lm)
resultado_dw

# Homocedasticidad
#Test de Breusch-Pagan  H0 Homocedasticidad
bp_test <- bptest(model_lm)
print(bp_test)


## Eliminando outlier

X = data_ok$Ventas_NA[indices_mantener]
Y = data_ok$Ventas_Global[indices_mantener]
plot(X, Y,
     #xlim = c(0, 0.3), ylim = c(0, 0.3),
     xlab = "Ventas_NA", ylab = "Ventas_Global")
points(X[outlier], Y[outlier], col = "red", pch = 20)

X = X[-outlier]
Y = Y[-outlier]


model_lm = lm(Y ~ X )
summary(model_lm)
mean(model_lm$residuals)

## Graficar los datos con la linea ajustada----
par(mfrow=c(1, 1))
plot(X, Y,
     #xlim = c(0, 0.3), ylim = c(0, 0.3),
     xlab = "Ventas_NA", ylab = "Ventas_Global")
abline(a = model_lm$coefficients[1],
       b = model_lm$coefficients[2], col = "blue", lwd = 3)

res = model_lm$residuals
residuos_est = rstandard(model_lm)

# Grafico de Residuos estandarizados vs valores predictores -----
plot(X, residuos_est,
     xlab = "EU_Sales", ylab = "Residuos",
     #ylim = c(-2.5,2.5),
     main = "Gráfico de Residuos vs. Valores Ajustados")
abline(h = 0, col = "red", lty = 2)



# H0 Dist. Normal
qqnorm(res)
qqline(res)
shapiro.test(res)
#Anderson-Darling normality test
library(nortest)
ad.test(model_lm$residuals)


# Autocorrelación H0 rho = 0
library(lmtest)
# Realizar el test de Durbin-Watson
resultado_dw <- dwtest(model_lm)
resultado_dw

# Homocedasticidad
#Test de Breusch-Pagan  H0 Homocedasticidad
bp_test <- bptest(model_lm)
print(bp_test)

