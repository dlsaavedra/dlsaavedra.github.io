library(openxlsx)
## ## ## ## ### ### ### ### ##

# Leer los datos #####
data = read.xlsx('Ayudantia5/Ay5_Datos_Curso.xlsx')

n = length(data$Tarea.N3)
data = na.omit(data)
# 0 Visualizar Datos------
str(data)
head(data)
summary(data)
pairs(data)
cor(data)

# Ajuste regresión----
model_lm = lm(I3 ~ Tarea.N3 , data = data)
summary(model_lm)
mean(model_lm$residuals)


## Graficar los datos con la linea ajustada----
par(mfrow=c(1, 1))
plot(data$Tarea.N3, data$I3,
     xlab = "Tarea.N3", ylab = "I3")
abline(a = model_lm$coefficients[1],
       b = model_lm$coefficients[2], col = "blue", lwd = 3)




# Grafico de Residuos -------
plot(model_lm)

res = model_lm$residuals
mean(res)

plot(res)
residuos_est = rstandard(model_lm)

# Grafico de Residuos estandarizados vs valores predictores -----
plot(data$Tarea.N3, residuos_est,
     xlab = "Tarea.N3", ylab = "I3",
     #ylim = c(-2.5,2.5),
     main = "Gráfico de Residuos vs. Valores Ajustados")
abline(h = 0, col = "red", lty = 2)

# Calcular las bandas de confianza para los residuos estandarizados
banda_superior <- qt(.975, n-3)
banda_inferior <- -qt(.975, n-3)

# Agregar las bandas de confianza al gráfico
lines(range(data$Tarea.N3), c(banda_superior, banda_superior), lty = 2, col = "green")  # Banda superior
lines(range(data$Tarea.N3), c(banda_inferior, banda_inferior), lty = 2, col = "green")  # Banda inferior

outlier = which(abs(residuos_est)> qt(.975, n-3))


# Graficar los puntos
plot(data$Tarea.N3, data$I3, xlab = "Tarea.N3", ylab = "I3", main = "Regresión Lineal Simple", pch = 16)
# Colorear los puntos con el índice guardado en index
points(data$Tarea.N3[outlier], data$I3[outlier], col = "red", pch = 20)
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


X = data$Tarea.N3
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
plot(data$Tarea.N3,cook_dist, type = "p", pch = 20, col = "blue",
     main = "Gráfico de Distancia de Cook",
     xlab = "Tarea.N3", ylab = "Distancia de Cook",
     ylim = c(0,1))

points(data$Tarea.N3[outlier], cook_dist[outlier], col = "red", pch = 20)
abline(h = pf(.95,2,n-2), col = "red", lty = 2)
# Añadir línea de referencia
plot(data$Tarea.N3,cook_dist, type = "p", pch = 20, col = "blue",
     main = "Gráfico de Distancia de Cook",
     xlab = "Tarea.N3", ylab = "Distancia de Cook")
abline(h = 4 / length(cook_dist), col = "red", lty = 2)

points(data$Tarea.N3[outlier], cook_dist[outlier], col = "red", pch = 20)
# Calcular DFFITS  cambio en Y_hat
dffits = dffits(model_lm)

plot(data$Tarea.N3, dffits, type = "p", pch = 20, col = "blue",
     main = "Gráfico de DFFITS",
     xlab = "Tarea.N3", ylab = "DFFITS",
     ylim = c(-.5, .5))
abline(h = 2 * sqrt(2/ length(dffits)), col = "red", lty = 2)
abline(h = -2 * sqrt(2/ length(dffits)), col = "red", lty = 2)

points(data$Tarea.N3[outlier], dffits[outlier], col = "red", pch = 20)

# Calcular DFBETAS  cambio en b_hat_j
dfbetas = dfbetas(model_lm)

plot(data$Tarea.N3, dfbetas[,1], type = "p", pch = 20, col = "blue",
     main = "Gráfico de DFBETAS Intercepto",
     xlab = "Tarea.N3", ylab = "DFBETAS Intercepto",
     ylim = c(-.3, .3))
abline(h = 2 * sqrt(1/ length(dffits)), col = "red", lty = 2)
abline(h = -2 * sqrt(1/ length(dffits)), col = "red", lty = 2)

points(data$Tarea.N3[outlier], dfbetas[outlier,1], col = "red", pch = 20)


plot(data$Tarea.N3,dfbetas[,2], type = "p", pch = 20, col = "blue",
     main = "Gráfico de DFBETAS PENDIENTE",
     xlab = "Tarea.N3", ylab = "DFBETAS",
     ylim = c(-.3, .3))
abline(h = 2 * sqrt(1/ length(dffits)), col = "red", lty = 2)
abline(h = -2 * sqrt(1/ length(dffits)), col = "red", lty = 2)

points(data$Tarea.N3[outlier], dfbetas[outlier,2], col = "red", pch = 20)

# Calcular COVRATIO
#Valores de COVRATIOi > 1 indican que la í-esima observación aumenta
#la precisión de los estimadores.
#Valores de COVRATIOi < 1 indican que la i  ́esima observaci  ́on dañ
#a la precisión de los estimadores.

covratio = covratio(model_lm)

plot(data$Tarea.N3,covratio, type = "p", pch = 20, col = "blue",
     main = "Gráfico de COVRATIO",
     ylim = c(0.8, 1.2),
     xlab = "Tarea.N3", ylab = "COVRATIO")
abline(h = 1, col = "red", lty = 2)
abline(h = 1 - 3*2/length(covratio), col = "green", lty = 2)
abline(h = 1 + 3*2/length(covratio), col = "green", lty = 2)

points(data$Tarea.N3[outlier], covratio[outlier], col = "red", pch = 20)

# Regresión con 2 covariables ----
## Correlación Parcial ------

cor(data)
cor(data$I2, data$I3)
model1 = lm(I3 ~ Tarea.N3, data = data)
# Residuos de model1 son las cosas no explicadas por Tarea.N3
# de la I3
model3 = lm(I2 ~ Tarea.N3, data = data)
# Residuos de model3 son las cosas no explicadas por Tarea.N3
# de la I2.
# I2 = b*Tarea.N3 -> Cor(I3, constante| Tarea.N3) = 0
# Correlación Parcial Cor(I3, I2| Tarea.N3)
cor(model1$residuals, model3$residuals)

plot(model1$residuals, model3$residuals)
#install.packages("ppcor")
library(ppcor)
# Calcular la correlación parcial
data_new = data[ ,c("I3", "I2", "Tarea.N3")]
pcor_results <- pcor(data_new)
print(pcor_results$estimate)

# Ajuste regresión----
#model_lm = lm(I3 ~  ., data = data)
#summary(model_lm)
#mean(model_lm$residuals)

model_lm = lm(I3 ~  I2 + Tarea.N3  , data = data)
summary(model_lm)

anova(model_lm)


res = model_lm$residuals
mean(res)

plot(res)
residuos_est = rstandard(model_lm)

# Grafico de Residuos estandarizados vs valores ajustados -----
plot(model_lm$fitted.values, residuos_est,
     xlab = "fitted.values", ylab = "residuos",
     ylim = c(-3.5,3.5),
     main = "Gráfico de Residuos vs. Valores Ajustados")
abline(h = 0, col = "red", lty = 2)


# Calcular las bandas de confianza para los residuos estandarizados
banda_superior <- qt(.975, n-4)
banda_inferior <- -qt(.975, n-4)

# Agregar las bandas de confianza al gráfico
lines(range(model_lm$fitted.values), c(banda_superior, banda_superior), lty = 2, col = "green")  # Banda superior
lines(range(model_lm$fitted.values), c(banda_inferior, banda_inferior), lty = 2, col = "green")  # Banda inferior

outlier = which(abs(residuos_est)> qt(.975, n-3))
points(model_lm$fitted.values[outlier], residuos_est[outlier], col = "red", pch = 20)


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


model_lm = lm(I3 ~  I2 + Tarea.N3  , data = data)
## Matriz Covarianza coeficiente------
matriz_covarianza_coef <- vcov(model_lm)

print(matriz_covarianza_coef)

sigma = summary(model_lm)$sigma
X = cbind(1,as.matrix(data[c("I2", "Tarea.N3")]))
matriz_cov_coef = sigma^2 * solve(t(X)%*%X)
print(matriz_cov_coef)

# Puntos extremos ----
# Calcular el leverage de cada observación
leverage <- hatvalues(model_lm)
plot(model_lm$fitted.values, leverage)
# Puntos de influence

# Calcular la distancia de Cook medir el cambio en b_hat

cook_dist <- cooks.distance(model_lm)
which.max(cooks.distance(model_lm))
cooks.distance(model_lm)[which.max(cooks.distance(model_lm))]
# Crear el gráfico
plot(model_lm$fitted.values,cook_dist, type = "p", pch = 20, col = "blue",
     main = "Gráfico de Distancia de Cook",
     xlab = "fitted_value", ylab = "Distancia de Cook",
     ylim = c(0,1))

points(model_lm$fitted.values[outlier], cook_dist[outlier], col = "red", pch = 20)
abline(h = pf(.95,2,n-2), col = "red", lty = 2)
# Añadir línea de referencia
plot(model_lm$fitted.values,cook_dist, type = "p", pch = 20, col = "blue",
     main = "Gráfico de Distancia de Cook",
     xlab = "fitted_value", ylab = "Distancia de Cook")
abline(h = 4 / length(cook_dist), col = "red", lty = 2)

points(model_lm$fitted.values[outlier], cook_dist[outlier], col = "red", pch = 20)
# Calcular DFFITS  cambio en Y_hat
dffits = dffits(model_lm)

plot(model_lm$fitted.values, dffits, type = "p", pch = 20, col = "blue",
     main = "Gráfico de DFFITS",
     xlab = "fitted_value", ylab = "DFFITS",
     ylim = c(-.4, .4))
abline(h = 2 * sqrt(2/ length(dffits)), col = "red", lty = 2)
abline(h = -2 * sqrt(2/ length(dffits)), col = "red", lty = 2)

points(model_lm$fitted.values[outlier], dffits[outlier], col = "red", pch = 20)

# Calcular DFBETAS  cambio en b_hat_j
dfbetas = dfbetas(model_lm)

plot(model_lm$fitted.values, dfbetas[,1], type = "p", pch = 20, col = "blue",
     main = "Gráfico de DFBETAS Intercepto",
     xlab = "fitted_value", ylab = "DFBETAS Intercepto",
     ylim = c(-.3, .3))
abline(h = 2 * sqrt(1/ length(dffits)), col = "red", lty = 2)
abline(h = -2 * sqrt(1/ length(dffits)), col = "red", lty = 2)

points(model_lm$fitted.values[outlier], dfbetas[outlier,1], col = "red", pch = 20)


plot(model_lm$fitted.values,dfbetas[,2], type = "p", pch = 20, col = "blue",
     main = "Gráfico de DFBETAS PENDIENTE",
     xlab = "fitted_value", ylab = "DFBETAS",
     ylim = c(-.3, .3))
abline(h = 2 * sqrt(1/ length(dffits)), col = "red", lty = 2)
abline(h = -2 * sqrt(1/ length(dffits)), col = "red", lty = 2)

points(model_lm$fitted.values[outlier], dfbetas[outlier,2], col = "red", pch = 20)

plot(model_lm$fitted.values,dfbetas[,3], type = "p", pch = 20, col = "blue",
     main = "Gráfico de DFBETAS PENDIENTE",
     xlab = "fitted_value", ylab = "DFBETAS",
     ylim = c(-.3, .3))
abline(h = 2 * sqrt(1/ length(dffits)), col = "red", lty = 2)
abline(h = -2 * sqrt(1/ length(dffits)), col = "red", lty = 2)

points(model_lm$fitted.values[outlier], dfbetas[outlier,3], col = "red", pch = 20)


# Calcular COVRATIO
#Valores de COVRATIOi > 1 indican que la í-esima observación aumenta
#la precisión de los estimadores.
#Valores de COVRATIOi < 1 indican que la i  ́esima observaci  ́on dañ
#a la precisión de los estimadores.

covratio = covratio(model_lm)

plot(model_lm$fitted,covratio, type = "p", pch = 20, col = "blue",
     main = "Gráfico de COVRATIO",
     #ylim = c(0.97, 1.03),
     xlab = "Tarea.N3", ylab = "COVRATIO")
abline(h = 1, col = "red", lty = 2)
abline(h = 1 - 3*2/length(covratio), col = "green", lty = 2)
abline(h = 1 + 3*2/length(covratio), col = "green", lty = 2)

points(model_lm$fitted[outlier], covratio[outlier], col = "red", pch = 20)


pairs(data)

# Eliminar punto 0 ----
L = data[(data$Tarea.N3 !=0) & (data$I3 !=0),]

# Ajuste regresión simple ----
model_lm = lm(I3 ~ Tarea.N3 , data = L)
summary(model_lm)


res = model_lm$residuals
mean(res)

plot(res)
residuos_est = rstandard(model_lm)

# Grafico de Residuos estandarizados vs valores predictores -----
plot(L$Tarea.N3, residuos_est,
     xlab = "Tarea.N3", ylab = "I3",
     #ylim = c(-2.5,2.5),
     main = "Gráfico de Residuos vs. Valores Ajustados")
abline(h = 0, col = "red", lty = 2)


# Calcular las bandas de confianza para los residuos estandarizados
banda_superior <- qt(.975, n-3)
banda_inferior <- -qt(.975, n-3)

# Agregar las bandas de confianza al gráfico
lines(range(L$Tarea.N3), c(banda_superior, banda_superior), lty = 2, col = "green")  # Banda superior
lines(range(L$Tarea.N3), c(banda_inferior, banda_inferior), lty = 2, col = "green")  # Banda inferior

outlier = which(abs(residuos_est)> qt(.975, n-3))


# Graficar los puntos
plot(L$Tarea.N3, L$I3, xlab = "Tarea.N3", ylab = "I3", main = "Regresión Lineal Simple", pch = 16)
# Colorear los puntos con el índice guardado en index
points(L$Tarea.N3[outlier], L$I3[outlier], col = "red", pch = 20)
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


# Eliminar punto 0 ----
L = data[(data$Tarea.N3 !=0) & (data$I3 !=0) & (data$I2 !=0),]

# Ajuste regresión----
model_lm = lm(I3 ~ I2 + Tarea.N3 , data = L)
summary(model_lm)


res = model_lm$residuals
mean(res)

plot(res)
residuos_est = rstandard(model_lm)

# Grafico de Residuos estandarizados vs valores predictores -----
plot(L$I3, residuos_est,
     xlab = "I3", ylab = "Residuos",
     ylim = c(-2.5,2.5),
     main = "Gráfico de Residuos vs. Valores Ajustados")
abline(h = 0, col = "red", lty = 2)


# Calcular las bandas de confianza para los residuos estandarizados
banda_superior <- qt(.975, n-3)
banda_inferior <- -qt(.975, n-3)

# Agregar las bandas de confianza al gráfico
lines(range(L$I3), c(banda_superior, banda_superior), lty = 2, col = "green")  # Banda superior
lines(range(L$I3), c(banda_inferior, banda_inferior), lty = 2, col = "green")  # Banda inferior

outlier = which(abs(residuos_est)> qt(.975, n-3))



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


# Ajuste regresión multiple ----
model_lm = lm(I3 ~ . , data = L)
summary(model_lm)

res = model_lm$residuals
mean(res)

plot(res)
residuos_est = rstandard(model_lm)

# Grafico de Residuos estandarizados vs valores predictores -----
plot(model_lm$fitted.values, residuos_est,
     xlab = "fitted_value", ylab = "I3",
     ylim = c(-2.5,2.5),
     main = "Gráfico de Residuos vs. Valores Ajustados")
abline(h = 0, col = "red", lty = 2)


# Calcular las bandas de confianza para los residuos estandarizados
banda_superior <- qt(.975, n-3)
banda_inferior <- -qt(.975, n-3)

# Agregar las bandas de confianza al gráfico
lines(range(model_lm$fitted.values), c(banda_superior, banda_superior), lty = 2, col = "green")  # Banda superior
lines(range(model_lm$fitted.values), c(banda_inferior, banda_inferior), lty = 2, col = "green")  # Banda inferior

outlier = which(abs(residuos_est)> qt(.975, n-3))


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

