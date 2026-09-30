|
datos = hubble
colnames(datos) = c("Galaxia","Velocidad","Distancia")

datos

sum(datos$Distancia^2)
sum(datos$Distancia * datos$Velocidad)
hmod <- lm(Velocidad ~ Distancia - 1, data = datos)

sum(hmod$res^2)

qt(0.975, 22)
qt(0.975, 23)
qt(0.975, 24)
qt(0.95, 22)
qt(0.95, 23)
qt(0.95, 24)


# Respuesta -----
D2 = sum(datos$Distancia^2)
H0 = sum(datos$Distancia * datos$Velocidad) / D2
sigma2 = sum(hmod$res^2)/(length(datos$Distancia) - 1)
T0 = sqrt(D2)*H0/sqrt(sigma2)
abs(T0) > qt(0.95, 23)

Dp = 15
delta = qt(0.975, 23)*sqrt(sigma2)*sqrt(1+ Dp**2/D2)
c(H0*15 - delta, H0*15 + delta)


# Obtener el intervalo de predicción para el nuevo valor
prediccion <- predict(hmod, newdata = data.frame(Distancia = Dp), interval = "prediction")
prediccion


# Análisis de Residuos -----

plot(hmod$residuals)
plot(ts(hmod$residuals))

# Gráfico de dispersión de residuos vs. valores ajustados
plot(hmod$fitted.values, hmod$residuals,
     xlab = "Valores Ajustados", ylab = "Residuos",
     main = "Gráfico de Residuos vs. Valores Ajustados")
abline(h = 0, col = "red", lty = 2)

# Histograma de los residuos
hist(hmod$residuals, main = "Histograma de Residuos",
     xlab = "Residuos", ylab = "Frecuencia")

# Gráfico de cuantiles normales
qqnorm(hmod$residuals, main = "Gráfico de Cuantiles Normales")
qqline(hmod$residuals)




# Realiza el test de Durbin-Watson H0:Homocedasticidad
dwtest(hmod)
acf(hmod$residuals)
