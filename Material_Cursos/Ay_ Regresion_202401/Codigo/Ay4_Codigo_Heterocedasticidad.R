n = 1000
x = runif(n,1,2)
b0 = 2
b1 = 5
sigma2 = 1
e = rnorm(n,0,x^2*sigma2)
y = b0+b1*x+e

plot(x,y) 

model_lm = lm(y~x)
summary(model_lm)


# Agregar la recta de regresión
abline(model_lm, col = "blue")

# Obtener el intervalo de confianza de la recta media
int_conf <- predict(model_lm, interval = "confidence", level = 0.95)

# Agregar el intervalo de confianza de la recta media
lines(x, int_conf[, "lwr"], col = "red", lty = 2)  # Línea inferior
lines(x, int_conf[, "upr"], col = "red", lty = 2)  # Línea superior

# Obtener el intervalo de predicción
int_pred <- predict(model_lm, interval = "prediction", level = 0.95)

# Agregar el intervalo de predicción
lines(x, int_pred[, "lwr"], col = "green", lty = 2)  # Línea inferior
lines(x, int_pred[, "upr"], col = "green", lty = 2)  # Línea superior

R^2 = 1-SCE/SCT