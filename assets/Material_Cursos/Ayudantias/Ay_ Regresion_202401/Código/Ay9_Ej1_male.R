
load("Ayudantia9/datos_pinguinos")


pairs(datos)
summary(datos)
str(datos)
head(datos)
cor(datos[, c("longitud_pico_cm", "masa_Kg")])

# Inicializar el gráfico
plot(datos$longitud_pico_cm, datos$masa_Kg, col = datos$sexo, pch = 19,
     xlab = "Longitud del Pico (cm)", ylab = "Peso Corporal (Kg)",
     main = "Peso Corporal vs Longitud del Pico por Especie", xlim = c(2,7), ylim = c(2,7))

# Añadir leyenda
niveles_sexo <- levels(datos$sexo)
legend("topleft", legend = c(niveles_sexo), col = c(1:length(niveles_sexo)),
       pch = 19, lwd = 2, cex = 0.7)


# Ajustar el modelo de regresión lineal simple
modelo_penguins0 <- lm(masa_Kg ~ longitud_pico_cm , data = datos)
summary(modelo_penguins0)
plot(datos$longitud_pico_cm, datos$masa_Kg, col = datos$sexo, pch = 19,
     xlab = "Longitud del Pico (cm)", ylab = "Peso Corporal (Kg)",
     main = "Peso Corporal vs Longitud del Pico por Especie", xlim = c(2,7), ylim = c(2,7))
abline(modelo_penguins0)
plot(datos$longitud_pico_cm,modelo_penguins0$residuals)
library(lmtest)
shapiro.test(modelo_penguins0$residuals)
dwtest(modelo_penguins0)
bptest(modelo_penguins0)

## Ajustar el modelo de regresión lineal con misma pendiente diferente intercepto
# Crear la matriz de variables dummy
matriz_dummy <- model.matrix(~ sexo -1, data = datos)
head(matriz_dummy)
# Mostrar la matriz de variables dummy
X1 = cbind(datos$longitud_pico_cm, matriz_dummy)
colnames(X1)[1] = "longitud_pico_cm"
head(X1)
modelo_penguins1_alternativo <- lm(datos$masa_Kg ~ X1 - 1)
summary(modelo_penguins1_alternativo)

modelo_penguins1 <- lm(masa_Kg ~ longitud_pico_cm + sexo -1, data = datos)
summary(modelo_penguins1)

## Ajustar el modelo de regresión lineal con diferente pendiente misma intercepto
# Crear la matriz de variables dummy
matriz_dummy <- model.matrix(~ sexo -1, data = datos)
matriz_dummy = matriz_dummy*datos$longitud_pico_cm
head(matriz_dummy)
# Mostrar la matriz de variables dummy
X2 = cbind(1, matriz_dummy)
colnames(X2)[1] = "Intercepto"
head(X2)
modelo_penguins2_alternativo <- lm(datos$masa_Kg ~ X2 - 1)
summary(modelo_penguins2_alternativo)

modelo_penguins2 <- lm(masa_Kg ~ longitud_pico_cm*sexo - sexo , data = datos)
summary(modelo_penguins2)

## Ajustar el modelo de regresión lineal con diferente pendiente e intercepto
# Crear la matriz de variables dummy
matriz_dummy <- model.matrix(~ sexo -1, data = datos)
matriz_dummy = cbind(matriz_dummy*datos$longitud_pico_cm, matriz_dummy)
# Mostrar la matriz de variables dummy
X3 = cbind(matriz_dummy)
colnames(X3)[1] = "longitud_pico_cm:sexofemale"
colnames(X3)[2] = "longitud_pico_cm:sexofemale"
head(X3)
modelo_penguins3_alternativo <- lm(datos$masa_Kg ~ X3 - 1)
summary(modelo_penguins3_alternativo)

modelo_penguins3 <- lm(masa_Kg ~ longitud_pico_cm*sexo, data = datos)
summary(modelo_penguins3)

#AIC(modelo_penguins1, modelo_penguins2, modelo_penguins3) # Menor AIC mejor
#BIC(modelo_penguins1, modelo_penguins2, modelo_penguins3) # Menor BIC mejor



# Realizar un análisis de varianza para el modelo
anova(modelo_penguins1)


betas_est = modelo_penguins1$coefficients
sigma = summary(modelo_penguins1)$sigma


#R = diag(1,4);R = R[-1,];R #Anova completo Test F del modelo
#R = cbind(0,0 ,diag(1,2));R #Anova para especies
#R = cbind(0,0, 0 ,diag(1,1));R #Anova para especiesGentoo
#R = diag(1,4);R = R[c(-1,-3, -4),];R = t(R);R# Anova creo para longitud_pico_cm
#R = diag(1,4);R = R[c(-1,-3),];R# Replicar anova para longitud
#R = cbind(0,0,1,-1);R # Hipótesis que los interceptos son iguales.
R = cbind(0,1,0,0);R # Hipótesis que los interceptos son iguales.

m = length(R[,1])
X = cbind(1, X1)
n_p = length(X[,1]) - length(X[1,])
r = c(1)
XtX_1 = solve(t(X)%*%X)
Ftest = t(R%*%betas_est - r)%*%solve(R%*%XtX_1%*%t(R))%*%(R%*%betas_est - r)/m/sigma^2; Ftest
alpha = 0.5
F_alpha = qf(1-alpha,m,n_p);F_alpha
p_vale = 1-pf(Ftest, m, n_p);p_vale

#R = cbind(0, diag(1, 3))
#Ftest_anova = betas_est[-1]%*%solve(R%*%XtX_1%*%t(R))%*%t(betas_est[-1])/(m-1)/sigma^2


vcov(modelo_penguins1)

# Predecir valores ajustados
datos$predicted_body_mass <- predict(modelo_penguins1)

# Inicializar el gráfico
plot(datos$longitud_pico_cm, datos$masa_Kg, col = datos$sexo, pch = 19,
     xlab = "Longitud del Pico (cm)", ylab = "Peso Corporal (Kg)",
     main = "Peso Corporal vs Longitud del Pico por Especie", xlim = c(2,7), ylim = c(2,7))

# Añadir líneas de regresión ajustadas para cada especie

for (i in 1:length(niveles_sexo)) {
  nivel_sexo <- niveles_sexo[i]
  subset_penguins <- datos[datos$sexo == nivel_sexo, ]
  lines(subset_penguins$longitud_pico_cm, subset_penguins$predicted_body_mass, col = i, lwd = 2)
}

# Añadir leyenda
legend("topleft", legend = niveles_sexo, col = 1:length(niveles_sexo),
       pch = 19, lwd = 2, cex = 0.7)

plot(datos$predicted_body_mass, modelo_penguins2$residuals, col = datos$especies)


library(lmtest)
shapiro.test(modelo_penguins2$residuals)
dwtest(modelo_penguins2)
bptest(modelo_penguins2)


