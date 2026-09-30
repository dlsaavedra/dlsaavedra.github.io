#install.packages("palmerpenguins")
library(palmerpenguins)

data(penguins)
head(penguins)

# Eliminar filas con valores NA
datos = na.omit(penguins[, c("species", "bill_length_mm", "body_mass_g", "sex")])
datos$body_mass_Kg = datos$body_mass_g/1000
datos$bill_length_cm = datos$bill_length_mm / 10
datos$species_sex <- interaction(datos$species, datos$sex)

#
datos = datos[, c("species", "bill_length_cm", "body_mass_Kg", "sex")]
colnames(datos) = c("especies", "longitud_pico_cm", "masa_Kg", "sexo")
save(datos, file = "Ayudantia9/datos_pinguinos")

load("Ayudantia9/datos_pinguinos")


pairs(datos)
summary(datos)
str(datos)
head(datos)
cor(datos[, c("longitud_pico_cm", "masa_Kg")])

# Inicializar el gráfico
plot(datos$longitud_pico_cm,datos$masa_Kg, col = datos$especies, pch = 19,
     xlab = "Longitud del Pico (cm)", ylab = "Peso Corporal (Kg)",
     main = "Peso Corporal vs Longitud del Pico por Especie", xlim = c(2,7), ylim = c(2,7))
points(datos$longitud_pico_cm[datos$sexo == "male"],
       datos$masa_Kg [datos$sexo == "male"],pch = 2, col = "blue")
# Añadir leyenda
niveles_especies <- levels(datos$especies)
legend("topleft", legend = c(niveles_especies, "male"), col = c(1:length(niveles_especies), "blue"),
       pch = 19, lwd = 2, cex = 0.5)


# Ajustar el modelo de regresión lineal simple
modelo_penguins0 <- lm(masa_Kg ~longitud_pico_cm , data = datos)
summary(modelo_penguins0)
plot(datos$longitud_pico_cm,datos$masa_Kg, col = datos$especies, pch = 19,
     xlab = "Longitud del Pico (cm)", ylab = "Peso Corporal (Kg)",
     main = "Peso Corporal vs Longitud del Pico por Especie")
abline(modelo_penguins0, col = "blue")
plot(datos$longitud_pico_cm,modelo_penguins0$residuals)
library(lmtest)
shapiro.test(modelo_penguins0$residuals)
dwtest(modelo_penguins0)
bptest(modelo_penguins0)

## Ajustar el modelo de regresión lineal con misma pendiente diferente intercepto
# Crear la matriz de variables dummy
matriz_dummy <- model.matrix(~ especies - 1, data = datos)
head(matriz_dummy)
# Mostrar la matriz de variables dummy
X1 = cbind(datos$longitud_pico_cm, matriz_dummy)
colnames(X1)[1] = "longitud_pico_cm"
head(X1)
modelo_penguins1_alternativo <- lm(datos$masa_Kg ~ X1 - 1)
summary(modelo_penguins1_alternativo)

modelo_penguins1 <- lm(masa_Kg ~ longitud_pico_cm + especies,
                       data = datos)
summary(modelo_penguins1)
plot(datos$longitud_pico_cm,modelo_penguins1$residuals)
library(lmtest)
shapiro.test(modelo_penguins1$residuals)
dwtest(modelo_penguins1)
bptest(modelo_penguins1)


## Ajustar el modelo de regresión lineal con diferente pendiente misma intercepto
# Crear la matriz de variables dummy
matriz_dummy <- model.matrix(~ especies -1, data = datos)
matriz_dummy = matriz_dummy*datos$longitud_pico_cm
head(matriz_dummy)
# Mostrar la matriz de variables dummy
X2 = cbind(1, matriz_dummy)
colnames(X2)[1] = "Intercepto"
head(X2)
modelo_penguins2_alternativo <- lm(datos$masa_Kg ~ X2 - 1)
summary(modelo_penguins2_alternativo)

modelo_penguins2 <- lm(masa_Kg ~ longitud_pico_cm*especies - especies , data = datos)
summary(modelo_penguins2)

## Ajustar el modelo de regresión lineal con diferente pendiente e intercepto
# Crear la matriz de variables dummy
matriz_dummy <- model.matrix(~ especies -1, data = datos)x
matriz_dummy = cbind(matriz_dummy*datos$longitud_pico_cm, matriz_dummy)
# Mostrar la matriz de variables dummy
X3 = cbind(matriz_dummy)
colnames(X3)[1] = "longitud_pico_cm:especiesAdelie"
colnames(X3)[2] = "longitud_pico_cm:especiesChinstrap"
colnames(X3)[3] = "longitud_pico_cm:especiesGentoo"
head(X3)
modelo_penguins3_alternativo <- lm(datos$masa_Kg ~ X3 - 1)
summary(modelo_penguins3_alternativo)

modelo_penguins3 <- lm(masa_Kg ~ longitud_pico_cm*especies, data = datos)
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
R = cbind(1,0,0,0);R # Hipótesis que los pendiente = 1

m = length(R[,1])
X = X1#cbind(1, X1)
n_p = length(X[,1]) - length(X[1,])
r = c(2)
XtX_1 = solve(t(X)%*%X)
Ftest = t(R%*%betas_est - r)%*%solve(R%*%XtX_1%*%t(R))%*%(R%*%betas_est - r)/m/sigma^2; Ftest
alpha = 0.05
F_alpha = qf(1-alpha,m,n_p);F_alpha
p_vale = 1-pf(Ftest, m, n_p);p_vale

#R = cbind(0, diag(1, 3))
#Ftest_anova = betas_est[-1]%*%solve(R%*%XtX_1%*%t(R))%*%t(betas_est[-1])/(m-1)/sigma^2


vcov(modelo_penguins1)

# Predecir valores ajustados
datos$predicted_body_mass <- predict(modelo_penguins3)

# Inicializar el gráfico
plot(datos$longitud_pico_cm, datos$masa_Kg, col = datos$especies, pch = 19,
     xlab = "Longitud del Pico (cm)", ylab = "Peso Corporal (Kg)",xlim = c(2,7), ylim = c(2,7),
     main = "Peso Corporal vs Longitud del Pico por Especie")

# Añadir líneas de regresión ajustadas para cada especie

for (i in 1:length(niveles_especies)) {
  nivel_especies <- niveles_especies[i]
  subset_penguins <- datos[datos$especies == nivel_especies, ]
  lines(subset_penguins$longitud_pico_cm, subset_penguins$predicted_body_mass, col = i, lwd = 2)
}

# Añadir leyenda
legend("topleft", legend = niveles_especies, col = 1:length(niveles_especies),
       pch = 19, lwd = 2, cex = 0.7)

plot(datos$predicted_body_mass, modelo_penguins2$residuals, col = datos$especies)


library(lmtest)
shapiro.test(modelo_penguins2$residuals)
dwtest(modelo_penguins2)
bptest(modelo_penguins2)


