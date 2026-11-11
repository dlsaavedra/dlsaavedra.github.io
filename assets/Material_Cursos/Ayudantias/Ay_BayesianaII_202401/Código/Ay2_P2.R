library(rjags)
library(readxl)


# Ejemplo 2----
# Datos de ejemplo (números de eventos observados en dos muestras)
datos1 <- c(3, 7, 8, 2, 5)
datos2 <- c(2, 6, 5, 3, 4)

# Especificación del modelo en formato de texto
modelo <- "model {
  # Prior para los parámetros lambda de las dos muestras
  lambda1 ~ dgamma(alpha1, beta1)
  lambda2 ~ dgamma(alpha2, beta2)

  # Parámetros de las distribuciones Gamma a priori
  alpha1 ~ dgamma(0.01, 0.01)
  beta1  ~ dgamma(0.01, 0.01)
  alpha2 ~ dgamma(0.01, 0.01)
  beta2 ~ dgamma(0.01, 0.01)
  
  #alpha1 = 1
  #beta1  = 1
  #alpha2 = 1
  #beta2 = 1
  
  
  # Verosimilitud Poisson para los datos de la muestra 1
  for (i in 1:N1) {
    eventos1[i] ~ dpois(lambda1)
  }

  # Verosimilitud Poisson para los datos de la muestra 2
  for (i in 1:N2) {
    eventos2[i] ~ dpois(lambda2)
  }


}"

# Definir los datos para el modelo
datos_modelo <- list(eventos1 = datos1, eventos2 = datos2,
                     N1 = length(datos1), N2 = length(datos2))

# Inicializar el modelo
modelo_jags <- jags.model(textConnection(modelo), data = datos_modelo, n.chains = 1)

# Actualización del modelo
actualizaciones <- 5000
actualizaciones_burnin <- 500
actualizaciones_totales <- actualizaciones + actualizaciones_burnin
mcmc_samples <- coda.samples(modelo_jags, variable.names = c("lambda1", "lambda2"), n.iter = actualizaciones_totales)

# Resumen del resultado
print(summary(mcmc_samples))

# Graficar la distribución posterior de lambda para ambas muestras
plot(mcmc_samples)

plot(density(mcmc_samples[[1]][,1]), col = "blue", ylim = c(0, 0.5))
lines(density(mcmc_samples[[1]][,2]), col = "red")
#curve(dgamma(x, 1, 1), add = TRUE, col = "green")
mean(datos1)
mean(datos2)

# Ejercicio 2 ----

datos4 = read.csv('../../Ayudantia_BayesianoII/Ayudantias/Ayudantia2/Category4Hurricanes.csv')
datos_decadas4 = floor(datos4$Season[1920 <= datos4$Season]/10) * 10
frecuencia_decadas4 <- table(datos_decadas4)
frecuencia_decadas4
valores_frecuencia4 <- as.vector(frecuencia_decadas4)
valores_frecuencia4

datos5 = read.csv('../../Ayudantia_BayesianoII/Ayudantias/Ayudantia2/Category5Hurricanes.csv')
fechas = datos5$Dates.as.aCategory.5
año <- as.numeric(regmatches(fechas, regexpr("\\d{4}", fechas)))
datos_decadas5 = floor(año/10) * 10
frecuencia_decadas5 <- table(datos_decadas5)
frecuencia_decadas5
valores_frecuencia5 <- as.vector(frecuencia_decadas5)
valores_frecuencia5



# Definir los datos para el modelo
datos_modelo <- list(eventos1 = valores_frecuencia4, eventos2 = valores_frecuencia5,
                     N1 = length(valores_frecuencia4), N2 = length(valores_frecuencia5))

# Inicializar el modelo
modelo_jags <- jags.model(textConnection(modelo), data = datos_modelo, n.chains = 1)

# Actualización del modelo
actualizaciones <- 1000
actualizaciones_burnin <- 500
actualizaciones_totales <- actualizaciones + actualizaciones_burnin
mcmc_samples <- coda.samples(modelo_jags, variable.names = c("lambda1", "lambda2"), n.iter = actualizaciones_totales)

# Resumen del resultado
print(summary(mcmc_samples))

# Graficar la distribución posterior de lambda para ambas muestras
plot(mcmc_samples)

plot(density(mcmc_samples[[1]][,1]), col = "blue", ylim = c(0, 0.8), xlim = c(2,13))
lines(density(mcmc_samples[[1]][,2]), col = "red")


