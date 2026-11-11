#install.packages("rjags")
library(rjags)
library(readxl)

# Ejemplo 1 -----
# Datos de ejemplo (números de eventos observados)
datos <- c(3, 7, 8, 2, 5)

# Especificación del modelo en formato de texto
modelo <- "model {
  # Prior para el parámetro lambda (tasa de Poisson)
  lambda ~ dgamma(.001, .001)


  # Verosimilitud Poisson
  for (i in 1:N) {
    eventos[i] ~ dpois(lambda)
  }
}"

# Definir los datos para el modelo
datos_modelo <- list(eventos = datos, N = length(datos))

# Inicializar el modelo
modelo_jags <- jags.model(textConnection(modelo), data = datos_modelo, n.chains = 1)

# Actualización del modelo
actualizaciones <- 5000
actualizaciones_burnin <- 500
actualizaciones_totales <- actualizaciones + actualizaciones_burnin
mcmc_samples <- coda.samples(modelo_jags, variable.names = c("lambda"), n.iter = actualizaciones_totales)

# Resumen del resultado
print(summary(mcmc_samples))

# Graficar la distribución posterior de lambda
plot(mcmc_samples)

plot(density(mcmc_samples[[1]]))

curve(dgamma(x, .001, .001), add = TRUE, col = "red")
#aux = mcmc_samples[[1]]
#lines(density(aux), col = "blue")
#Clásico
mean(datos)


# Ejercio 1 ----
datos = read.csv('../../Ayudantia_BayesianoII/Ayudantias/Ayudantia2/Category4Hurricanes.csv')


datos_decadas = floor(datos$Season/10) * 10
frecuencia_decadas <- table(datos_decadas)
valores_frecuencia <- as.vector(frecuencia_decadas)
valores_frecuencia


# Especificación del modelo en formato de texto
modelo <- "model {
  # Prior para el parámetro lambda (tasa de Poisson)
  lambda ~ dgamma(.001, .001)

  # Verosimilitud Poisson
  for (i in 1:N) {
    eventos[i] ~ dpois(lambda)
  }
}"

# Definir los datos para el modelo
datos_modelo <- list(eventos = valores_frecuencia, N = length(valores_frecuencia))

# Inicializar el modelo
modelo_jags <- jags.model(textConnection(modelo), data = datos_modelo, n.chains = 1)

# Actualización del modelo
actualizaciones <- 5000
actualizaciones_burnin <- 500
actualizaciones_totales <- actualizaciones + actualizaciones_burnin
mcmc_samples <- coda.samples(modelo_jags, variable.names = c("lambda"), n.iter = actualizaciones_totales)

# Resumen del resultado
print(summary(mcmc_samples))

# Graficar la distribución posterior de lambda
plot(mcmc_samples)

plot(density(mcmc_samples[[1]]))
curve(dgamma(x, .001, .001), add = TRUE, col = "red")

# Clasico
mean(valores_frecuencia)
