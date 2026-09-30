#install.packages("rjags")
library(rjags)
library(readxl)

# Ejemplo 1 -----
# Datos de ejemplo (números de eventos observados)
datos <- c(3, 7, 8, 2, 5)

# Especificación del modelo en formato de texto
modelo <- "model{
  # Prior para el parámetro lambda (tasa de Poisson)
  lambda ~ dgamma(alpha, beta)
  

  # Verosimilitud Poisson
  for (i in 1:N) {
    eventos[i] ~ dpois(lambda)
  }
}"

# Definir los datos para el modelo
datos_modelo <- list(eventos = datos, N = length(datos),alpha = 10, beta =1)

# Inicializar el modelo

modelo_jags <- jags.model(textConnection(modelo), data = datos_modelo, n.chains = 1)

# Actualización del modelo
actualizaciones <- 5000
actualizaciones_burnin <- 500
actualizaciones_totales <- actualizaciones + actualizaciones_burnin
mcmc_samples <- coda.samples(modelo_jags, variable.names = c("lambda"),
                             n.iter = actualizaciones_totales)

# Resumen del resultado
print(summary(mcmc_samples))

# Graficar la distribución posterior de lambda
plot(mcmc_samples)

plot(density(mcmc_samples[[1]]))
curve(dgamma(x, 10, 1), add = TRUE, col = "red")

mean(datos)

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
  beta1 ~ dgamma(0.01, 0.01)
  alpha2 ~ dgamma(0.01, 0.01)
  beta2 ~ dgamma(0.01, 0.01)


  # Verosimilitud Poisson para los datos de la muestra 1
  for (i in 1:N1) {
    eventos1[i] ~ dpois(lambda1)
  }

  # Verosimilitud Poisson para los datos de la muestra 2
  for (i in 1:N2) {
    eventos2[i] ~ dpois(lambda2)
  }
  ratio = lambda1/lambda2

}"

# Definir los datos para el modelo
datos_modelo <- list(eventos1 = datos1, eventos2 = datos2,
                     N1 = length(datos1), N2 = length(datos2))

# Inicializar el modelo
modelo_jags <- jags.model(textConnection(modelo), data = datos_modelo, n.chains = 1)

# Actualización del modelo
actualizaciones <- 4500
actualizaciones_burnin <- 500
# Quema de las primeras 500 muestras
update(modelo_jags, actualizaciones_burnin)

mcmc_samples <- coda.samples(modelo_jags, 
                             variable.names = c("lambda1", "lambda2", "ratio"), 
                             n.iter = actualizaciones, )

# Resumen del resultado
print(summary(mcmc_samples))

# Graficar la distribución posterior de lambda para ambas muestras
plot(mcmc_samples)

plot(density(mcmc_samples[[1]][,1]), col = "blue", ylim = c(0, 0.5))
lines(density(mcmc_samples[[1]][,2]), col = "red")

plot(density(mcmc_samples[[1]][,3]), col = "blue", ylim = c(0, 1.5))

#install.packages("rstan")
library(StanHeaders)
library(rstan)
options(mc.cores=4)

# Ejemplo 3----

# Definir el modelo en Stan
modelo_stan <- "
data {
  int<lower=0> N; // Número total de observaciones
  int<lower=0, upper=1> y[N]; // Vector de respuestas binarias (0 o 1)
  matrix[N, 2] X; // Matriz de covariables
}
parameters {
  vector[2] beta; // Coeficientes del modelo
}

model {
  // Priori plana para los coeficientes beta
  beta ~ normal(0, 1e3);

  // Verosimilitud logística
  y ~ bernoulli_logit(X * beta);
}
"

# Crear los datos de ejemplo
set.seed(123)
N <- 1000 # Número total de observaciones
X <- cbind(1, runif(N)) # Matriz de covariables (intercepto y variable aleatoria)
beta_true <- c(-1, 5) # Coeficientes del modelo verdaderos
prob <- plogis(X %*% beta_true) # Probabilidad de éxito para cada observación
y <- rbinom(N, size = 1, prob = prob) # Vector de respuestas binarias

# Preparar los datos para Stan
datos_stan <- list(N = N, y = y, X = X)

# Compilar el modelo
modelo_compilado <- stan_model(model_code = modelo_stan)

# Ajustar el modelo a los datos
ajuste_modelo <- sampling(modelo_compilado, data = datos_stan, warmup =500,
                          chains = 1, iter = 10000)

# Resumen del ajuste
print(ajuste_modelo)

# Gráfico de diagnóstico
plot(ajuste_modelo)

params = extract(ajuste_modelo)
ts.plot(params$beta[,1],xlab="Iterations",ylab="beta1")
ts.plot(params$beta[,2],xlab="Iterations",ylab="beta2")
hist(params$beta[,1], main="",xlab="beta1")
hist(params$beta[,2], main="",xlab="beta2")

plot(density(params$beta[,1]), col = "blue", ylim = c(0, 3), xlim = c(-5,10))
lines(density(params$beta[,2]), col = "red")
curve(dnorm(x,0,1e3), col = "green", add = TRUE)

