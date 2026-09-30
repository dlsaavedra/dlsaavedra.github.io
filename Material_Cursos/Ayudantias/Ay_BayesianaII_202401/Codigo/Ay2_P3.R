# Instalar y cargar la librería rstan
#install.packages("rstan")
rm(list = ls())
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
N <- 100 # Número total de observaciones
X <- cbind(1, runif(N)) # Matriz de covariables (intercepto y variable aleatoria)
beta_true <- c(-1, 5) # Coeficientes del modelo verdaderos
prob <- plogis(X %*% beta_true) # Probabilidad de éxito para cada observación
y <- rbinom(N, size = 1, prob = prob) # Vector de respuestas binarias

# Preparar los datos para Stan
datos_stan <- list(N = N, y = y, X = X)

# Compilar el modelo
modelo_compilado <- stan_model(model_code = modelo_stan)

# Ajustar el modelo a los datos
ajuste_modelo <- sampling(modelo_compilado, data = datos_stan, chains = 1, 
                          warmup =500, iter = 5000)

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

# Modelo clásico

# Ajustar el modelo de regresión logística
modelo <- glm(y ~ X[,2],
              family = binomial(link = "logit"))

# Mostrar resumen del modelo
summary(modelo)


# Ejercicio 3----


load("Ayudantia2/ENS_completa.RData")
Y = as.numeric(ENS$hipertension) - 1
X = ENS[c("edad", "peso", "altura", "cintura", "imc")]
# Calcular la media de cada columna
#X <- log(X)
#medias <- colMeans(X)
# Centrar cada columna restando la media
X <- X - medias

N = length(Y)
# Definir el modelo en Stan
modelo_stan <- "
data {
  int<lower=0> N; // Número total de observaciones
  int<lower=0, upper=1> y[N]; // Vector de respuestas binarias (0 o 1)
  matrix[N, 5] X; // Matriz de covariables
}
parameters {
  vector[5] beta; // Coeficientes del modelo
}

model {
  // Priori plana para los coeficientes beta
  beta ~ normal(0, 1e6);

  // Verosimilitud logística
  y ~ bernoulli_logit(X * beta);
}
"

# Preparar los datos para Stan
datos_stan <- list(N = N, y = Y, X = X)

# Compilar el modelo
modelo_compilado <- stan_model(model_code = modelo_stan)

# Ajustar el modelo a los datos
ajuste_modelo <- sampling(modelo_compilado, data = datos_stan, 
                          warmup =500, chains = 1, iter = 5000)

# Resumen del ajuste
print(ajuste_modelo)
exp(-0.12 )
# Gráfico de diagnóstico
plot(ajuste_modelo)

params = extract(ajuste_modelo)
ts.plot(params$beta[,1],xlab="Iterations",ylab="beta1")
ts.plot(params$beta[,2],xlab="Iterations",ylab="beta2")
ts.plot(params$beta[,3],xlab="Iterations",ylab="beta3")
ts.plot(params$beta[,4],xlab="Iterations",ylab="beta4")
ts.plot(params$beta[,5],xlab="Iterations",ylab="beta5")
hist(params$beta[,1], main="",xlab="beta1", freq = FALSE)
hist(params$beta[,2], main="",xlab="beta2", freq = FALSE)
hist(params$beta[,3], main="",xlab="beta3", freq = FALSE)
hist(params$beta[,4], main="",xlab="beta4", freq = FALSE)
hist(params$beta[,5], main="",xlab="beta5", freq = FALSE)

plot(params$beta[,1], params$beta[,2])

plot(density(params$beta[,1]), col = "blue", ylim = c(0, 10), xlim = c(-2,2))
lines(density(params$beta[,2]), col = "red")
lines(density(params$beta[,3]), col = "black")
lines(density(params$beta[,4]), col = "yellow")
lines(density(params$beta[,5]), col = "cyan")
curve(dnorm(x,0,1e6), col = "green", add = TRUE)


# Modelo clásico

# Ajustar el modelo de regresión logística
modelo <- glm(Y ~ X$edad + X$peso + X$altura + X$cintura + X$imc,
              family = binomial(link = "logit"))

# Mostrar resumen del modelo
summary(modelo)


