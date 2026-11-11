#install.packages("rjags")
library(rjags)

##################
## Ejemplo 1 #####
##################

n = 100 
k = sample(1:n,1)
print(k)
theta = 1
lambda = 1.5
Y1 = rpois(k, lambda = theta)
Y2 = rpois(n-k, lambda = lambda)
Y = c(Y1,Y2)
plot(ts(Y))
# Especificación del modelo en formato de texto
modelo <- "model {

  # Prior para los parámetros lambda de las dos muestras
  lambda[1] ~ dgamma(alpha1, beta1)
  lambda[2] ~ dgamma(alpha2, beta2)
  K ~ dcat(pk[])
  
  # Parámetros de las distribuciones Gamma a priori
  alpha1 ~ dgamma(0.5, 0.5)
  beta1  ~ dgamma(0.5, 0.5)
  alpha2 ~ dgamma(.5, 0.5)
  beta2 ~ dgamma(0.5, 0.5)
  
  # Verosimilitud Poisson para los datos de la muestra 1
  for(i in 1:N){

    eventos[i] ~ dpois(lambda[ind[i]])
    ind[i] <- 1 + step(i-K-0.01)
    pk[i] <- 1/N
}
  ratio <- lambda[1]/lambda[2]

}"


# Definir los datos para el modelo
datos_modelo <- list(eventos = Y, N = length(Y))

# Inicializar el modelo
modelo_jags <- jags.model(textConnection(modelo), data = datos_modelo, n.chains = 1)

# Actualización del modelo
actualizaciones <- 5000
actualizaciones_burnin <- 500
actualizaciones_totales <- actualizaciones + actualizaciones_burnin
update(modelo_jags, actualizaciones_burnin)
mcmc_samples <- coda.samples(modelo_jags, variable.names = c("lambda", "K", "ratio"),
                             n.iter = actualizaciones)

# Resumen del resultado
print(summary(mcmc_samples))

# Graficar la distribución posterior de lambda para ambas muestras
plot(mcmc_samples)

plot(density(mcmc_samples[[1]][,2]), col = "blue", ylim = c(0, 5), xlim = c(.6,13.2))
lines(density(mcmc_samples[[1]][,3]), col = "red")
#curve(dgamma(x, 1, 1), add = TRUE, col = "green")
hist(mcmc_samples[[1]][,1],breaks = 50, col = "blue")
mean(Y1)
mean(Y2)
k



##################
## Ejemplo 2 #####
##################

#install.packages("rstan")
library(StanHeaders)
library(rstan)
options(mc.cores=4)


# Definir el modelo en Stan
modelo_stan <- "
data {
  int<lower=0> N; // Número total de observaciones
  int<lower=0, upper=1> y[N]; // Vector de respuestas binarias (0 o 1)
  matrix[N,2] X; // Matriz de covariables
  //real lambda;         // Parámetro de penalización Lasso
}
parameters {
  real beta ;       // Coeficientes del modelo
  real intercept;     // Intercepto del modelo
  real<lower=0> lambda; // Parámetro de penalización Lasso
}

model {
// Priori normal para el intercepto
  intercept ~ normal(0, 100);
  
  // Priori plana para los coeficientes beta
  beta ~ double_exponential(0, 1/abs(lambda));

  // Verosimilitud logística
  y ~ bernoulli_logit(intercept + X[,2] * beta);
  
  // Priori Cauchy para el parámetro lambda
   lambda ~ cauchy(0, 1);
}
"
# Compilar el modelo
modelo_compilado <- stan_model(model_code = modelo_stan)


set.seed(123)
N <- 1000 # Número total de observaciones
X <- cbind(1,runif(N)) # Matriz de covariables (intercepto y variable aleatoria)
beta_true <- c(-1, 5) # Coeficientes del modelo verdaderos
prob <- plogis(X %*% beta_true) # Probabilidad de éxito para cada observación
y <- rbinom(N, size = 1, prob = prob) # Vector de respuestas binarias
lambda = 1# Parámetro de penalización Lasso # realizar ejemplo con lambda =100

# Preparar los datos para Stan
datos_stan <- list(N = N, y = y, X = X, lambda = lambda)

# Ajustar el modelo a los datos
ajuste_modelo <- sampling(modelo_compilado, data = datos_stan, chains = 1, 
                          warmup =500, iter = 10000)

# Resumen del ajuste
print(ajuste_modelo)

# Gráfico de diagnóstico
plot(ajuste_modelo)

params = extract(ajuste_modelo)
ts.plot(params$beta,xlab="Iterations",ylab="beta")
ts.plot(params$intercept,xlab="Iterations",ylab="intercept")
ts.plot(params$lambda,xlab="Iterations",ylab="lambda")
hist(params$beta, main="",xlab="beta")
hist(params$intercept, main="",xlab="intercept")
hist(params$lambda, main="",xlab="lambda")


plot(density(params$beta), col = "blue", ylim = c(0, .5), xlim = c(-10,10))
library(nimble)
curve(ddexp(x,0,1), col = "green", add = TRUE)

# Modelo clásico

# Ajustar el modelo de regresión logística
library(glmnet)

# Ajusta el modelo de regresión logística con penalización Lasso
##lasso_model <- glmnet(X, y, family = "binomial", alpha = 1)
cvlasso_model <- cv.glmnet(X, y, family = "binomial", alpha = 1)
plot(cvlasso_model)
coef(cvlasso_model, s = "lambda.min")

