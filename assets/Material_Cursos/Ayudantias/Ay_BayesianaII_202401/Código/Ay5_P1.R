#install.packages('haven')
library(haven)
library(rstan)
options(mc.cores=8)
# DATA -----
#https://www.kaggle.com/code/gauravduttakiit/explore-the-poisson-regression
data <- read.csv("Ayudantia5/competition_awards_data.csv")
summary(data)
plot(data$Math.Score, data$Awards)

# MODELO CLÁSICO -----
# Ajustar el modelo de regresión de Poisson
modelo <- glm(Awards ~ Math.Score, data = data, family = poisson)
# Mostrar un resumen del modelo
summary(modelo)

# STAN MODEL1 -----

#data_cov = data[, !names(data2) %in% "ncit"]
# Convertir la matriz de diseño y la variable de respuesta a formato Stan
datos_stan <- list(N = nrow(data), eventos = data$Awards, X = as.matrix(data$Math.Score), K = 1)


# Definir el modelo en Stan
modelo_stan1 <- "
data {
  int<lower=0> N; // Número de observaciones
  int<lower=0> K; // Número de covariables
  int<lower=0> eventos[N]; // Variable de respuesta (eventos)
  matrix[N, K] X; // Matriz de diseño de variables explicativas
}
parameters {
  real alpha; // Intercepto
  vector[K] beta; // Coeficientes de las variables explicativas
}
model {
  // Prior
  alpha ~ normal(1, 10);
  beta ~ normal(1, 10);
  
  // Likelihood
  for (i in 1:N) {
    eventos[i] ~ poisson_log(alpha + dot_product(X[i], beta));
  }
}

// Generar predicciones
generated quantities {
  vector[N] y_pred;
  for (i in 1:N) {
    y_pred[i] = poisson_rng(exp(alpha + dot_product(X[i], beta))); // Predicciones
  }
  // Calcular la verosimilitud logarítmica para cada muestra
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = poisson_lpmf(eventos[i] | exp(alpha + dot_product(X[i], beta)));
  }
}

"

# Compilar el modelo
modelo_compilado1 <- stan_model(model_code = modelo_stan1)

# Ajustar el modelo a los datos
ajuste1 <- sampling(modelo_compilado1, data = datos_stan,
                   chains = 1, warmup =5000, iter = 15000)

# Mostrar un resumen del ajuste
#print(ajuste1)
param1 = extract(ajuste1)
summary(cbind(param1$alpha, param1$beta))

ts.plot(param1$beta[,1],xlab="Iterations",ylab="beta1")
ts.plot(param1$alpha,xlab="Iterations",ylab="intercept")
pairs(cbind(param1$beta, param1$alpha))

# MODELO 2 POISSON -----


# Definir el modelo en Stan
modelo_stan2 <- "
data {
  int<lower=0> N; // Número de observaciones
  int<lower=0> K; // Número de covariables
  int<lower=0> eventos[N]; // Variable de respuesta (eventos)
  matrix[N, K] X; // Matriz de diseño de variables explicativas
}
parameters {
  real alpha; // Intercepto
  vector[K] beta; // Coeficientes de las variables explicativas
  real<lower=0> lambda; // Parámetro de penalización Lasso
}
model {
  // Prior
  alpha ~ normal(1, 10);
   // Priori plana para los coeficientes beta
  beta ~ double_exponential(1, 1/lambda);
  // Priori Cauchy para el parámetro lambda
   lambda ~ cauchy(0, 1);

  // Likelihood
  for (i in 1:N) {
    eventos[i] ~ poisson_log(alpha + dot_product(X[i], beta));
  }
}

// Generar predicciones
generated quantities {
  vector[N] y_pred;
  for (i in 1:N) {
    y_pred[i] = poisson_rng(exp(alpha + dot_product(X[i], beta))); // Predicciones
  }
  // Calcular la verosimilitud logarítmica para cada muestra
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = poisson_log_lpmf(eventos[i] | alpha + dot_product(X[i], beta));
  }
}

"

# Compilar el modelo
modelo_compilado2 <- stan_model(model_code = modelo_stan2)

# Ajustar el modelo a los datos
ajuste2 <- sampling(modelo_compilado2, data = datos_stan,
                    chains = 1, warmup =5000, iter = 15000)

# Mostrar un resumen del ajuste
#print(ajuste2)
param2 = extract(ajuste2)
summary(cbind(param2$alpha, param2$beta))


ts.plot(param2$beta[,1],xlab="Iterations",ylab="beta1")
ts.plot(param2$alpha,xlab="Iterations",ylab="intercept")
pairs(cbind(param2$beta, param2$alpha, param2$lambda))


# Criterio DIC -----

dic <- function(y, X, loglikelihood, postSamples){
  
  log_link <- function(theta){loglikelihood(y, X, theta)}
  thetaBayes <- colMeans(postSamples)
  pDIC <- 2*(log_link(thetaBayes) - mean(apply(postSamples, 1, log_link) ))
  dic <- -2*log_link(thetaBayes) + 2*pDIC
  
  return (dic)
}
loglikelihood_m1 <- function(y, X, sample){
  log = rep(0, length(y))
  for (i in 1:length(y)){
    log[i] = dpois(y[i], exp(sample[1] + X[i,]%*%sample[2]), log=T)
  }
  return (sum(log))
}

dic1 = dic(data$Awards, as.matrix(data$Math.Score),loglikelihood_m1 ,postSamples = cbind(param1$alpha,param1$beta))
dic2 = dic(data$Awards, as.matrix(data$Math.Score),loglikelihood_m1 ,postSamples = cbind(param2$alpha,param2$beta))


print(c(dic1, dic2))
## Menor DIC es mejor

# Criterio WAIC----
library(loo)
waic_m1 = waic(param1$log_lik)$waic
waic_m2 = waic(param2$log_lik)$waic


print(c(waic_m1, waic_m2))
#Escoger el modelo con valor más bajo de WAIC.

# Criterio LPML----
LPML_m1 = sum(-log(colMeans(1/exp(param1$log_lik))))
LPML_m2 = sum(-log(colMeans(1/exp(param2$log_lik))))


print(c(LPML_m1, LPML_m2))
#Escoger modelo con el mayor valor de LPML


#Criterio ECM----

ECM_m1 = mean((data$Awards - colMeans(param1$y_pred))^2)
ECM_m2 = mean((data$Awards - colMeans(param2$y_pred))^2)
ECM_classic = mean((data$Awards - predict(modelo, data, type = "response"))^2)


print(c(ECM_m1, ECM_m2, ECM_classic))
##escoger modelo con el menor valor de ECM