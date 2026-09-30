#install.packages('haven')
library(haven)
library(rstan)
options(mc.cores=8)
# DATA -----
#https://www.kaggle.com/datasets/new-york-city/nyc-east-river-bicycle-crossings
data <- read.csv("Ayudantia5/Ay5_nyc-east-river-bicycle-counts.csv")
summary(data)
str(data)
data$Precipitation[data$Precipitation == "0.47 (S)"] = "0.47"
data$Precipitation[data$Precipitation == "T"] = "0"
data$Precipitation = as.numeric(data$Precipitation)
# MODELO CLÁSICO -----
# Ajustar el modelo de regresión de Poisson
modelo <- glm(Manhattan.Bridge ~ High.Temp...F.+ Low.Temp...F. + Precipitation, data = data, family = poisson(link = "log"))
# Mostrar un resumen del modelo
summary(modelo)

# STAN MODEL1 -----

#data_cov = data[, !names(data2) %in% "ncit"]


# Definir el modelo en Stan
modelo_stan1 <- "
data {
  int<lower=0> N; // Número de observaciones
  int<lower=0> K; // Número de covariables
  int<lower=0> eventos[N]; // Variable de respuesta (eventos)
  matrix[N, K] X; // Matriz de diseño de variables explicativas
  real mu0; // media priori
  real sigma02; // varianza priori
}
parameters {
  real alpha; // Intercepto
  vector[K] beta; // Coeficientes de las variables explicativas
}
model {
  // Prior
  alpha ~ normal(mu0, sqrt(sigma02));
  beta ~ normal(mu0, sqrt(sigma02));
  
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
# Convertir la matriz de diseño y la variable de respuesta a formato Stan
datos_stan <- list(N = nrow(data), eventos = data$Manhattan.Bridge, 
                   X = as.matrix(cbind(data$High.Temp...F.,data$Low.Temp...F., data$Precipitation)), 
                   K = 3, mu0 = 10, sigma02 = 100)

# Compilar el modelo
modelo_compilado1 <- stan_model(model_code = modelo_stan1)

# Ajustar el modelo a los datos
ajuste1 <- sampling(modelo_compilado1, data = datos_stan,
                    chains = 1, warmup =5000, iter = 15000)

# Mostrar un resumen del ajuste
#print(ajuste1)
param1 = extract(ajuste1)
summary(cbind(param1$alpha, param1$beta, param2$lambda))

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
  real mu0; // media priori
  real sigma02; // varianza priori
}
parameters {
  real alpha; // Intercepto
  vector[K] beta; // Coeficientes de las variables explicativas
  real<lower=0> lambda; // Parámetro de penalización Lasso
}
model {
  // Prior
  alpha ~ normal(mu0, sqrt(sigma02));
   // Priori plana para los coeficientes beta
  beta ~ double_exponential(mu0, 1/lambda);
  // Priori Cauchy para el parámetro lambda
   lambda ~ cauchy(1, 1);

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
summary(cbind(param2$alpha, param2$beta, param2$lambda))


ts.plot(param2$beta[,1],xlab="Iterations",ylab="beta1")
ts.plot(param2$alpha,xlab="Iterations",ylab="intercept")
pairs(cbind(param2$alpha, param2$beta, param2$lambda))


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
    log[i] = dpois(y[i], exp(sample[1] + X[i,]%*%sample[2:4]), log=T)
  }
  return (sum(log))
}

dic1 = dic(data$Manhattan.Bridge, as.matrix(cbind(data$High.Temp...F.,data$Low.Temp...F., data$Precipitation)),
           loglikelihood_m1 ,postSamples = cbind(param1$alpha,param1$beta))
dic2 = dic(data$Manhattan.Bridge, as.matrix(cbind(data$High.Temp...F.,data$Low.Temp...F., data$Precipitation)),
           loglikelihood_m1 ,postSamples = cbind(param2$alpha,param2$beta))


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

ECM_m1 = mean((data$Manhattan.Bridge - colMeans(param1$y_pred))^2)
ECM_m2 = mean((data$Manhattan.Bridge - colMeans(param2$y_pred))^2)
ECM_classic = mean((data$Manhattan.Bridge - predict(modelo, data, type = "response"))^2)


print(c(ECM_m1, ECM_m2, ECM_classic))
##escoger modelo con el menor valor de ECM