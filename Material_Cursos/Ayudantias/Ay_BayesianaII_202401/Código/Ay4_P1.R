library(StanHeaders)
library(rstan)
library(openxlsx)
options(mc.cores=16)

## ## ## ## ### ### ### ### ##


## Leer los datos #####
HC_data <- read.xlsx("Ayudantia4/Life_Expectancy_Data_NA.xlsx")
## Visualizar Datos------
str(HC_data)
head(HC_data)
summary(HC_data)
pairs(HC_data[3:10])
HC_data = HC_data[-1:-2]
#HC_data = HC_data[1:100,]
#X = cbind(1, HC_data[c('Adult.Mortality', 'Alcohol',
#                      'BMI', 'Schooling')])
#model_lm <- lm(Life.expectancy ~ Adult.Mortality + Alcohol +
#                 BMI + Schooling , data = HC_data)

X = cbind(1, scale(HC_data[-1]))
model_lm <- lm(HC_data$Life.expectancy ~ scale(HC_data[-1]))
summary(model_lm)


# Especificación del modelo en Stan-----
modelo_stan1 <- "
data {
  int<lower=0> N;         // Número de observaciones
  int<lower=0> P;         // Número de covariables
  matrix[N, P] covariables; // Matriz de covariables
  vector[N] y;            // Variable de respuesta
  real<lower=0> tau;
  real<lower=0> v0;
  real<lower=0> sigma02;
}

parameters {
  vector[P] beta;           // Coeficientes para las covariables
  real<lower=0> inv_sigma2; // Desviación estándar del error
}

model {
  
  // Prior para los parámetros
  
  inv_sigma2 ~ gamma(v0/2, v0*sigma02/2);  // Prior para sigma
  
  beta ~ normal(0, sqrt(tau)*1/sqrt(inv_sigma2));
  // Likelihood
  y ~ normal(covariables[,1:P] * beta, 1/sqrt(inv_sigma2)); // Modelo lineal con múltiples covariables
}

// Generar predicciones
generated quantities {
  vector[N] y_pred;
  real<lower=0> sigma2;
  sigma2 = 1/inv_sigma2;
  for (i in 1:N) {
    y_pred[i] = normal_rng(covariables[i,1:P]* beta, 1/sqrt(inv_sigma2)); // Predicciones
  }
  // Calcular la verosimilitud logarítmica para cada muestra
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(y[i] | covariables[i,1:P]* beta, 1/sqrt(inv_sigma2));
  }
}
"

# Crear un listado de datos para pasar al modelo
datos <- list(N = length(HC_data$Life.expectancy),
              P = ncol(X),
              covariables = X, 
              y = HC_data$Life.expectancy,
              tau = 10000, v0 = 1, sigma02 = 15)

# Compilar el modelo
modelo_compilado1 <- stan_model(model_code = modelo_stan1)

# Ajustar el modelo
ajuste_modelo1 <- sampling(modelo_compilado1, data = datos, 
                           chains = 1, warmup =500, iter = 10000)

param1 = extract(ajuste_modelo1)
# Resumen del ajuste
summary(param1$beta)
summary(param1$sigma2)

ts.plot(param1$beta[,1])
ts.plot(param1$beta[,2])
ts.plot(param1$beta[,3])
ts.plot(param1$beta[,4])
ts.plot(param1$beta[,5])
ts.plot(param1$beta[,6])
ts.plot(param1$beta[,7])
ts.plot(param1$beta[,8])
ts.plot(param1$sigma2)

pairs(param1$beta)





# Especificación priori plana sigma----
modelo_stan2 <- "
data {
  int<lower=0> N;         // Número de observaciones
  int<lower=0> P;         // Número de covariables
  matrix[N, P] covariables; // Matriz de covariables
  vector[N] y;            // Variable de respuesta
  real<lower=0> tau;
}

parameters {
  vector[P] beta;           // Coeficientes para las covariables
  real<lower=0> sigma2;    // Desviación estándar del error
  real<lower=0> sigma02;    // Desviación estándar del error del beta
}

model {
  // Prior para los parámetros
  beta ~ normal(0, sqrt(tau * sigma02));
  target += -log(sigma02);
  target += -log(sigma2);
  // Likelihood
  y ~ normal(covariables[,1:P] * beta, sqrt(sigma2)); // Modelo lineal con múltiples covariables
}

// Generar predicciones
generated quantities {
  vector[N] y_pred;
  for (i in 1:N) {
    y_pred[i] = normal_rng(covariables[i,1:P]* beta, sqrt(sigma2)); // Predicciones
  }
  // Calcular la verosimilitud logarítmica para cada muestra
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(y[i] | covariables[i,1:P]* beta, sqrt(sigma2));
  }
}
"

# Crear un listado de datos para pasar al modelo
datos2 <- list(N = length(HC_data$Life.expectancy),
              covariables = X, 
              P = ncol(X),
              y = HC_data$Life.expectancy,
              tau = 10000)

# Compilar el modelo
modelo_compilado2 <- stan_model(model_code = modelo_stan2)

# Ajustar el modelo
ajuste_modelo2 <- sampling(modelo_compilado2, data = datos2, 
                           chains = 1, warmup =500, iter = 10000)

param2 = extract(ajuste_modelo2)
# Resumen del ajuste
summary(param2$beta)
summary(param2$sigma2)

# Especificación priori g de Zellner-----
modelo_stan3 <- "
data {
  int<lower=0> N;         // Número de observaciones
  int<lower=0> P;         // Número de covariables
  matrix[N, P] covariables; // Matriz de covariables
  vector[N] y;            // Variable de respuesta
  matrix[P,P] inv_XtX;  //
  real<lower=0> g;
}

parameters {
  vector[P] beta;           // Coeficientes para las covariables
  real<lower=0> sigma2;    // Desviación estándar del error
}

model {
  // Prior para los parámetros
  beta ~ multi_normal(rep_vector(0, P), sigma2*g*inv_XtX);
  target += -log(sigma2);
  
  // Likelihood
  y ~ normal(covariables[,1:P] * beta, sqrt(sigma2)); // Modelo lineal con múltiples covariables
}

// Generar predicciones
generated quantities {
  vector[N] y_pred;
  for (i in 1:N) {
    y_pred[i] = normal_rng(covariables[i,1:P]* beta, sqrt(sigma2)); // Predicciones
  }
  // Calcular la verosimilitud logarítmica para cada muestra
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(y[i] | covariables[i,1:P]* beta, sqrt(sigma2));
  }
}
"
C = as.matrix(X)
inv_XtX = solve(t(C)%*%C)
# Crear un listado de datos para pasar al modelo
datos3 <- list(N = length(HC_data$Life.expectancy),
              covariables = X, 
              P = ncol(X),
              y = HC_data$Life.expectancy,
              inv_XtX = inv_XtX, g = length(HC_data$Life.expectancy))

# Compilar el modelo
modelo_compilado3 <- stan_model(model_code = modelo_stan3)

# Ajustar el modelo
ajuste_modelo3 <- sampling(modelo_compilado3, data = datos3, 
                           chains = 1, warmup =500, iter = 10000)

param3 = extract(ajuste_modelo3)
# Resumen del ajuste
summary(param3$beta)
pairs(param3$beta)
summary(param3$sigma2)
ts.plot(param3$sigma2)

# Especificación priori no informativa-----
modelo_stan4 <- "
data {
  int<lower=0> N;         // Número de observaciones
  int<lower=0> P;         // Número de covariables
  matrix[N,P] covariables; // Matriz de covariables
  vector[N] y;            // Variable de respuesta
}

parameters {
  vector[P] beta;           // Coeficientes para las covariables
  real<lower=0> sigma2;    // Desviación estándar del error
}

model {
  // Prior para los parámetros
  target += -log(sigma2);
  
  // Likelihood
  y ~ normal(covariables[,1:P] * beta, sqrt(sigma2)); // Modelo lineal con múltiples covariables
}

// Generar predicciones
generated quantities {
  vector[N] y_pred;
  for (i in 1:N) {
    y_pred[i] = normal_rng(covariables[i,1:P]* beta, sqrt(sigma2)); // Predicciones
  }
  // Calcular la verosimilitud logarítmica para cada muestra
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(y[i] | covariables[i,1:P]* beta, sqrt(sigma2));
  }
}
"
# Crear un listado de datos para pasar al modelo
datos4 <- list(N = length(HC_data$Life.expectancy),
               P = ncol(X),
               covariables = X, 
               y = HC_data$Life.expectancy)
# Compilar el modelo
modelo_compilado4 <- stan_model(model_code = modelo_stan4)

# Ajustar el modelo
ajuste_modelo4 <- sampling(modelo_compilado4, data = datos4, 
                           chains = 1, warmup =500, iter = 10000)

param4 = extract(ajuste_modelo4)
# Resumen del ajuste
summary(param4$beta)
summary(param4$sigma2)
pairs(param4$beta)

ts.plot(param4$beta[,1])
ts.plot(param4$beta[,2])
ts.plot(param4$beta[,3])
ts.plot(param4$beta[,4])
ts.plot(param4$beta[,5])
ts.plot(param4$beta[,6])
ts.plot(param4$beta[,7])
ts.plot(param4$beta[,8])
ts.plot(param4$sigma2)

pairs(param4$beta)
summary(param4$sigma2)
summary(param4$beta)
summary(model_lm)

hist(param4$beta[,5])
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
    log[i] = dnorm(y[i], X[i,]%*%sample[1:5], sqrt(sample[6]), log=T)
  }
  return (sum(log))
}

dic1 = dic(HC_data$Life.expectancy, as.matrix(X),loglikelihood_m1 ,postSamples = cbind(param1$beta,param1$sigma2))
dic2 = dic(HC_data$Life.expectancy, as.matrix(X),loglikelihood_m1 ,postSamples = cbind(param2$beta,param2$sigma2))
dic3 = dic(HC_data$Life.expectancy, as.matrix(X),loglikelihood_m1 ,postSamples = cbind(param3$beta,param3$sigma2))
dic4 = dic(HC_data$Life.expectancy, as.matrix(X),loglikelihood_m1 ,postSamples = cbind(param4$beta,param4$sigma2))

print(c(dic1, dic2, dic3,dic4))
## Menor DIC es mejor

# Criterio WAIC----
library(loo)
waic_m1 = waic(param1$log_lik)$waic
waic_m2 = waic(param2$log_lik)$waic
waic_m3 = waic(param3$log_lik)$waic
waic_m4 = waic(param4$log_lik)$waic

print(c(waic_m1, waic_m2, waic_m3,waic_m4))
#Escoger el modelo con valor más bajo de WAIC.

# Criterio LPML----
LPML_m1 = sum(-log(colMeans(1/exp(param1$log_lik))))
LPML_m2 = sum(-log(colMeans(1/exp(param2$log_lik))))
LPML_m3 = sum(-log(colMeans(1/exp(param3$log_lik))))
LPML_m4 = sum(-log(colMeans(1/exp(param4$log_lik))))

print(c(LPML_m1, LPML_m2, LPML_m3,LPML_m4))
#Escoger modelo con el mayor valor de LPML

#Criterio ECM----
ECM_m1 = mean((HC_data$Life.expectancy - colMeans(param1$y_pred))^2)
ECM_m2 = mean((HC_data$Life.expectancy - colMeans(param2$y_pred))^2)
ECM_m3 = mean((HC_data$Life.expectancy - colMeans(param3$y_pred))^2)
ECM_m4 = mean((HC_data$Life.expectancy - colMeans(param4$y_pred))^2)

print(c(ECM_m1, ECM_m2, ECM_m3, ECM_m4))
##escoger modelo con el menor valor de ECM