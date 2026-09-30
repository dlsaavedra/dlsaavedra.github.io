# Instalar y cargar la librería rstan
#install.packages("rstan")
library(StanHeaders)
library(rstan)
options(mc.cores=4)


##################
## Ejemplo 2 #####
##################


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
N <- 100 # Número total de observaciones
X <- cbind(1,runif(N)) # Matriz de covariables (intercepto y variable aleatoria)
beta_true <- c(-1, 5) # Coeficientes del modelo verdaderos
prob <- plogis(X %*% beta_true) # Probabilidad de éxito para cada observación
y <- rbinom(N, size = 1, prob = prob) # Vector de respuestas binarias
lambda = 1# Parámetro de penalización Lasso # realizar ejemplo con lambda =100

# Preparar los datos para Stan
datos_stan <- list(N = N, y = y, X = X)#, lambda = lambda)

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




##################
##Ejercicio 2#####
##################


load("Ayudantia2/ENS_completa.RData")
Y = as.numeric(ENS$hipertension) - 1
X = ENS[c("edad", "peso", "altura", "cintura", "imc")]
# Calcular la media de cada columna
#X <- log(X)
#medias <- colMeans(X)
#sds <- apply(X, 2, sd)
# Centrar cada columna restando la media
#X <- scale(X)


N = length(Y)

# Definir el modelo en Stan
modelo_stan <- "
data {
  int<lower=0> N; // Número total de observaciones
  int<lower=0, upper=1> y[N]; // Vector de respuestas binarias (0 o 1)
  matrix[N,5] X; // Matriz de covariables
  //real lambda;         // Parámetro de penalización Lasso
}
parameters {
  vector[5] beta ;       // Coeficientes del modelo
  real intercept;     // Intercepto del modelo
  vector[5] lambda; // Parámetro de penalización Lasso
  //real lambda;
}

model {
// Priori normal para el intercepto
  intercept ~ normal(0, 100);
  
  // Priori plana para los coeficientes beta
  for (i in 1:5) {
  beta[i] ~ double_exponential(0, 1/abs(lambda[i]));
  }
  // Verosimilitud logística
  y ~ bernoulli_logit(intercept + X[,1:5] * beta);
  
  // Priori Cauchy para el parámetro lambda
   lambda ~ cauchy(0, 1);
}
"
# Compilar el modelo
modelo_compilado <- stan_model(model_code = modelo_stan)

# Preparar los datos para Stan
datos_stan <- list(N = N, y = Y, X = X)


# Ajustar el modelo a los datos
ajuste_modelo <- sampling(modelo_compilado, data = datos_stan, 
                          warmup =500, chains = 1, iter = 10000)

# Resumen del ajuste
print(ajuste_modelo)
exp(0.02)
# Gráfico de diagnóstico
plot(ajuste_modelo)

params = extract(ajuste_modelo)
ts.plot(params$beta[,1],xlab="Iterations",ylab="beta1")
ts.plot(params$beta[,2],xlab="Iterations",ylab="beta2")
ts.plot(params$beta[,3],xlab="Iterations",ylab="beta3")
ts.plot(params$beta[,4],xlab="Iterations",ylab="beta4")
ts.plot(params$intercept,xlab="Iterations",ylab="intercept")
hist(params$beta[,1], main="",xlab="beta1", freq = FALSE)
hist(params$beta[,2], main="",xlab="beta2", freq = FALSE)
hist(params$beta[,3], main="",xlab="beta3", freq = FALSE)
hist(params$beta[,4], main="",xlab="beta4", freq = FALSE)
hist(params$intercept, main="",xlab="intercept", freq = FALSE)

plot(params$beta[,1], params$beta[,2])
pairs(params$beta[,1:5])
plot(density(params$beta[,1]), col = "blue", ylim = c(0, 10), xlim = c(-2,2))
lines(density(params$beta[,2]), col = "red")
lines(density(params$beta[,3]), col = "black")
lines(density(params$beta[,4]), col = "yellow")
lines(density(params$beta[,5]), col = "cyan")
curve(dnorm(x,0,1e6), col = "green", add = TRUE)


# Modelo clásico
# Ajusta el modelo de regresión logística con penalización Lasso
##lasso_model <- glmnet(X, y, family = "binomial", alpha = 1)
cvlasso_model <- cv.glmnet(as.matrix(X), Y, family = "binomial", alpha = 1)
plot(cvlasso_model)
coef(cvlasso_model, s = "lambda.min")

#####################
##### WAIC STAN #####
#####################
library(loo)

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
   // Sección para calcular la verosimilitud logarítmica
    generated quantities {
  // Calcular la verosimilitud logarítmica para cada punto de datos
    vector[N] log_lik;
    for (i in 1:N) {
      log_lik[i] = bernoulli_logit_lpmf(y[i] | intercept + X[i,2] * beta); // Por ejemplo, si tienes una distribución Bernoulli
  }
}
"

set.seed(123)
N <- 100 # Número total de observaciones
X <- cbind(1,runif(N)) # Matriz de covariables (intercepto y variable aleatoria)
beta_true <- c(-1, 5) # Coeficientes del modelo verdaderos
prob <- plogis(X %*% beta_true) # Probabilidad de éxito para cada observación
y <- rbinom(N, size = 1, prob = prob) # Vector de respuestas binarias

# Preparar los datos para Stan
datos_stan <- list(N = N, y = y, X = X)#, lambda = lambda)
# Compilar el modelo
modelo_compilado <- stan_model(model_code = modelo_stan)
# Ajustar el modelo a los datos

ajuste_modelo <- sampling(modelo_compilado, data = datos_stan, 
                          warmup =500, chains = 1, iter = 10000)

# Calcular WAIC
waic <- loo(ajuste_modelo)
# Obtener los resultados de WAIC
print(waic)
log_lik1 <- extract_log_lik(ajuste_modelo)
waic1 <- waic(log_lik1)
# Calcular WAIC,AIC,DIC,BIC
WAIC = waic$looic
p = 3
D = rowSums(-2*log_lik1)
DIC = .5*var(D) + mean(D)
AIC = 2*p + which.max(D)
BIC = p*log(N) + which.max(D)
