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

X = cbind(1, HC_data[-1])
model_lm <- lm(Life.expectancy ~ . , data = HC_data)
summary(model_lm)


# Especificación del modelo en Stan-----
modelo_stan1 <- "
data {
  int<lower=0> N;         // Número de observaciones
  matrix[N, 5] covariables; // Matriz de covariables
  vector[N] y;            // Variable de respuesta
  real<lower=0> tau;
  real<lower=0> v0;
  real<lower=0> sigma02;
}

parameters {
  vector[5] beta;           // Coeficientes para las covariables
  real<lower=0> inv_sigma2; // Desviación estándar del error
}

model {
  
  // Prior para los parámetros
  
  inv_sigma2 ~ gamma(v0/2, v0*sigma02/2);  // Prior para sigma
  
  beta ~ normal(10, sqrt(tau)*1/sqrt(inv_sigma2));
  // Likelihood
  y ~ normal(covariables[,1:5] * beta, 1/sqrt(inv_sigma2)); // Modelo lineal con múltiples covariables
}

// Generar predicciones
generated quantities {
  vector[N] y_pred;
  real<lower=0> sigma2;
  sigma2 = 1/inv_sigma2;
  for (i in 1:N) {
    y_pred[i] = normal_rng(covariables[i,1:5]* beta, 1/sqrt(inv_sigma2)); // Predicciones
  }
  // Calcular la verosimilitud logarítmica para cada muestra
  vector[N] log_lik;
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(y[i] | covariables[i,1:5]* beta, 1/sqrt(inv_sigma2));
  }
}
"

# Crear un listado de datos para pasar al modelo
datos <- list(N = length(HC_data$Life.expectancy),
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
pairs(param1$beta)

