#install.packages("rjags")
library(rjags)

##################
## Ejemplo 1 #####
##################

n = 100
k = sample(1:n,1)
theta = 1
lambda = 3
Y1 = rpois(k, lambda = theta)
Y2 = rpois(n-k, lambda = lambda)
Y = c(Y1,Y2)

# Especificación del modelo en formato de texto
modelo <- "model {

  # Prior para los parámetros lambda de las dos muestras
  lambda[1] ~ dgamma(alpha1, beta1)
  lambda[2] ~ dgamma(alpha2, beta2)
  K ~ dcat(pk[])
  
  # Parámetros de las distribuciones Gamma a priori
  alpha1 ~ dgamma(0.1, 0.1)
  beta1  ~ dgamma(0.1, 0.1)
  alpha2 ~ dgamma(0.1, 0.1)
  beta2 ~ dgamma(0.1, 0.1)
  
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
mcmc_samples <- coda.samples(modelo_jags, variable.names = c("lambda", "K", "ratio"),
                             n.iter = actualizaciones_totales, burn.in = actualizaciones_burnin)

# Resumen del resultado
print(summary(mcmc_samples))

# Graficar la distribución posterior de lambda para ambas muestras
plot(mcmc_samples)

plot(density(mcmc_samples[[1]][,2]), col = "blue", ylim = c(0, 5), xlim = c(.6,4.2))
lines(density(mcmc_samples[[1]][,3]), col = "red")
#curve(dgamma(x, 1, 1), add = TRUE, col = "green")
hist(mcmc_samples[[1]][,1],breaks = 50, col = "blue")
mean(Y1)
mean(Y2)
k

##################
##Ejercicio 1#####
##################
# minas de carbón británicas por año (1851-1962).\\

# Leer el archivo de texto
texto <- readLines("Ayudantia3/Data_Coal.txt", warn = FALSE)

# Unir todas las líneas en una sola cadena
texto_completo <- paste(texto, collapse = "\n")

# Evaluar la cadena de texto para obtener el objeto
datos <- eval(parse(text = texto_completo))

plot(ts(datos$Count, start = 1851))




# Especificación del modelo en formato de texto
modelo <- "model {

  # Prior para los parámetros lambda de las dos muestras
  lambda[1] ~ dgamma(alpha1, beta1)
  lambda[2] ~ dgamma(alpha2, beta2)
  K ~ dcat(pk[])
  
  # Parámetros de las distribuciones Gamma a priori
  alpha1 ~ dgamma(0.1, 0.1)
  beta1  ~ dgamma(0.1, 0.1)
  alpha2 ~ dgamma(0.1, 0.1)
  beta2 ~ dgamma(0.1, 0.1)
  
  # Verosimilitud Poisson para los datos de la muestra 1
  for(i in 1:N){

    eventos[i] ~ dpois(lambda[ind[i]])
    ind[i] <- 1 + step(i-K-0.01)
    pk[i] <- 1/N
}
  ratio <- lambda[1]/lambda[2]
  
  Fecha = K + 1851
}"
# Definir los datos para el modelo
datos_modelo <- list(eventos = datos$Count, N = datos$N)

# Inicializar el modelo
modelo_jags <- jags.model(textConnection(modelo), data = datos_modelo, n.chains = 2)

# Actualización del modelo
actualizaciones <- 5000
actualizaciones_burnin <- 500
actualizaciones_totales <- actualizaciones + actualizaciones_burnin
mcmc_samples <- coda.samples(modelo_jags, variable.names = c("lambda", "K", "ratio", "Fecha"), 
                             n.iter = actualizaciones_totales, burn.in = actualizaciones_burnin)

# Resumen del resultado
print(summary(mcmc_samples))

# Graficar la distribución posterior de lambda para ambas muestras
plot(mcmc_samples)

plot(density(mcmc_samples[[1]][,3]), col = "blue", ylim = c(0, 5), xlim = c(.6,4.2))
lines(density(mcmc_samples[[1]][,4]), col = "red")
#curve(dgamma(x, 1, 1), add = TRUE, col = "green")
hist(mcmc_samples[[1]][,1],breaks = 50, col = "blue")

plot(density(mcmc_samples[[1]][,5]))

###############
#### DIC ######
###############


dic1 = dic.samples(modelo_jags, n.iter = 5000)


# Especificación del modelo en formato de texto
modelo2 <- "model {

  # Prior para los parámetros lambda de las dos muestras
  lambda[1] ~ dgamma(alpha1, beta1)
  lambda[2] ~ dgamma(alpha2, beta2)
  K ~ dcat(pk[])
  
  # Parámetros de las distribuciones Gamma a priori
  alpha1 =  .1
  beta1 =  .1
  alpha2 =  .1
  beta2 =  .1
  
  # Verosimilitud Poisson para los datos de la muestra 1
  for(i in 1:N){

    eventos[i] ~ dpois(lambda[ind[i]])
    ind[i] <- 1 + step(i-K-0.01)
    pk[i] <- 1/N
}
  ratio <- lambda[1]/lambda[2]
  
  Fecha = K + 1851
}"
# Definir los datos para el modelo
datos_modelo <- list(eventos = datos$Count, N = datos$N)

# Inicializar el modelo
modelo_jags2 <- jags.model(textConnection(modelo2), data = datos_modelo, n.chains = 2)

dic2 = dic.samples(modelo_jags2, n.iter = 5000)

diffdic(dic1, dic2)


# Especificación del modelo en formato de texto
modelo3 <- "model {

  # Prior para los parámetros lambda de las dos muestras
  lambda[1] ~ dgamma(alpha1, beta1)
  lambda[2] ~ dgamma(alpha2, beta2)
  K ~ dcat(pk[])
  
  # Parámetros de las distribuciones Gamma a priori
  alpha1 =  .1
  beta1 =  .1
  alpha2 =  .1
  beta2 =  .1
  
  # Verosimilitud Poisson para los datos de la muestra 1
  for(i in 1:N){

    eventos[i] ~ dpois(lambda[ind[i]])
    ind[i] <- 1 + step(i-K-0.01)
    pk[i] <- 1/N
    }
  ratio <- lambda[1]/lambda[2]
  Fecha = K + 1851

  
}"

modelo_jags3 <- jags.model(textConnection(modelo3), data = datos_modelo, n.chains = 2)
# Actualización del modelo
actualizaciones <- 5000
actualizaciones_burnin <- 500
actualizaciones_totales <- actualizaciones + actualizaciones_burnin
mcmc_samples <- coda.samples(modelo_jags, variable.names = c("lambda", "K"), 
                             n.iter = actualizaciones_totales, burn.in = actualizaciones_burnin)

MC = as.data.frame(mcmc_samples[[1]])
log_link = rep(0,actualizaciones_totales)
for (i in 1:actualizaciones_totales){
  log_link[i] = sum(log(dpois(datos_modelo$eventos[1:MC$K[i]],MC$`lambda[1]`[i]))) + 
                sum(log(dpois(datos_modelo$eventos[(MC$K[i]+1):datos_modelo$N],MC$`lambda[2]`[i])))
}

p = 3
D = -2*log_link
DIC = .5*var(D) + mean(D)
AIC = 2*p + which.max(D)
BIC = p*log(N) + which.max(D)
