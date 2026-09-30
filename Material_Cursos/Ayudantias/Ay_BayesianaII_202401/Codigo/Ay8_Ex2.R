#install.packages("BNPdensity")
library(BNPdensity)
# Generar un conjunto de datos de ejemplo
set.seed(123)
# Función para generar una mezcla de normales multivariadas
generar_mezcla_normales <- function(n, medias, sigmas, proporciones) {
  # Validar entradas
  k <- length(medias)
  if (length(sigmas) != k | length(proporciones) != k) {
    stop("Las listas de medias, sigmas y proporciones deben tener la misma longitud")
  }
  if (sum(proporciones) != 1) {
    stop("Las proporciones deben sumar 1")
  }
  
  # Generar muestras para cada componente
  muestras <- list()
  for (i in 1:k) {
    ni <- round(n * proporciones[i])
    muestras[[i]] <- mvrnorm(ni, medias[[i]], sigmas[[i]])
  }
  
  # Combinar todas las muestras
  todas_muestras <- do.call(rbind, muestras)
  
  return(todas_muestras)
}

### Definir medias -----
#medias <- list(c(-10, -15), c(10, 8))
### Definir matrices de covarianza
#sigmas <- list(matrix(c(5, 0.3, 0.3, 1), nrow=2),
#             matrix(c(1, -0.5, -0.5, 5), nrow=2))

medias <- list(c(-15), c(8))
### Definir matrices de covarianza
sigmas <- list(c(10),c(1))
### Definir proporciones  -----
proporciones <- c(0.4, 0.6)
# Número total de muestras a generar 
n <- 100
muestras_mezcla <- generar_mezcla_normales(n, medias, sigmas, proporciones)[,1]
### Visualizar las muestras generadas  -----
plot(density(muestras_mezcla), main="Mezcla de Normales Multivariadas", xlab="X1", ylab="X2", 
     col = c(rep(1, proporciones[1]*n), rep(2,  proporciones[2]*n)))

grid <- seq(-30, 20, length.out = 200)

out <- MixNRMI1(muestras_mezcla)

# Plotting density estimate + 95% credible interval
plot(out)


# Plotting number of clusters
par(mfrow = c(2, 1))
plot(out$R, type = "l", main = "Trace of R")
hist(out$R, breaks = min(out$R - 0.5):max(out$R + 0.5), probability = TRUE)


out = MixNRMI1(muestras_mezcla, Alpha = 1, Kappa = 1, Gama = 0,
               Nit = 1000, Pbi = 0.2)

plot(out)
# Plotting cpo
par(mfrow = c(2, 1))
plot(out$cpo, main = "Scatter plot of CPO's")
boxplot(out$cpo, horizontal = TRUE, main = "Boxplot of CPO's")
print(paste("Average log(CPO)=", round(mean(log(out$cpo)), 4)))
print(paste("Median log(CPO)=", round(median(log(out$cpo)), 4)))


out = MixNRMI1(muestras_mezcla, Alpha = 1, Kappa = 1, Gama = 0,
               distr.k = "gamma",
               Nit = 1000, Pbi = 0.2)

plot(out)