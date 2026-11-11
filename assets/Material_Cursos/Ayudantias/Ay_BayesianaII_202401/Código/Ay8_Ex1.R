# Instalar y cargar el paquete BNPmix
#install.packages("BNPmix")
library(MASS)
library(BNPmix)

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
strength <- 1  # Parámetro de concentración
discount <- 0.5  # Parámetro de estabilidad

# Ajustar un modelo de mezcla bayesiano no paramétrico
fit <- PYdensity(muestras_mezcla, 
                 list(niter = 10000, nburn = 2000, print_message = T), 
                 prior = list(strength = strength, discount = discount),
                 output = list(grid = grid, out_param = TRUE))

# Resumen del ajuste
summary(fit)

# Graficar la densidad ajustada
plot(fit, main = "Densidad ajustada por BNPmix", xlab = "Valores", ylab = "Densidad", add = TRUE)

x <- 1.4
dBNPdens(fit, x)

#Partición
class(fit)
P = partition(fit)

heatmap(P$psm, Rowv = NA, Colv = NA, col = heat.colors(256), scale = "none",
        main = "Matriz de similitud", xlab = "Observaciones", ylab = "Observaciones")

P = partition(fit, dist = "Binder")

D = colMeans(fit$density)
heatmap(D, Rowv = NA, Colv = NA, col = heat.colors(256), scale = "none",
        main = "Matriz de similitud", xlab = "Observaciones", ylab = "Observaciones")

plot(grid,D)


# Caso Bivariado -----

### Definir medias -----
medias <- list(c(-10, -15), c(10, 8))
### Definir matrices de covarianza
sigmas <- list(matrix(c(5, 0.3, 0.3, 1), nrow=2),
               matrix(c(1, -0.5, -0.5, 5), nrow=2))

### Definir proporciones  -----
proporciones <- c(0.4, 0.6)
# Número total de muestras a generar 
n <- 100
muestras_mezcla <- generar_mezcla_normales(n, medias, sigmas, proporciones)
### Visualizar las muestras generadas  -----
plot(muestras_mezcla, main="Mezcla de Normales Multivariadas", xlab="X1", ylab="X2", 
     col = c(rep(1, proporciones[1]*n), rep(2,  proporciones[2]*n)))

# Generar una malla de puntos para evaluar la densidad bivariada estimada
x_vals <- seq(min(muestras_mezcla[, 1]), max(muestras_mezcla[, 1]), length.out = 100)
y_vals <- seq(min(muestras_mezcla[, 2]), max(muestras_mezcla[, 2]), length.out = 100)
grid <- expand.grid(x = x_vals, y = y_vals)


strength <- 1  # Parámetro de concentración
discount <- 0.5  # Parámetro de estabilidad

# Ajustar un modelo de mezcla bayesiano no paramétrico
fit <- PYdensity(muestras_mezcla, 
                 list(niter = 10000, nburn = 2000, print_message = T), 
                 prior = list(strength = strength, discount = discount),
                 output = list(grid = grid, out_param = TRUE))

# Resumen del ajuste
summary(fit)

# Graficar la densidad ajustada
plot(fit, main = "Densidad ajustada por BNPmix", xlab = "Valores", ylab = "Densidad", add = TRUE)


#Partición
class(fit)
P = partition(fit)

heatmap(P$psm, Rowv = NA, Colv = NA, col = heat.colors(256), scale = "none",
        main = "Matriz de similitud", xlab = "Observaciones", ylab = "Observaciones")

P = partition(fit, dist = "Binder")
heatmap(P$psm, Rowv = NA, Colv = NA, col = heat.colors(256), scale = "none",
        main = "Matriz de similitud", xlab = "Observaciones", ylab = "Observaciones")


D = colMeans(fit$density)
# Crear una matriz para el heatmap
density_matrix <- matrix(D, nrow = length(x_vals), ncol = length(y_vals))

# Graficar el heatmap con la densidad estimada
image(x_vals, y_vals,density_matrix, col = heat.colors(256), axes = TRUE,
      main = "Heatmap de Densidad Estimada Bivariada por BNPmix", xlab = "X", ylab = "Y")
contour(x_vals, y_vals, density_matrix, add = TRUE, nlevels = 10, col = "black", lwd = 0.5)
