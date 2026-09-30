# Parámetros de las distribuciones Poisson
lambda1 <- 3
lambda2 <- 5

# Número de simulaciones
n <- 100000

# Simulamos dos variables Poisson independientes
X1 <- rpois(n, lambda1)
X2 <- rpois(n, lambda2)

# Suma de las dos variables
S <- X1 + X2

# Esperanza teórica
lambda_total <- lambda1 + lambda2

# Comparamos con la Poisson teórica
hist(S, breaks = 30, probability = TRUE, 
     col = "lightblue", border = "white",
     main = "Suma de dos Poisson(3) y Poisson(5)",
     xlab = "Número total de eventos")

# Añadimos la densidad teórica Poisson(8)
x_vals <- 0:max(S)
lines(x_vals, dpois(x_vals, lambda_total), 
      col = "red", lwd = 2)

# Mostramos media y varianza empíricas vs teóricas
cat("Media empírica:", mean(S), "\n")
cat("Varianza empírica:", var(S), "\n")
cat("Media teórica:", lambda_total, "\n")
cat("Varianza teórica:", lambda_total, "\n")

