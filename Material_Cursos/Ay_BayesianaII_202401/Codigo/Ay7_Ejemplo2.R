library(mixtools)

set.seed(0)
n <- 100
x <- runif(n, 0, 10)
beta1 <- 0.5
beta2 <- 0.1
lambda1 = exp(beta1 * x[1:(n/2)])
lambda2 = exp(beta2 * x[(n/2 + 1):n])

# Generar datos de conteo con dos componentes
y <- c(rpois(n/2, lambda = lambda1),
       rpois(n/2, lambda = lambda2))

# Combinar datos en un data frame
data <- data.frame(x = x, y = y)

plot(x,y, col = c(rep(1, n/2), rep(2, n/2)))

# Ajustar el modelo de mezclas con regresión Poisson k = 2-----
poisson_mix <- poisregmixEM(data$y, data$x, k = 2, 
                            maxit = 100, epsilon = 1e-02)

# Imprimir los resultados
print(poisson_mix$beta)
print(poisson_mix$lambda)


# Ajustar el modelo de mezclas con regresión Poisson k=3 -----
poisson_mix_k3 <- poisregmixEM(data$y, data$x, k = 3, 
                               maxit = 100, epsilon = 1e-02)

# Imprimir los resultados
print(poisson_mix_k3$beta)
print(poisson_mix_k3$lambda)


# Ajustar el modelo de mezclas con regresión Poisson sin intercepto ----
poisson_mix_nointer <- poisregmixEM(data$y, data$x, k = 2, 
                                    maxit = 100, epsilon = 1e-02,
                                    addintercept = FALSE)
# Imprimir los resultados
print(poisson_mix_nointer$beta)
print(poisson_mix_nointer$lambda)




plot(x, y, pch = 19, col = ifelse(poisson_mix$posterior[, 1] > 0.5, "red", "blue"), main = "Modelo de Mezclas con Regresión Poisson")
legend("topleft", legend = c("Componente 1", "Componente 2"), col = c("red", "blue"), lwd = 2)

plot(x, y, pch = 19, col = ifelse(poisson_mix_nointer$posterior[, 1] > 0.5, "red", "blue"), main = "Modelo de Mezclas con Regresión Poisson")
legend("topleft", legend = c("Componente 1", "Componente 2"), col = c("red", "blue"), lwd = 2)

numeros = apply(poisson_mix_k3$posterior, 1, which.max)
plot(x, y, pch = 19, col = ifelse(numeros == 1, "red", ifelse(numeros == 2, "blue", "green")), main = "Modelo de Mezclas con Regresión Poisson")
legend("topleft", legend = c("Componente 1", "Componente 2", "Componente 3"), 
       col = c("red", "blue", "green"), lwd = 2)

f_BIC <- function(loglink, n, k){k * log(n) - 2*loglink }
f_BIC(poisson_mix$loglik, n, 6)
f_BIC(poisson_mix_nointer$loglik, n, 4)
f_BIC(poisson_mix_k3$loglik, n, 9)
# lower BIC values are generally preferred