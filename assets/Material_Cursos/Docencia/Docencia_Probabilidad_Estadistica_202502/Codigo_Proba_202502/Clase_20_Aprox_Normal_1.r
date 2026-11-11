dev.off()

### 1. Aproximación Normal de la Binomial ---------------------------------
# Parámetros
n <- 200        # número de ensayos
p <- 0.4       # probabilidad de éxito
x <- 0:n       # soporte discreto

# Probabilidades exactas
binom_probs <- dbinom(x, n, p)

# Aproximación normal
mu <- n * p
sigma <- sqrt(n * p * (1 - p))

# Corrección de continuidad: evaluamos la densidad entre x ± 0.5
x_cont <- seq(-2, n + 2, by = 0.01)
normal_approx <- pnorm(x_cont + 0.5, mu, sigma) - pnorm(x_cont - 0.5, mu, sigma)

# Gráfico comparativo
plot(x, binom_probs, type = "h", lwd = 2, col = "blue",
     main = "Aproximación Normal de la Binomial",
     ylab = "Probabilidad", xlab = "x", ylim = c(0, max(binom_probs)), 
     xlim = c(min(x_cont), max(x_cont)))
lines(x_cont, normal_approx, col = "red", lwd = 2)

legend("topright", legend = c("Binomial exacta", "Normal aproximada"),
       col = c("blue", "red"), lwd = 2, bty = "n")


### 2. Aproximación Normal de la Gamma -----------------------------------

# Parámetros
n <- 10
rate  <- 2

# Soporte continuo
x <- seq(0, n, length.out = 500)

# Densidad exacta Gamma
gamma_dens <- dgamma(x, shape = n, rate = rate)

# Aproximación normal
mu <- n / rate
sigma <- sqrt(n) / rate
normal_dens <- dnorm(x, mean = mu, sd = sigma)

# Gráfico
plot(x, gamma_dens, type = "l", lwd = 2, col = "blue",
     main = "Aproximación Normal de la Gamma",
     ylab = "Densidad", xlab = "x")
lines(x, normal_dens, col = "red", lwd = 2)
legend("topright", legend = c("Gamma", "Normal aprox."),
       col = c("blue", "red"), lwd = 2, bty = "n")


### 3. Aproximación Normal de la Poisson ---------------------------------

# Parámetro
n = 10
lambda <- 2

# Soporte discreto
x <- 0:(lambda*n*2)

# Probabilidades exactas
pois_probs <- dpois(x, lambda = lambda*n)

# Aproximación normal (con corrección de continuidad)
x_cont <- seq(-0.5, max(x) + 0.5, by = 0.01)
normal_approx <- pnorm(x_cont + 0.5, mean = lambda*n, sd = sqrt(lambda*n)) -
  pnorm(x_cont - 0.5, mean = lambda*n, sd = sqrt(lambda*n))

# Gráfico
plot(x, pois_probs, type = "h", lwd = 2, col = "blue",
     main = "Aproximación Normal de la Poisson",
     ylab = "Probabilidad", xlab = "x", ylim = c(0, max(pois_probs)))
lines(x_cont, normal_approx, col = "red", lwd = 2)
legend("topright", legend = c("Poisson", "Normal aprox."),
       col = c("blue", "red"), lwd = 2, bty = "n")

