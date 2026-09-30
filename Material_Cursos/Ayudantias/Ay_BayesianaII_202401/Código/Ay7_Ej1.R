source("Ayudantia7/Gamma_mix_EM.R")
library(mixtools)

mixture_density_gamma <- function(x, pi, alpha, beta) {
  density <- sapply(x, function(xi) {
    sum(sapply(1:length(pi), function(k) {
      pi[k] * dgamma(xi, shape=alpha[k], rate=beta[k])
    }))
  })
  return(density)
}

set.seed(0)
n = 10000
data1 <- rgamma(n, shape = 2, scale = 1)
data2 <- rgamma(n, shape = 5, scale = 2)
data3 <- rgamma(n, shape = 9, scale = 3)
X <- c(data1, data2, data3)
?gammamixEM
gmm <- gammamixEM(X, k = 3, maxit = 100, epsilon = 1e-08)
gmm <- gammamixEM(X, k = 3, maxit = 100, epsilon = 1e-02)

print(gmm$gamma.pars)
print(gmm$lambda)
alpha = runif(3, 1,200)
beta = runif(3, 1, 200)
gmm <- gammamixEM(X, k = 3, alpha = alpha, beta = beta , maxit = 500, epsilon = 1e-02)
print(gmm$gamma.pars)
print(gmm$lambda)
gmm_propio = gamma_mix_em(X, K = 3, plot = TRUE, max_iters= 500)
print(gmm_propio$alpha)
print(1/gmm_propio$beta)
print(gmm_propio$pi)

hist(X, breaks = 30, probability = TRUE, main = "Modelo de Mezclas Gamma", xlab = "Datos", col = "lightgrey")
lines(density(X), col = "blue", lwd = 2)

x_seq <- seq(min(X), max(X), length.out = 1000)
pi = gmm$lambda
alpha = gmm$gamma.pars[1,] 
scale = gmm$gamma.pars[2,] 
lines(x_seq, mixture_density_gamma(x_seq, pi, alpha, 1/scale), col = "red", lwd = 2)

lines(x_seq, mixture_density_gamma(x_seq, gmm_propio$pi, gmm_propio$alpha, gmm_propio$beta), col = "green", lwd = 2)



# Cargar los datos
data(faithful)
plot(faithful)
X = faithful$waiting
X = X-min(X) + 1
hist(X, breaks = 30, probability = TRUE, 
     main = "Modelo de Mezclas Gamma", xlab = "Datos", 
     col = "lightgrey")


gmm <- gammamixEM(X, k = 2, maxit = 500, epsilon = 1e-02)
gmm$gamma.pars
x_seq <- seq(min(X), max(X), length.out = 1000)
pi = gmm$lambda; pi
alpha = gmm$gamma.pars[1,] 
scale = gmm$gamma.pars[2,] 
lines(x_seq, mixture_density_gamma(x_seq, pi, alpha, 1/scale), col = "red", lwd = 2)
