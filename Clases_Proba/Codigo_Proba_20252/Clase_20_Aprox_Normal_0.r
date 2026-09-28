# ============================================
# Teorema Central del Límite (TCL)
# ============================================

set.seed(123)

# Función general para ilustrar el TCL
TCL_demo <- function(distribution = "exp", n_values = c(1, 5, 30, 100),
                     n_samples = 10000) {
  
  # Selección de la función generadora
  rfun <- switch(distribution,
                 unif = function(n) runif(n, min = 0, max = 1),
                 exp  = function(n) rexp(n, rate = 1),
                 bin  = function(n) rbinom(n, size = 10, prob = 0.5),
                 pois = function(n) rpois(n, lambda = 4),
                 stop("Distribución no reconocida"))
  
  par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
  
  for (n in n_values) {
    # Simular medias muestrales
    means <- replicate(n_samples, mean(rfun(n)))
    
    # Estandarizar medias
    # (media - media teórica) / (desv. teórica / sqrt(n))
    if (distribution == "unif") {
      mu <- 0.5; sigma <- 1/sqrt(12)
    } else if (distribution == "exp") {
      mu <- 1; sigma <- 1
    } else if (distribution == "bin") {
      mu <- 10 * 0.5; sigma <- sqrt(10 * 0.5 * 0.5)
    } else if (distribution == "pois") {
      mu <- 4; sigma <- sqrt(4)
    }
    
    z <- (means - mu) / (sigma / sqrt(n))
    
    # Gráfico
    hist(z, breaks = 30, probability = TRUE,
         col = "skyblue", border = "white",
         main = paste("n =", n),
         xlab = "Medias estandarizadas")
    curve(dnorm(x, 0, 1), add = TRUE, col = "red", lwd = 2)
  }
  
  mtext(paste("Teorema Central del Límite - Distribución:", distribution),
        side = 3, outer = TRUE, line = -2, cex = 1.2)
}

# ============================================
# Ejemplos
# ============================================

# Distribución exponencial (asimétrica)
TCL_demo("exp", n_values = c(1, 20, 100, 1000))

# Distribución uniforme
TCL_demo("unif", n_values = c(1, 20, 100, 1000))

# Distribución binomial
TCL_demo("bin", n_values = c(1, 20, 100, 1000))

# Distribución Poisson
TCL_demo("pois", n_values = c(1, 20, 100, 1000))

