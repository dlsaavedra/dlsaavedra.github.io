# Ejemplo de estimadores de la media mu

m = 100 # Repetición del experimento
mu_real = 10
sigma_real = 1

n = 1000 # Número de datos

est1_mu = rep(0,m)
est2_mu = rep(0,m)
est3_mu = rep(0,m)
est4_mu = rep(0,m)

for(i in 1:m){
  
  x = rnorm(n, mu_real, sigma_real)
  est1_mu[i] = (max(x)+min(x))/2
  est2_mu[i] = (quantile(x, 0.25) + quantile(x, 0.75))/2
  est3_mu[i] = median(x)
  est4_mu[i] = mean(x)
}

# Histogramas ----
# Configurar una ventana con 4 gráficos
par(mfrow = c(2, 2),       # 2 filas, 2 columnas
    mar = c(4, 4, 2, 1))   # márgenes adecuados

# Límites comunes para facilitar la comparación
lim <- range(c(est1_mu, est2_mu, est3_mu, est4_mu))

# Histograma 1
hist(est1_mu, breaks = 15, col = "tomato", xlim = lim,
     main = expression(hat(mu)[1] == (max(X)+min(X))/2),
     xlab = expression(hat(mu)), freq = FALSE)
abline(v = mu_real, col = "black", lwd = 2, lty = 2)

# Histograma 2
hist(est2_mu, breaks = 15, col = "steelblue", xlim = lim,
     main = expression(hat(mu)[2] == (Q[1]+Q[3])/2),
     xlab = expression(hat(mu)), freq = FALSE)
abline(v = mu_real, col = "black", lwd = 2, lty = 2)

# Histograma 3
hist(est3_mu, breaks = 15, col = "seagreen3", xlim = lim,
     main = expression(hat(mu)[3] == median(X)),
     xlab = expression(hat(mu)), freq = FALSE)
abline(v = mu_real, col = "black", lwd = 2, lty = 2)

# Histograma 4
hist(est4_mu, breaks = 15, col = "gold", xlim = lim,
     main = expression(hat(mu)[4] == mean(X)),
     xlab = expression(hat(mu)), freq = FALSE)
abline(v = mu_real, col = "black", lwd = 2, lty = 2)

# Boxplot -----

# Configurar una ventana con 4 diagramas
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# Límites comunes
lim <- range(c(est1_mu, est2_mu, est3_mu, est4_mu))

# Diagrama 1
boxplot(est1_mu, col = "tomato", ylim = lim,
        main = expression(hat(mu)[1] == (max(X)-min(X))/2),
        ylab = expression(hat(mu)))
abline(h = mu_real, col = "black", lwd = 2, lty = 2)

# Diagrama 2
boxplot(est2_mu, col = "steelblue", ylim = lim,
        main = expression(hat(mu)[2] == (Q[1]+Q[3])/2),
        ylab = expression(hat(mu)))
abline(h = mu_real, col = "black", lwd = 2, lty = 2)

# Diagrama 3
boxplot(est3_mu, col = "seagreen3", ylim = lim,
        main = expression(hat(mu)[3] == median(X)),
        ylab = expression(hat(mu)))
abline(h = mu_real, col = "black", lwd = 2, lty = 2)

# Diagrama 4
boxplot(est4_mu, col = "gold", ylim = lim,
        main = expression(hat(mu)[4] == mean(X)),
        ylab = expression(hat(mu)))
abline(h = mu_real, col = "black", lwd = 2, lty = 2)

# Restaurar la configuración gráfica original
par(mfrow = c(1, 1))

# Print media y varianza -----
# Calcular medias y varianzas de cada estimador
mean_est <- c(mean(est1_mu), mean(est2_mu), mean(est3_mu), mean(est4_mu))
var_est  <- c(var(est1_mu),  var(est2_mu),  var(est3_mu),  var(est4_mu))

# Mostrar resultados en una tabla
resultados <- data.frame(
  Estimador = c("max-min / 2", "(Q1+Q3)/2", "Mediana", "Media"),
  Media     = round(mean_est, 4),
  Varianza  = round(var_est, 4)
)

print(resultados)
