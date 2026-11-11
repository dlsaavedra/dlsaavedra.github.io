library(ggplot2)

# Parámetros de la distribución Beta a priori
alpha_prior <- 1
beta_prior <- 1

# Número de ensayos (datos observados)
n <- 100
# Número de éxitos (éxitos observados)
y <- 80

# Generar secuencia de valores para la probabilidad p
p_values <- seq(0, 1, by = 0.001)

# Calcular densidad de probabilidad a priori
prior_density <- dbeta(p_values, alpha_prior, beta_prior)

# Calcular densidad de probabilidad a posteriori
alpha_posterior <- alpha_prior + y
beta_posterior <- beta_prior + n - y
posterior_density <- dbeta(p_values, alpha_posterior, beta_posterior)

# Crear data frame para gráficos
df <- data.frame(p = p_values, prior = prior_density, posterior = posterior_density)

# Graficar distribución a priori y a posteriori
ggplot(df, aes(x = p)) +
  geom_line(aes(y = prior, color = "Prior"), size = 1.2) +
  geom_line(aes(y = posterior, color = "Posterior"), size = 1.2) +
  labs(x = "Probabilidad p", y = "Densidad de probabilidad", color = "Distribución") +
  ggtitle("Distribución a priori y a posteriori en un modelo Beta-Binomial") +
  theme_minimal()

# Calcular distribución predictiva
c_theta = rbeta(length(p_values), alpha_prior + y, beta_prior + n - y)

aux_dbinom = function(value){
  return(mean(dbinom(value, n, c_theta)))}

densidad_predictiva <-  sapply(1:200, aux_dbinom)

# Crear data frame para gráfico
df <- data.frame(p = 1:200, densidad_predictiva = densidad_predictiva)

# Graficar distribución predictiva
ggplot(df, aes(x = p, y = densidad_predictiva)) +
  geom_line(color = "blue", size = 1) +
  labs(x = "X", y = "Densidad predictiva",
       title = "Densidad de la distribución predictiva en un modelo Beta-Binomial") +
  theme_minimal()
which.max(densidad_predictiva)

library(manipulate)

manipulate(
  curve(dbeta(x,alpha_prior + floor(.8*n), beta_prior + n - floor(.8*n)),
       col = "red", xlim=c(-.1,1.1), ylim = c(0,10)),
 n=slider(0,100))

curve(dbeta(x,alpha_prior + y, beta_prior + n - y))
     