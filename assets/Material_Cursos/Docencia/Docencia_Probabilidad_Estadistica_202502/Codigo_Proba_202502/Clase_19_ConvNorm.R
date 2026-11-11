# Instalar librerías si es necesario
# install.packages(c("ggplot2", "gganimate", "dplyr", "gifski"))

library(ggplot2)
library(gganimate)
library(dplyr)
library(gifski)

# Parámetros de las normales
mu1 <- -2
sigma1 <- 1
mu2 <- 2
sigma2 <- 1
x_vals <- seq(-10, 10, by = 0.05)

# Distribuciones individuales
y1 <- dnorm(x_vals, mean = mu1, sd = sigma1)
y2 <- dnorm(x_vals, mean = mu2, sd = sigma2)

# Crear frames para animación: desplazamiento de la segunda normal
frames <- lapply(seq(-6, 6, by = 0.05), function(shift) {
  data.frame(
    x = x_vals,
    y1 = dnorm(x_vals, mean = mu1, sd = sigma1),             # normal fija
    y2 = dnorm(shift - x_vals, mean = mu2, sd = sigma2),     # normal desplazada
    shift = shift
  )
}) %>% bind_rows()

# Distribución teórica de la suma
conv_teorica <- data.frame(
  x = x_vals,
  y = dnorm(x_vals, mean = mu1 + mu2, sd = sqrt(sigma1^2 + sigma2^2))
)

# Gráfico animado
g <- ggplot(frames, aes(x = x)) +
  geom_line(aes(y = y1), color = "steelblue", size = 1.2) +
  geom_line(aes(y = y2), color = "darkorange", size = 1.2) +
  geom_line(data = conv_teorica, aes(y = y), color = "red", size = 1.5) +
  coord_cartesian(xlim = c(-10, 10), ylim = c(0, 0.45)) +
  labs(
    title = "Convolución de dos distribuciones normales",
    subtitle =  "Ubicación de la segunda normal: {current_frame}",
    x = "x",
    y = "Densidad"
  ) +
  theme_minimal(base_size = 14) +
  transition_manual(shift)

# Animación y guardado
anim <- animate(g, fps = 10, width = 700, height = 400, renderer = gifski_renderer(loop = TRUE))
anim_save("convolucion_normal.gif", animation = anim)

cat("✅ GIF generado como 'convolucion_normal.gif' en tu directorio de trabajo.\n")
