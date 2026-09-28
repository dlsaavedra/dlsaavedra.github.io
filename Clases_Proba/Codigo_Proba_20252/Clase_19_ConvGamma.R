# Instalar librerías si es necesario
# install.packages(c("ggplot2", "gganimate", "dplyr", "gifski"))

library(ggplot2)
library(gganimate)
library(dplyr)
library(gifski)

# Parámetros de las Gamma
alpha1 <- 2
beta1 <- 1
alpha2 <- 3
beta2 <- 1
x_max <- 20
x_vals <- seq(0, x_max, by = 0.1)

# Distribuciones individuales
y1 <- dgamma(x_vals, shape = alpha1, rate = beta1)
y2 <- dgamma(x_vals, shape = alpha2, rate = beta2)

# Crear frames para animación: desplazamiento de la segunda densidad
frames <- lapply(seq(0, x_max, by = 0.1), function(shift) {
  data.frame(
    x = x_vals,
    y1 = dgamma(x_vals, shape = alpha1, rate = beta1),       # fija
    y2 = dgamma(shift - x_vals , shape = alpha2, rate = beta2), # desplazada
    shift = shift
  )
}) %>% bind_rows()

# Distribución teórica de la suma
conv_teorica <- data.frame(
  x = x_vals,
  y = dgamma(x_vals, shape = alpha1 + alpha2, rate = beta1)
)

# Gráfico animado
g <- ggplot(frames, aes(x = x)) +
  geom_line(aes(y = y1), color = "steelblue", size = 1.2) +
  geom_line(aes(y = y2), color = "darkorange", size = 1.2) +
  geom_line(data = conv_teorica, aes(y = y), color = "red", size = 1.5) +
  coord_cartesian(xlim = c(0, x_max), ylim = c(0, 0.4)) +
  labs(
    title = "Convolución de dos distribuciones Gamma",
    subtitle =  "Ubicación de la segunda Gamma: {current_frame}",
    x = "x",
    y = "Densidad"
  ) +
  theme_minimal(base_size = 14) +
  transition_manual(shift)

# Animación y guardado
anim <- animate(g, fps = 10, width = 700, height = 400, renderer = gifski_renderer(loop = TRUE))
anim_save("convolucion_gamma.gif", animation = anim)

cat("✅ GIF generado como 'convolucion_gamma.gif' en tu directorio de trabajo.\n")
