# Instalar librerías si es necesario
# install.packages(c("ggplot2", "gganimate", "dplyr", "gifski"))

library(ggplot2)
library(gganimate)
library(dplyr)
library(gifski)

# Parámetros de las Poisson
lambda1 <- 3
lambda2 <- 5
k_max <- 20

# Soporte
x_vals <- 0:k_max

# Distribuciones individuales
p1 <- dpois(x_vals, lambda1)
p2 <- dpois(x_vals, lambda2)

# Generamos marcos (frames) para la animación
frames <- lapply(0:k_max, function(shift) {
  data.frame(
    x = x_vals,
    y1 = dpois(x_vals, lambda1),                # Poisson fija
    y2 = dpois(shift-x_vals, lambda2),        # Poisson desplazada
    shift = shift
  )
}) %>% bind_rows()

# Distribución teórica de la suma
conv_teorica <- data.frame(
  x = x_vals,
  y = dpois(x_vals, lambda1 + lambda2)
)

# Creamos el gráfico animado
g <- ggplot(frames, aes(x = x)) +
  # Poisson azul fija
  geom_col(aes(y = y1), fill = "steelblue", alpha = 0.5) +
  # Poisson naranja desplazada
  geom_col(aes(y = y2), fill = "darkorange", alpha = 0.5) +
  # Resultado teórico en rojo
  geom_line(data = conv_teorica, aes(y = y), color = "red", size = 1.2) +
  coord_cartesian(xlim = c(0, k_max), ylim = c(0, 0.25)) +
  labs(
    title = "Convolución de dos distribuciones Poisson",
    subtitle =  "Ubicación de la segunda Poisson: {current_frame}",
    x = "x",
    y = "Probabilidad"
  ) +
  theme_minimal(base_size = 14) +
  transition_manual(shift)

# Animación y guardado
anim <- animate(
  g, fps = 4, width = 700, height = 400,
  renderer = gifski_renderer(loop = TRUE)
)

# Guardar el GIF
anim_save("convolucion_poisson_desplazada.gif", animation = anim)

cat("✅ GIF generado como 'convolucion_poisson_desplazada.gif' en tu directorio de trabajo.\n")
