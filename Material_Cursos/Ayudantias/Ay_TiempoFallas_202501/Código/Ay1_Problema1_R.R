data = read.table("Ayudantia_1/datos1_falla_ISSN 0120-1751.txt")
perc_censura = 1- mean(data$Censura);perc_censura


summary(data$Meses)
sd(data$Meses)

hist(data$Meses, freq = F)
empirical_cdf = ecdf(data$Meses)
plot(empirical_cdf)

# Cargar paquete necesario
library(survival)
library(ggplot2)  # Para gráficos más avanzados (opcional)

# Crear objeto de supervivencia
# Reemplaza 'tiempo', 'evento' con los nombres de tus columnas
surv_obj <- Surv(time = data$Meses, event = data$Censura)

# Ajustar el modelo de Kaplan-Meier
km_fit <- survfit(surv_obj ~ 1)  # Para análisis global
# km_fit <- survfit(surv_obj ~ grupo, data = data)  # Para comparar grupos

# Graficar curva básica
plot(km_fit, main = "Curva de Kaplan-Meier",
     xlab = "Tiempo", ylab = "Probabilidad de Supervivencia",
     col = "blue", lwd = 2)

times <- sort(unique(data$Meses))
surv_prob <- 1 - empirical_cdf(times)
points(times, surv_prob, col = "red")


