data = read.table("Ayudantia_1/datos2_falla_Engine_Fan_Nelson_1982.txt", header = T)
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


library(survminer)


# Curva KM sin censura
km_no_censoring <- survfit(Surv(Meses, rep(1, nrow(data))) ~ 1, data = data)

# Combinar ambas curvas
combined_fit <- list(Normal = km_fit, "Sin Censura" = km_no_censoring)


# Graficar comparación
ggsurvplot(combined_fit, data = data,
           combine = TRUE,
           palette = c("blue", "red"),
           legend.title = "Método",
           legend.labs = c("Con censura", "Sin censura"),
           title = "Comparación de Curvas de Supervivencia",
           xlab = "Tiempo",
           ylab = "Probabilidad de Supervivencia",
           conf.int = TRUE)     # Mostrar intervalo de confianza

           