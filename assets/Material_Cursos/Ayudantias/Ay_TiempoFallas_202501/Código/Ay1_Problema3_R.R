rm(list = ls())

# Cargar paquetes necesarios
library(survival)
library(survminer)
library(ggplot2)

data <- read.table("Ayudantia_1/datos3_snubber_data_formatted.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
head(data)
# Separar los grupos
data_old = data[data$Group == "Old",]
data_new = data[data$Group == "New",]

surv_obj_old <- Surv(time = data_old$Hours, event = data_old$Status)
surv_obj_new <- Surv(time = data_new$Hours, event = data_new$Status)

km_fit_old <- survfit(surv_obj_old ~ 1)
km_fit_new <- survfit(surv_obj_new ~ 1)

plot(km_fit_old, main = "Curva de Kaplan-Meier",
     xlab = "Tiempo", ylab = "Probabilidad de Supervivencia",
     col = "blue", lwd = 2)

lines(km_fit_new, main = "Curva de Kaplan-Meier",
     xlab = "Tiempo", ylab = "Probabilidad de Supervivencia",
     col = "red", lwd = 2, add = T)


# Crear objeto de supervivencia
surv_obj <- Surv(time = data$Hours, event = data$Status)

# Ajustar el modelo de Kaplan-Meier por grupo
km_fit <- survfit(surv_obj ~ Group, data = data)

plot(km_fit, main = "Curva de Kaplan-Meier",
     xlab = "Tiempo", ylab = "Probabilidad de Supervivencia",
     col = c("red","blue"), lwd = 2)

# Realizar el test de log-rank
logrank_test <- survdiff(Surv(Hours, Status) ~ Group, data = data)

surv_obj <- Surv(time = data$Hours, event = data$Status)
logrank_test <- survdiff(surv_obj ~ Group, data = data)

# Mostrar resultados completos
print(logrank_test)

cat("Estadístico chi-cuadrado:", logrank_test$chisq, "\n")
cat("Valor p:", logrank_test$pvalue, "\n\n")

# Interpretación
if(logrank_test$pvalue < 0.05) {
  cat("Conclusión: Existe diferencia significativa entre los grupos (p < 0.05)\n")
  cat("Los grupos NO son iguales en términos de supervivencia.\n")
} else {
  cat("Conclusión: No hay diferencia significativa entre los grupos (p ≥ 0.05)\n")
  cat("No podemos rechazar la hipótesis de que los grupos son iguales.\n")
}

# Ajustar el modelo de Kaplan-Meier por grupo
km_fit <- survfit(surv_obj ~ Group, data = data)

# Graficar con survminer (mejor presentación)
ggsurvplot(km_fit, data = data,
           pval = TRUE,          # Mostrar valor p del log-rank test
           conf.int = TRUE,       # Mostrar intervalos de confianza
           palette =  c("blue", "red"),       # Esquema de colores
           #ggtheme = theme_bw(),  # Tema del gráfico
           legend.title = "Grupo",
           legend.labs = levels(factor(data$Group)), # Etiquetas de leyenda
           title = "Curvas de Kaplan-Meier por Grupo",
           xlab = "Horas",
           ylab = "Probabilidad de Supervivencia",
           break.time.by = 100)    # Intervalos de 24 horas en eje X
