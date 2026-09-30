datos <- read.table("Ayudantia_3/Life_Device_A_P1.txt", header = TRUE, sep = "\t")
# Expandir las filas según el número de dispositivos
data_expanded <- datos[rep(1:nrow(datos), datos$Number_of_Devices), ]
#Crear la variable de evento (1 = fallo, 0 = censura)
data_expanded$Evento <- ifelse(data_expanded$Status == "Failed", 1, 0)

# Cargar la librería de supervivencia
library(survival)
library(ggplot2)
library(survminer)

# Crear el objeto Surv
surv_obj <- Surv(time = data_expanded$Hours, event = data_expanded$Evento)
# Ajustar el Kaplan-Meier
ajuste_km <- survfit(surv_obj ~ 1)

# Graficar
plot(ajuste_km, xlab = "Horas", ylab = "Probabilidad de supervivencia", main = "Estimación de Kaplan-Meier")

ajuste_km <- survfit(surv_obj ~ factor(data_expanded$Temperature))
# Graficar las curvas con intervalos de confianza
plot(ajuste_km, xlab = "Horas", ylab = "Probabilidad de supervivencia",
     main = "Estimación de Kaplan-Meier", col = 1:4)
legend("bottomleft",                                     # Posición de la leyenda
       legend = levels(factor(data_expanded$Temperature)),  # Etiquetas
       col = 1:4,                                         # Colores que coinciden con el gráfico
       lty = 1,                                           # Tipo de línea
       title = "Temperatura",                            # Título de la leyenda
       cex = 0.5)



ggsurvplot(ajuste_km,
           data = data_expanded,
           pval = FALSE,  # Añadiremos el p-valor manualmente después
           conf.int = TRUE,  # Intervalos de confianza
           risk.table = F,  # Tabla de riesgo
           palette = "jco",  # Colores (opcional)
           title = "Curvas de Kaplan-Meier por Grupo",
           xlab = "Tiempo (horas)",
           ylab = "Probabilidad de Supervivencia",
           legend.labs = levels(factor(data_expanded$Temperature)),  # Etiquetas de grupos
           break.time.by = 1000)  # Ajustar según tus datos


# Realizar la prueba de log-rank ----
logrank_test <- survdiff(Surv(Hours, Evento) ~ factor(data_expanded$Temperature), data = data_expanded)

# Mostrar resultados
print(logrank_test)

# Modelo Riesgos Proporcionales  ----

# Ajustar modelo de Cox
modelo_cox <- coxph(Surv(Hours, Evento) ~ Temperature, data = data_expanded)

# Resumen del modelo
summary(modelo_cox)

# Test Wald 

resumen <- summary(modelo_cox)
resumen$coefficients
#Nos interesa la Temperatura
coef_T <- resumen$coefficients["Temperature", "coef"]
se_T   <- resumen$coefficients["Temperature", "se(coef)"]
wald_stat <- (coef_T / se_T)^2
p_value <- 1 - pchisq(wald_stat, df = 1)
cat("Estadístico de Wald:", wald_stat, "\n")
cat("Valor-p:", p_value, "\n")

#Opción librería car Test Wald
library(car)
linearHypothesis(modelo_cox, c("Temperature = 0"))


# Modelo sin covariables (nulo)
modelo_nulo <- coxph(Surv(Hours, Evento) ~ 1, data = data_expanded)
# Test de razón de verosimilitud (LRT)
anova(modelo_nulo, modelo_cox, test = "LRT")



# Ajuste de curva base
# Crear dos escenarios de temperatura
nuevo_mayor40 <- data.frame(Temperature = 50)     # representando Temp > 40
nuevo_menor40 <- data.frame(Temperature = 30)     # representando Temp ≤ 40

# Obtener curvas de supervivencia para cada caso
superv_mayor40 <- survfit(modelo_cox, newdata = nuevo_mayor40)
superv_menor40 <- survfit(modelo_cox, newdata = nuevo_menor40)


# --- Gráfico de Supervivencia ---
plot(superv_mayor40, col = "red", lwd = 2,
     xlab = "Horas", ylab = "Supervivencia",
     main = "Curvas de Supervivencia según Temperatura", ylim = c(0.8,1))
lines(superv_menor40, col = "blue", lwd = 2, lty = 2)
legend("bottomleft", legend = c("Temp = 50°C", "Temp = 30°C"),
       col = c("red", "blue"), lty = c(1, 2), lwd = 2)


# Estimar la función de riesgo base (derivada de la función de riesgo acumulado)
base_haz <- basehaz(modelo_cox, centered = FALSE)

# Crear función para obtener riesgo para un valor de temperatura
obtener_hazard <- function(temp_val) {
  rr <- exp(coef(modelo_cox) * temp_val)
  hazard <- rr * diff(c(0, base_haz$hazard))  # derivada discreta
  data.frame(time = base_haz$time, hazard = hazard)
}

# Obtener riesgos para dos temperaturas
haz_menor40 <- obtener_hazard(30)  # Temp ≤ 40
haz_mayor40 <- obtener_hazard(50)  # Temp > 40

# Graficar
plot(haz_menor40$time, haz_menor40$hazard, type = "l", col = "blue", lwd = 2,
     xlab = "Horas", ylab = "Riesgo instantáneo (hazard)", ylim = c(0,0.02), 
     main = "Función de riesgo para diferentes temperaturas")
lines(haz_mayor40$time, haz_mayor40$hazard, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Temp = 30°C", "Temp = 50°C"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)





nuevo <- data.frame(Temperature = 90)
s_base <- survfit(modelo_cox, newdata = nuevo)

nuevo2 <- data.frame(Temperature = 80)
s_base2 <- survfit(modelo_cox, newdata = nuevo2)
# Graficar la curva para inspección visual
plot(s_base, xlim = c(0, 5000), main = "Curva de Supervivencia a 90°C")
lines(s_base2, col = 2)
legend("topright", legend = c("Temp = 90°C", "Temp = 80°C"),
       col = c("black", "red"), lty = c(1, 2), lwd = 2)



nuevo <- data.frame(Temperature = 90)
s_base <- survfit(modelo_cox, newdata = nuevo)
plot(s_base, xlim = c(0, 10000), main = "Curva de Supervivencia a 90°C")




ajuste_km <- survfit(surv_obj ~ factor(data_expanded$Temperature))
plot(ajuste_km, xlab = "Horas", ylab = "Probabilidad de supervivencia",
     main = "Estimación de Kaplan-Meier", col = 1:4, fun = function (s) - log(-log(s)), lwd = 2)
legend("bottomleft",                                     # Posición de la leyenda
       legend = levels(factor(data_expanded$Temperature)),  # Etiquetas
       col = 1:4,                                         # Colores que coinciden con el gráfico
       lty = 1,                                           # Tipo de línea
       title = "Temperatura",                            # Título de la leyenda
       cex = 0.5)
