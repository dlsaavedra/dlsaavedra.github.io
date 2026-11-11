# Cargar la librería de supervivencia
library(survival)
library(ggplot2)
library(survminer)

datos <- read.table("Ayudantia_3/Life_Device_P2.txt", header = TRUE, sep = "\t")
# Expandir los datos
datos_expandido <- datos[rep(1:nrow(datos), datos$Devices), c("Lower", "Upper", "Status", "Temperature")]
datos_expandido$Evento <- ifelse(datos_expandido$Status == "Failed", 1, 0)



# Crear el objeto Surv
surv_obj_Lower <- Surv(time = datos_expandido$Lower, event = datos_expandido$Evento)
ajuste_km <- survfit(surv_obj_Lower ~ factor(datos_expandido$Temperature > 250))

ggsurvplot(ajuste_km,
           data = datos_expandido,
           pval = FALSE,  # Añadiremos el p-valor manualmente después
           conf.int = TRUE,  # Intervalos de confianza
           risk.table = F,  # Tabla de riesgo
           palette = "jco",  # Colores (opcional)
           title = "Curvas de Kaplan-Meier por Grupo",
           xlab = "Tiempo (horas)",
           ylab = "Probabilidad de Supervivencia",
           legend.labs = levels(factor(datos_expandido$Temperature> 200)),  # Etiquetas de grupos
           break.time.by = 1000)  # Ajustar según tus datos


surv_obj_Upper <- Surv(time = datos_expandido$Upper, event = datos_expandido$Evento)
ajuste_km <- survfit(surv_obj_Upper ~ factor(datos_expandido$Temperature > 250))

ggsurvplot(ajuste_km,
           data = datos_expandido,
           pval = FALSE,  # Añadiremos el p-valor manualmente después
           conf.int = TRUE,  # Intervalos de confianza
           risk.table = F,  # Tabla de riesgo
           palette = "jco",  # Colores (opcional)
           title = "Curvas de Kaplan-Meier por Grupo",
           xlab = "Tiempo (horas)",
           ylab = "Probabilidad de Supervivencia",
           legend.labs = levels(factor(datos_expandido$Temperature> 200)),  # Etiquetas de grupos
           break.time.by = 1000)  # Ajustar según tus datos





# Realizar la prueba de log-rank ----
logrank_test <- survdiff(surv_obj ~ factor(datos_expandido$Temperature > 250), data = datos_expandido)

# Mostrar resultados
print(logrank_test)

# Modelo Riesgos Proporcionales  ----

# Ajustar modelo de Cox
modelo_cox_Lower <- coxph(surv_obj_Lower ~ Temperature, data = datos_expandido)
modelo_cox_Upper <- coxph(surv_obj_Upper ~ Temperature, data = datos_expandido)
# Resumen del modelo
summary(modelo_cox_Lower)
summary(modelo_cox_Upper)
# Test Wald 

resumen <- summary(modelo_cox_Lower)
resumen$coefficients
#Nos interesa la Temperatura
coef_T <- resumen$coefficients["Temperature", "coef"]
se_T   <- resumen$coefficients["Temperature", "se(coef)"]
wald_stat <- (coef_T / se_T)^2
p_value <- 1 - pchisq(wald_stat, df = 1)
cat("Estadístico de Wald:", wald_stat, "\n")
cat("Valor-p:", p_value, "\n")

#Opción librería car
library(car)


linearHypothesis(modelo_cox_Lower, c("Temperature = 0"))


# Modelo sin covariables (nulo)
modelo_nulo_Lower <- coxph(surv_obj_Lower ~ 1, data = datos_expandido)
# Test de razón de verosimilitud (LRT)
anova(modelo_nulo_Lower, modelo_cox_Lower, test = "LRT")



# Ajuste de curva base
# Crear dos escenarios de temperatura
nuevo_mayor250 <- data.frame(Temperature = 260)     # representando Temp > 250
nuevo_menor250 <- data.frame(Temperature = 240)     # representando Temp ≤ 250

# Obtener curvas de supervivencia para cada caso
superv_mayor250 <- survfit(modelo_cox_Lower, newdata = nuevo_mayor250)
superv_menor250 <- survfit(modelo_cox_Lower, newdata = nuevo_menor250)


# --- Gráfico de Supervivencia ---
plot(superv_mayor250, col = "red", lwd = 2,
     xlab = "Horas", ylab = "Supervivencia",
     main = "Curvas de Supervivencia según Temperatura", ylim = c(0,1))
lines(superv_menor250, col = "blue", lwd = 2, lty = 2)
legend("bottomleft", legend = c("Temp = 260°C", "Temp = 240°C"),
       col = c("red", "blue"), lty = c(1, 2), lwd = 2)


# Estimar la función de riesgo base (derivada de la función de riesgo acumulado)
base_haz <- basehaz(modelo_cox_Lower, centered = FALSE)

# Crear función para obtener riesgo para un valor de temperatura
obtener_hazard <- function(temp_val) {
  rr <- exp(coef(modelo_cox_Lower) * temp_val)
  hazard <- rr * diff(c(0, base_haz$hazard))  # derivada discreta
  data.frame(time = base_haz$time, hazard = hazard)
}

# Obtener riesgos para dos temperaturas
haz_menor240 <- obtener_hazard(240)  # Temp ≤ 40
haz_mayor260 <- obtener_hazard(260)  # Temp > 40

# Graficar
plot(haz_menor240$time, haz_menor240$hazard, type = "l", col = "blue", lwd = 2,
     xlab = "Horas", ylab = "Riesgo instantáneo (hazard)", ylim = c(0,0.5), 
     main = "Función de riesgo para diferentes temperaturas")
lines(haz_mayor260$time, haz_mayor260$hazard, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("Temp = 240°C", "Temp = 260°C"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)





nuevo <- data.frame(Temperature = 280)
s_base <- survfit(modelo_cox_Lower, newdata = nuevo)

nuevo2 <- data.frame(Temperature = 280)
s_base2 <- survfit(modelo_cox_Upper, newdata = nuevo2)

# Graficar la curva para inspección visual
plot(s_base, xlim = c(0, 3000), main = "Curva de Supervivencia a 280°C")
lines(s_base2, col = "red")

legend("topright", legend = c("Lower", "Upper"),
       col = c("black", "red"), lty = c(1, 2), lwd = 2)

ajuste_km <- survfit(surv_obj ~ factor(datos_expandido$Temperature > 250))
plot(ajuste_km, xlab = "Horas", ylab = "-log(-log(S))",
     main = "Estimación de Kaplan-Meier", col = 1:2, fun = function (s) - log(-log(s)), lwd = 2)
legend("bottomleft",                                     # Posición de la leyenda
       legend = c("<= 250", "> 250"),  # Etiquetas
       col = 1:2,                                         # Colores que coinciden con el gráfico
       lty = 1,                                           # Tipo de línea
       title = "Temperatura",                            # Título de la leyenda
       cex = 0.5)

