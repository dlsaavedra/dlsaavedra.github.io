datos <- read.table("Ayudantia_2/Life_Device_A.txt", header = TRUE, sep = "\t")
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

# Modelos AFT ----
# Modelo AFT con errores Gaussianos (log-normal) 
aft_gaussian <- survreg(
  Surv(Hours, Evento) ~ factor(data_expanded$Temperature),  # Fórmula: horas ~ grupo (BaseD)
  data = data_expanded,
  dist = "lognormal"  # Distribución log-normal (errores Gaussianos en log(t))
)

# Modelo AFT con errores de valores extremos (Weibull)
aft_weibull <- survreg(
  Surv(Hours, Evento) ~ factor(data_expanded$Temperature),
  data = data_expanded,
  dist = "weibull"  # Distribución Weibull (errores de valores extremos)
)


# Resumen del modelo log-normal
summary(aft_gaussian)

# Resumen del modelo Weibull
summary(aft_weibull)




# Modelo AFT con errores Gaussianos (log-normal)Temperatura Continua
aft_gaussian <- survreg(
  Surv(Hours, Evento) ~ data_expanded$Temperature,  # Fórmula: horas ~ grupo (BaseD)
  data = data_expanded,
  dist = "lognormal"  # Distribución log-normal (errores Gaussianos en log(t))
)

# Modelo AFT con errores de valores extremos (Weibull)
aft_weibull <- survreg(
  Surv(Hours, Evento) ~ data_expanded$Temperature,
  data = data_expanded,
  dist = "weibull"  # Distribución Weibull (errores de valores extremos)
)


# Resumen del modelo log-normal
summary(aft_gaussian)

# Resumen del modelo Weibull
summary(aft_weibull)



# AIC # Mejor modelo (AIC más bajo)
AIC(aft_gaussian, aft_weibull) 

# Graficar 

# Crear predicciones de tiempo esperado
data_expanded$pred_normal <- predict(aft_gaussian, type = "response")
data_expanded$pred_weibull <- predict(aft_weibull, type = "response")


km_fit <- survfit(Surv(Hours, Evento) ~ Temperature, data = data_expanded)

km_df <- data.frame(
  Tiempo = km_fit$time,
  Supervivencia = km_fit$surv,
  Temperatura = as.numeric(gsub(".*=(.*)", "\\1", rep(names(km_fit$strata), km_fit$strata)))
)
km_df$Modelo <- "Kaplan-Meier"
# Gráfico base con KM

# Crear grid de temperaturas para predicción
temp_vals <- c(10, 40, 60,80)
tseq <- seq(1, max(datos$Hour), length.out = 100)

# Predecir supervivencia Weibull
weibull_scale <- aft_weibull$scale
weibull_coef <- aft_weibull$coefficients
weibull_surv <- matrix(NA, nrow = length(tseq), ncol = length(temp_vals))

for (i in seq_along(temp_vals)) {
  lp <-  weibull_coef%*%c(1, temp_vals[i])
  scale <- exp(lp)
  weibull_surv[, i] <- 1 - pweibull(tseq, shape = 1/weibull_scale, scale = scale)
}

# Predecir supervivencia Normal
normal_scale <- aft_gaussian$scale
normal_coef <- aft_gaussian$coefficients
normal_surv <- matrix(NA, nrow = length(tseq), ncol = length(temp_vals))

for (i in seq_along(temp_vals)) {
  lp <-  c(normal_coef%*%c(1, temp_vals[i]))
  z <- (log(tseq) - lp) / normal_scale
  normal_surv[, i] <- 1 - pnorm(z)
}


# Construir dataframes para ggplot
df_weibull <- data.frame(
  Tiempo = rep(tseq, 4),
  Supervivencia = as.vector(weibull_surv),
  Modelo = "Weibull",
  Temperatura = rep(as.factor(temp_vals), each = length(tseq))
)
df_normal <- data.frame(
  Tiempo = rep(tseq, 4),
  Supervivencia = as.vector(normal_surv),
  Modelo = "Normal",
  Temperatura = rep(as.factor(temp_vals), each = length(tseq))
)

df_modelos <- rbind(df_weibull, df_normal)

# Gráfico
ggplot(df_modelos, aes(x = Tiempo, y = Supervivencia, color = Temperatura, linetype = Modelo)) +
  geom_line(size = 1) +
  labs(title = "Curvas de Supervivencia AFT vs Kaplan-Meier",
       x = "Horas", y = "Probabilidad de supervivencia") +
  theme_minimal()



# Combinar con curvas de los modelos AFT
df_todos <- rbind(df_modelos, km_df[, c("Tiempo", "Supervivencia", "Temperatura", "Modelo")])

# Definir colores personalizados (por ejemplo, rojo, azul y verde)
df_todos <- rbind(df_modelos, km_df[, c("Tiempo", "Supervivencia", "Temperatura", "Modelo")])

# Gráfico combinado
ggplot(df_todos, aes(x = Tiempo, y = Supervivencia,
                     color = Temperatura, linetype = Modelo)) +
  geom_line(data = subset(df_todos, Modelo != "Kaplan-Meier"), size = 1) +
  geom_step(data = subset(df_todos, Modelo == "Kaplan-Meier"), size = 1) +
  labs(title = "Curvas de Supervivencia: AFT vs Kaplan-Meier",
       x = "Horas", y = "Probabilidad de supervivencia") +
  theme_minimal()



# Test LRV -----
# Modelo completo con covariable
aft_weibull <- survreg(Surv(Hours, Evento) ~ Temperature, data = data_expanded, dist = "weibull")
aft_normal  <- survreg(Surv(Hours, Evento) ~ Temperature, data = data_expanded, dist = "loggaussian")

# Modelo nulo (sin covariables)
null_weibull <- survreg(Surv(Hours, Evento) ~ 1, data = data_expanded, dist = "weibull")
null_normal  <- survreg(Surv(Hours, Evento) ~ 1, data = data_expanded, dist = "loggaussian")

# Weibull
lrt_weibull <- 2 * (logLik(aft_weibull) - logLik(null_weibull))
pval_lrt_weibull <- pchisq(lrt_weibull, df = 1, lower.tail = FALSE)
pval_lrt_weibull
anova(null_weibull, aft_weibull)
# Normal
lrt_normal <- 2 * (logLik(aft_normal) - logLik(null_normal))
pval_lrt_normal <- pchisq(lrt_normal, df = 1, lower.tail = FALSE)
pval_lrt_normal
anova(null_normal, aft_normal)

# Test Wald ----
## Weibull ----
coefs <- aft_normal$coefficients[-1]  # excluir intercepto si deseas solo test de covariables
vcov_matrix <- vcov(aft_normal)[2, 2]  # submatriz para betas

wald_stat <- t(coefs) %*% solve(vcov_matrix) %*% coefs
pvalue_wald <- pchisq(wald_stat, df = length(coefs), lower.tail = FALSE)
pvalue_wald
## Weibull ----
coefs <- aft_weibull$coefficients[-1]  # excluir intercepto si deseas solo test de covariables
vcov_matrix <- vcov(aft_weibull)[2, 2]  # submatriz para betas

wald_stat <- t(coefs) %*% solve(vcov_matrix) %*% coefs
pvalue_wald <- pchisq(wald_stat, df = length(coefs), lower.tail = FALSE)
pvalue_wald



# Análisis de Residuos ----
## Normal----

fits_normal = aft_normal$linear.predictors
resids_normal = (log(aft_normal$y[, 1]) - fits_normal) / aft_normal$scale

resKM_normal <- survfit(Surv(resids_normal, Evento) ~ 1, data = data_expanded)
# plot the KM estimate
plot(resKM_normal, mark.time = FALSE, ylim = c(0.5,1))
# superimpose the survival function of the assumed
# extreme value distribution
xx <- seq(min(resids_normal), max(resids_normal), length.out = 35)
yy <- 1 - pnorm(xx)
lines(xx, yy, col = "red", lwd = 2)


## Weibull ----
fits_weibull = aft_weibull$linear.predictors
resids_weibull = (log(aft_weibull$y[, 1]) - fits_weibull) / aft_weibull$scale

resKM_weibull <- survfit(Surv(resids_weibull, Evento) ~ 1, data = data_expanded)
# plot the KM estimate
plot(resKM_weibull, mark.time = FALSE, ylim = c(0.9,1))
# superimpose the survival function of the assumed
# extreme value distribution
xx <- seq(min(resids_weibull), max(resids_weibull), length.out = 35)
yy <- exp(- exp(xx))
lines(xx, yy, col = "red", lwd = 2)




# Fallos luego de los 10.0000 y 30.000 horas
# Crear grid de temperaturas para predicción
temp_vals <- c(10)
tseq <- seq(1, 35000, length.out = 1000)

# Predecir supervivencia Weibull
weibull_scale <- aft_weibull$scale
weibull_coef <- aft_weibull$coefficients
weibull_surv <- matrix(NA, nrow = length(tseq), ncol = length(temp_vals))

for (i in seq_along(temp_vals)) {
  lp <-  weibull_coef%*%c(1, temp_vals[i])
  scale <- exp(lp)
  weibull_surv[, i] <- 1 - pweibull(tseq, shape = 1/weibull_scale, scale = scale)
}

# Predecir supervivencia Normal
normal_scale <- aft_gaussian$scale
normal_coef <- aft_gaussian$coefficients
normal_surv <- matrix(NA, nrow = length(tseq), ncol = length(temp_vals))

for (i in seq_along(temp_vals)) {
  lp <-  c(normal_coef%*%c(1, temp_vals[i]))
  z <- (log(tseq) - lp) / normal_scale
  normal_surv[, i] <- 1 - pnorm(z)
}


# Construir dataframes para ggplot
df_weibull <- data.frame(
  Tiempo = rep(tseq, 1),
  Supervivencia = as.vector(weibull_surv),
  Modelo = "Weibull",
  Temperatura = rep(as.factor(temp_vals), each = length(tseq))
)
df_normal <- data.frame(
  Tiempo = rep(tseq, 1),
  Supervivencia = as.vector(normal_surv),
  Modelo = "Normal",
  Temperatura = rep(as.factor(temp_vals), each = length(tseq))
)

df_modelos <- rbind(df_weibull, df_normal)

# Gráfico
ggplot(df_modelos, aes(x = Tiempo, y = Supervivencia, color = Temperatura, linetype = Modelo)) +
  geom_line(size = 1) +
  labs(title = "Curvas de Supervivencia AFT vs Kaplan-Meier",
       x = "Horas", y = "Probabilidad de supervivencia") +
  theme_minimal()

