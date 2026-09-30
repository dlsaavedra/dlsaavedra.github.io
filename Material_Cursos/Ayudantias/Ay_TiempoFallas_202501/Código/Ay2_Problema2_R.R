# Preparar los datos ----
datos <- read.table("Ayudantia_2/bleed_system_failure_data.txt", header = TRUE)

# Convertir la variable Status a binaria
datos$Status <- ifelse(datos$Status == "Failed", 1, 0)

# Expandir los datos
data_expanded <- do.call(rbind, lapply(1:nrow(datos), function(i) {
  base_d <- if (datos$Base_D[i] > 0) {
    data.frame(Hours = rep(datos$Hours[i], datos$Base_D[i]),
               Status = rep(datos$Status[i], datos$Base_D[i]),
               BaseD = 1)
  } else NULL
  
  other_bases <- if (datos$Other_Bases[i] > 0) {
    data.frame(Hours = rep(datos$Hours[i], datos$Other_Bases[i]),
               Status = rep(datos$Status[i], datos$Other_Bases[i]),
               BaseD = 0)
  } else NULL
  
  rbind(base_d, other_bases)
}))

# Vista previa
head(data_expanded)


library(survival)
library(ggplot2)
library(survminer)

# Ajustar el modelo Kaphlan-Meier por grupo ----
km_fit <- survfit(Surv(Hours, Status) ~ BaseD, data = data_expanded)

# Graficar las curvas con intervalos de confianza
ggsurvplot(km_fit,
           data = data_expanded,
           pval = FALSE,  # Añadiremos el p-valor manualmente después
           conf.int = TRUE,  # Intervalos de confianza
           risk.table = F,  # Tabla de riesgo
           palette = "jco",  # Colores (opcional)
           title = "Curvas de Kaplan-Meier por Grupo",
           xlab = "Tiempo (horas)",
           ylab = "Probabilidad de Supervivencia",
           legend.labs = levels(factor(data_expanded$BaseD)),  # Etiquetas de grupos
           break.time.by = 1000)  # Ajustar según tus datos

# Realizar la prueba de log-rank ----
logrank_test <- survdiff(Surv(Hours, Status) ~ BaseD, data = data_expanded)

# Mostrar resultados
print(logrank_test)

# Modelos AFT ----
# Modelo AFT con errores Gaussianos (log-normal) 
aft_gaussian <- survreg(
  Surv(Hours, Status) ~ BaseD,  # Fórmula: horas ~ grupo (BaseD)
  data = data_expanded,
  dist = "loggaussian"  # Distribución log-normal (errores Gaussianos en log(t))
)

# Modelo AFT con errores de valores extremos (Weibull)
aft_weibull <- survreg(
  Surv(Hours, Status) ~ BaseD,
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

                        
km_fit <- survfit(Surv(Hours, Status) ~ BaseD, data = data_expanded)

# Gráfico base con KM
# Crear secuencia de tiempo
tseq <- seq(1, max(datos$Hours), length.out = 100)

# Crear nuevo data.frame para cada grupo
grupo0 <- data.frame(BaseD = factor(0, levels = c(0,1)))
grupo1 <- data.frame(BaseD = factor(1, levels = c(0,1)))

# Funciones de supervivencia teóricas
S_weibull <- function(t, model, newdata) {
  lp <- predict(model, newdata = newdata, type = "lp")
  sigma <- model$scale
  1 - pweibull(t, shape = 1/sigma, scale = exp(lp))
}

S_normal <- function(t, model, newdata) {
  lp <- predict(model, newdata = newdata, type = "lp")
  sigma <- model$scale
  z <- (log(t) - lp) / sigma
  1 - pnorm(z)
}

# Calcular curvas teóricas
curvas <- data.frame(
  Time = rep(tseq, 4),
  Survival = c(S_weibull(tseq, aft_weibull, grupo0),
               S_weibull(tseq, aft_weibull, grupo1),
               S_normal(tseq, aft_gaussian, grupo0),
               S_normal(tseq, aft_gaussian, grupo1)),
  Modelo = rep(c("Weibull", "Weibull", "Normal", "Normal"), each = length(tseq)),
  Grupo = rep(c("BaseD = 0", "BaseD = 1"), times = 2, each = length(tseq))
)

# Gráfico base KM por grupo
g_km <- ggsurvplot(km_fit, data = data_expanded, conf.int = FALSE,
                   legend.title = "Grupo", legend.labs = c("BaseD = 0", "BaseD = 1"),
                   ggtheme = theme_minimal(), palette = c("blue", "red"))

# Agregar curvas teóricas
g_km$plot +
  geom_line(data = subset(curvas, Modelo == "Weibull"),
            aes(x = Time, y = Survival, color = Grupo, linetype = "Weibull"), size = 1) +
  geom_line(data = subset(curvas, Modelo == "Normal"),
            aes(x = Time, y = Survival, color = Grupo, linetype = "Normal"), size = 1) +
  scale_linetype_manual(name = "Modelo AFT", values = c("Weibull" = "dashed", "Normal" = "dotdash")) +
  ylim(c(.9,1)) + 
  labs(title = "Kaplan-Meier vs Modelos AFT por Grupo",
       x = "Horas", y = "Probabilidad de Supervivencia") +
  theme(legend.position = "right")


 # Test LRV -----
# Modelo completo con covariable
aft_weibull <- survreg(Surv(Hours, Status) ~ BaseD, data = data_expanded, dist = "weibull")
aft_normal  <- survreg(Surv(Hours, Status) ~ BaseD, data = data_expanded, dist = "loggaussian")

# Modelo nulo (sin covariables)
null_weibull <- survreg(Surv(Hours, Status) ~ 1, data = data_expanded, dist = "weibull")
null_normal  <- survreg(Surv(Hours, Status) ~ 1, data = data_expanded, dist = "loggaussian")

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
summary(aft_normal)
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

resKM_normal <- survfit(Surv(resids_normal, Status) ~ 1, data = data_expanded)
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

resKM_weibull <- survfit(Surv(resids_weibull, Status) ~ 1, data = data_expanded)
# plot the KM estimate
plot(resKM_weibull, mark.time = FALSE, ylim = c(0.9,1))
# superimpose the survival function of the assumed
# extreme value distribution
xx <- seq(min(resids_weibull), max(resids_weibull), length.out = 35)
yy <- exp(- exp(xx))
lines(xx, yy, col = "red", lwd = 2)



