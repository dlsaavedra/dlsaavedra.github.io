library(survival)
library(ggplot2)
# Datos de ejemplo (simulados)
datos = read.table("Ayudantia_1/datos2_falla_Engine_Fan_Nelson_1982.txt", header = T)


# Ajustar Kaplan-Meier
km_fit <- survfit(Surv(Meses, Censura) ~ 1, data = datos)
km_df <- data.frame(
  time = km_fit$time,
  surv = km_fit$surv,
  lower = km_fit$lower,
  upper = km_fit$upper
)
plot(km_fit)

# Ajustar el modelo exponencial ----

exp_model <- survreg(Surv(Meses, Censura) ~ 1, data = datos, dist = "exponential")
summary(exp_model)
lambda <- exp(exp_model$coefficients)           # Tasa de riesgo (λ)
cat("Tasa de riesgo (λ):", lambda)

# Ajustar el modelo Weibull ----
weibull_model <- survreg(Surv(Meses, Censura) ~ 1, data = datos, dist = "weibull")
summary(weibull_model)
# Escala (λ) y forma (γ) en Weibull
scale <- exp(weibull_model$coefficients)  # λ = exp(Intercept)
shape <- 1 / weibull_model$scale          # γ = 1/scale
scale
shape

# Ajustar el modelo LogNormal ----
lnorm_model <- survreg(Surv(Meses, Censura) ~ 1, data = datos, dist = "lognormal")
summary(lnorm_model)


# Secuencia de tiempos para predicción
times <- seq(0, max(datos$Meses) , length.out = 100)

# Función de supervivencia teórica para cada modelo
surv_exp <- 1 - pexp(times, rate = 1 / exp(exp_model$coefficients))
surv_lnorm <- 1 - plnorm(times, meanlog = lnorm_model$coefficients, sdlog = lnorm_model$scale)
surv_weibull <- 1 - pweibull(times, shape = 1 / weibull_model$scale, scale = exp(weibull_model$coefficients))

# Gamma requiere flexsurv para S(t)
library(flexsurv)
gamma_model <- flexsurvreg(Surv(Meses, Censura) ~ 1, data = datos, dist = "gamma")
#flexsurvreg(Surv(Meses, Censura) ~ 1, data = datos, dist = "exp")
#flexsurvreg(Surv(Meses, Censura) ~ 1, data = datos, dist = "lnorm")
flexsurvreg(Surv(Meses, Censura) ~ 1, data = datos, dist = "weibull")
surv_gamma <- 1 - pgamma(times, shape = gamma_model$res["shape", "est"], rate = gamma_model$res["rate", "est"])

# Dataframe para ggplot
df_curves <- data.frame(
  time = rep(times, 4),
  surv = c(surv_exp, surv_lnorm, surv_gamma, surv_weibull),
  model = rep(c("Exponencial", "Log-Normal", "Gamma", "Weibull"), each = length(times))
)

# Gráfico
ggplot()+
  # Curvas paramétricas
  geom_line(data = df_curves, aes(x = time, y = surv, color = model), linewidth = 1) +
  
  # Kaplan-Meier (puntos y IC)
  geom_step(data = km_df, aes(x = time, y = surv), color = "black", linewidth = 1.2) +
  geom_ribbon(data = km_df, aes(x = time, ymin = lower, ymax = upper), 
              fill = "gray", alpha = 0.3) +
  
  # Configuración del gráfico
  labs(
    title = "Comparación de Modelos Paramétricos vs Kaplan-Meier",
    x = "Tiempo (Meses)",
    y = "Probabilidad de Supervivencia S(t)",
    color = "Modelo Paramétrico"
  ) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "red")) +  # Colores
  theme_minimal() +
  theme(legend.position = "bottom")

AIC_values <- data.frame(
  Modelo = c("Exponencial", "Log-Normal", "Gamma", "Weibull"),
  AIC = c(AIC(exp_model), AIC(lnorm_model), AIC(gamma_model), AIC(weibull_model))
)
print(AIC_values)

# Imputar Censura ---- 

datos_imputados = datos
datos_imputados$Censura = 1
# Ajustar Kaplan-Meier
km_fit_imputados <- survfit(Surv(Meses, Censura) ~ 1, data = datos_imputados)
km_df_imputados<- data.frame(
  time = km_fit_imputados$time,
  surv = km_fit_imputados$surv,
  lower = km_fit_imputados$lower,
  upper = km_fit_imputados$upper
)
plot(km_fit_imputados)
lines(km_fit, col= "red", add = T)
# Ajustar el modelo exponencial ----

exp_model_imputados <- survreg(Surv(Meses, Censura) ~ 1, data = datos_imputados, dist = "exponential")
summary(exp_model_imputados)
lambda <- exp(-exp_model_imputados$coefficients)           # Tasa de riesgo (λ)
cat("Tasa de riesgo (λ):", lambda)

weibull_model_imputados <- survreg(Surv(Meses, Censura) ~ 1, data = datos_imputados, dist = "weibull")
summary(weibull_model_imputados)
# Escala (λ) y forma (γ) en Weibull
scale <- exp(weibull_model_imputados$coefficients)  # λ = exp(Intercept)
shape <- 1 / weibull_model_imputados$scale          # γ = 1/scale
scale
shape
lnorm_model_imputados <- survreg(Surv(Meses, Censura) ~ 1, data = datos_imputados, dist = "lognormal")
summary(lnorm_model_imputados)


# Secuencia de tiempos para predicción
times <- seq(0, max(datos$Meses), length.out = 100)

# Función de supervivencia teórica para cada modelo
surv_exp_imputados <- 1 - pexp(times, rate = 1 / exp(exp_model_imputados$coefficients))
surv_lnorm_imputados <- 1 - plnorm(times, meanlog = lnorm_model_imputados$coefficients, sdlog = lnorm_model_imputados$scale)
surv_weibull_imputados <- 1 - pweibull(times, shape = 1 / weibull_model_imputados$scale, scale = exp(weibull_model_imputados$coefficients))

# Gamma requiere flexsurv para S(t)

gamma_model_imputados <- flexsurvreg(Surv(Meses, Censura) ~ 1, data = datos_imputados, dist = "gamma")
#flexsurvreg(Surv(Meses, Censura) ~ 1, data = datos, dist = "exp")
#flexsurvreg(Surv(Meses, Censura) ~ 1, data = datos, dist = "lnorm")
flexsurvreg(Surv(Meses, Censura) ~ 1, data = datos, dist = "weibull")
surv_gamma_imputados <- 1 - pgamma(times, shape = gamma_model_imputados$res["shape", "est"], rate = gamma_model_imputados$res["rate", "est"])

# Dataframe para ggplot
df_curves_imputados <- data.frame(
  time = rep(times, 4),
  surv = c(surv_exp_imputados, surv_lnorm_imputados, surv_gamma_imputados, surv_weibull_imputados),
  model = rep(c("Exponencial", "Log-Normal", "Gamma", "Weibull"), each = length(times))
)

# Gráfico
ggplot()+
  # Curvas paramétricas
  geom_line(data = df_curves, aes(x = time, y = surv, color = model), linewidth = 1) +
  geom_line(data = df_curves_imputados, aes(x = time, y = surv, color = model), linewidth = 1,linetype = "dashed") +
  
  # Kaplan-Meier (puntos y IC)
  geom_step(data = km_df, aes(x = time, y = surv), color = "black", linewidth = 1.2) +
  geom_ribbon(data = km_df, aes(x = time, ymin = lower, ymax = upper), 
              fill = "gray", alpha = 0.3) +
  
  geom_step(data = km_df_imputados, aes(x = time, y = surv), color = "black", linewidth = 1.2, linetype = "dashed") +
  geom_ribbon(data = km_df_imputados, aes(x = time, ymin = lower, ymax = upper), 
              fill = "gray", alpha = 0.3) +
  # Configuración del gráfico
  labs(
    title = "Comparación de Modelos Paramétricos vs Kaplan-Meier",
    x = "Tiempo (Meses)",
    y = "Probabilidad de Supervivencia S(t)",
    color = "Modelo Paramétrico"
  ) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73", "red")) +  # Colores
  theme_minimal() +
  theme(legend.position = "bottom")

AIC_values <- data.frame(
  Modelo = c("Exponencial", "Log-Normal", "Gamma", "Weibull"),
  AIC = c(AIC(exp_model_imputados), AIC(lnorm_model_imputados), 
          AIC(gamma_model_imputados), AIC(weibull_model_imputados))
)
print(AIC_values)
