library(mixtools)

set.seed(0)
n <- 100
x <- runif(n, 0, 10)
beta1 <- 0.5
beta2 <- 0.1
lambda1 = exp(beta1 * x[1:(n/2)])
lambda2 = exp(beta2 * x[(n/2 + 1):n])

# Generar datos de conteo con dos componentes
y <- c(rpois(n/2, lambda = lambda1),
       rpois(n/2, lambda = lambda2))

# Combinar datos en un data frame
data <- data.frame(x = x, y = y)

plot(x,y,col = c(rep(1, n/2), rep(2, n/2)))

?poisregmixEM
# Ajustar el modelo de mezclas con regresión Poisson k = 2-----
poisson_mix <- poisregmixEM(data$y, data$x, k = 2, 
                            maxit = 100, epsilon = 1e-02)

# Imprimir los resultados
print(poisson_mix$beta)
print(poisson_mix$lambda)


# Ajustar el modelo de mezclas con regresión Poisson k=3 -----
poisson_mix_k3 <- poisregmixEM(data$y, data$x, k = 3, 
                               maxit = 100, epsilon = 1e-02)


# Imprimir los resultados
print(poisson_mix_k3$beta)
print(poisson_mix_k3$lambda)


# Ajustar el modelo de mezclas con regresión Poisson sin intercepto ----
poisson_mix_nointer <- poisregmixEM(data$y, data$x, k = 2, 
                            maxit = 100, epsilon = 1e-02,
                            addintercept = FALSE)
# Imprimir los resultados
print(poisson_mix_nointer$beta)
print(poisson_mix_nointer$lambda)




plot(x, y, pch = 19, col = ifelse(poisson_mix$posterior[, 1] > 0.5, "red", "blue"), main = "Modelo de Mezclas con Regresión Poisson")
legend("topleft", legend = c("Componente 1", "Componente 2"), col = c("red", "blue"), lwd = 2)

plot(x, y, pch = 19, col = ifelse(poisson_mix_nointer$posterior[, 1] > 0.5, "red", "blue"), main = "Modelo de Mezclas con Regresión Poisson")
legend("topleft", legend = c("Componente 1", "Componente 2"), col = c("red", "blue"), lwd = 2)

numeros = apply(poisson_mix_k3$posterior, 1, which.max)
plot(x, y, pch = 19, col = ifelse(numeros == 1, "red", ifelse(numeros == 2, "blue", "green")), main = "Modelo de Mezclas con Regresión Poisson")
legend("topleft", legend = c("Componente 1", "Componente 2", "Componente 3"), 
       col = c("red", "blue", "green"), lwd = 2)

f_BIC <- function(loglink, n, k){k * log(n) - 2*loglink }
f_BIC(poisson_mix$loglik, n, 6)
f_BIC(poisson_mix_nointer$loglik, n, 4)
f_BIC(poisson_mix_k3$loglik, n, 9)
# lower BIC values are generally preferred


# Ejercicio 1 ----

# DATA -----
#https://www.kaggle.com/code/gauravduttakiit/explore-the-poisson-regression
data <- read.csv("Ayudantia5/competition_awards_data.csv")
summary(data)
plot(data$Math.Score, data$Awards)
n = length(data$Awards)

# Ajustar el modelo de mezclas con regresión Poisson k = 2-----
list_BIC = rep(0,10)
for (k in 2:5){
  print(k)
  poisson_mix <- poisregmixEM(data$Awards, data$Math.Score, k = k, 
                            maxit = 100, epsilon = 1e-02, addintercept = FALSE)
  list_BIC[k] = f_BIC(poisson_mix$loglik, n, 2*k)
}
list_BIC

poisson_mix <- poisregmixEM(data$Awards, data$Math.Score, k = 2, 
                            maxit = 100, epsilon = 1e-02)
poisson_mix_nointer <- poisregmixEM(data$Awards, data$Math.Score, k = 2, 
                            maxit = 100, epsilon = 1e-02, addintercept = FALSE)

# Imprimir los resultados
print(poisson_mix_nointer$beta)
print(poisson_mix_nointer$lambda)

plot(data$Math.Score, data$Awards, pch = 19, col = ifelse(poisson_mix_nointer$posterior[, 1] > 0.5, "red", "blue"), main = "Modelo de Mezclas con Regresión Poisson")
legend("topleft", legend = c("Componente 1", "Componente 2"), col = c("red", "blue"), lwd = 2)

poisson_glm = glm(Awards ~ Math.Score , data = data, family = poisson)
poisson_glm_nointer = glm(Awards ~ Math.Score - 1, data = data, family = poisson)


f_BIC(poisson_mix$loglik, n, 3*k)
f_BIC(poisson_mix_nointer$loglik, n, 2*k)
BIC(poisson_glm)
BIC(poisson_glm_nointer)


data <- read.csv("Ayudantia5/Ay5_nyc-east-river-bicycle-counts.csv")
summary(data)
str(data)
data$Precipitation[data$Precipitation == "0.47 (S)"] = "0.47"
data$Precipitation[data$Precipitation == "T"] = "0"
data$Precipitation = as.numeric(data$Precipitation)
# MODELO CLÁSICO -----
# Ajustar el modelo de regresión de Poisson
poisson_glm <- glm(Manhattan.Bridge ~ High.Temp...F., data = data, family = poisson(link = "log"))
poisson_glm_nointer <- glm(Manhattan.Bridge ~ High.Temp...F. - 1, data = data, family = poisson(link = "log"))

# Mostrar un resumen del modelo
summary(poisson_glm)
summary(poisson_glm_nointer)

Y = data$Manhattan.Bridge
X = data$High.Temp...F.
plot(X,Y)

n = length(Y)

poisson_mix <- poisregmixEM(Y, X, k = 2, 
                            maxit = 100, epsilon = 1e-02)





poisson_mix$beta

BIC(poisson_glm)
BIC(poisson_glm_nointer)
f_BIC(poisson_mix$loglik, n, 3)


plot(X, Y, pch = 19, col = ifelse(poisson_mix$posterior[, 1] > 0.5, "red", "blue"), main = "Modelo de Mezclas con Regresión Poisson")
legend("topleft", legend = c("Componente 1", "Componente 2"), col = c("red", "blue"), lwd = 2)
