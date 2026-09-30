# Datos simulados
set.seed(123)
x <- rnorm(100, mean = 5, sd = 2)
plot(ecdf(x))
curve(pnorm(x,mean = 5, sd = 2), add = T, col ="red")

# Forma manual
x_sorted <- sort(x) 
n <- length(x_sorted)
p <- (1:n-0.5) / n;p
z_theoretical <- qnorm(p)

plot(z_theoretical, x_sorted,
     xlab = "Cuantiles teóricos N(0,1)",
     ylab = "Cuantiles empíricos de X",
     main = "QQ-plot construido manualmente")


ajuste_linea <- lm(x_sorted ~ z_theoretical)
abline(ajuste_linea, col = "red", lwd = 2)


# QQ-plot básico
qqnorm(x, main = "QQ-plot de normalidad para X")
qqline(x, col = "red", lwd = 2)


# Ejemplo exponencial
x = rexp(100, rate = 2)
# QQ-plot básico
qqnorm(x, main = "QQ-plot de normalidad para X")
qqline(x, col = "red", lwd = 2)

# Ejemplo t student
x = rt(1000, df = 5)
# QQ-plot básico
qqnorm(x, main = "QQ-plot de normalidad para X")
qqline(x, col = "red", lwd = 2)


# Comparación con la exponencial

set.seed(2)
x <- rexp(5000, rate = 1)

# 1. Ordenar la muestra
x_sorted <- sort(x)
n <- length(x_sorted)

# 2. Cuantiles teóricos
p <- (1:n - 0.5) / n
q_theoretical <- -log(1 - p) 

# 3. QQ-plot manual
plot(q_theoretical, x_sorted,
     xlab = "Cuantiles teóricos Exp(1)",
     ylab = "Cuantiles empíricos X",
     main = "QQ-plot Exponencial manual")

# 4. Recta de referencia
ajuste <- lm(x_sorted ~ q_theoretical)
abline(ajuste, col = "red", lwd = 2)

# Comparación para cualquier distribución

## Q-Q plot for Chi^2 data against true theoretical distribution:
y <- rchisq(500, df = 3)
qqplot(qchisq(ppoints(500), df = 3), y,
       main = expression("Q-Q plot for" ~~ {chi^2}[nu == 3]))
qqline(y, distribution = function(p) qchisq(p, df = 3),
       probs = c(0.1, 0.9), col = 2)



