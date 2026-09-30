
# Test Kolmogorov-Smirnov
set.seed(10)
x <- rexp(50, rate = 1)
hist(x, freq = F)

plot(ecdf(x))
curve(pexp(x), add = T, col = "red")
# --- Test con la función ---
ks.test(x, "pexp", rate = 1)

# --- Cálculo manual ---
x_sorted <- sort(x)
n <- length(x_sorted)

F0 <- pexp(x_sorted, rate = 1)

F_emp_abajo <- (0:(n-1))/(n)
F_emp_arriba <- (1:n)/(n)

Dplus  <- max(F_emp_arriba - F0);Dplus
Dminus <- max(F0 - F_emp_abajo);Dminus
D_manual <- max(Dplus, Dminus)
D_manual

k = 100
p_value <- 2 * sum((-1)^(1:k - 1) * exp(-2 * (1:k)^2 * D_manual^2 * n))
p_value


# Test Chi-Cuadrado
#Ej Experimeto dados
# Datos
obs <- c(1,9,10,18,12,10) 
n <- sum(obs);n

# Hipótesis H0: p = rep(1/6,6)
p0 <- rep(1/6, 6)
exp <- n * p0;exp
(obs - exp)
# Estadístico chi-cuadrado manual
chi_comp <- (obs - exp)^2 / exp;chi_comp
chi2_stat <- sum(chi_comp);chi2_stat
df <- length(obs) - 1
p_value <- 1 - pchisq(chi2_stat, df);p_value

# Resultados manuales
chi2_stat
chi_comp
p_value
df

# Usando la función integrada 
chisq.test(x = obs, p = p0, correct = FALSE)


# Ejemplo Exp Cortando los intervalos
# Datos de ejemplo (sustituya x por sus datos)
x <- rexp(50, rate = 1)   # ejemplo: n = 50

# Cortes
cuts <- c(0, 0.5, 1, 1.5, 2, Inf)
n <- length(x)

# lambda conocida (ejemplo)
lambda0 <- 1

# Probabilidades teóricas
p <- numeric(length(cuts)-1)
for (j in seq_along(p)) {
  a1 <- cuts[j]; a2 <- cuts[j+1]
  p[j] <- (exp(-lambda0 * a1) - exp(-lambda0 * ifelse(is.infinite(a2), Inf, a2)))
}

# Si a2 == Inf, exp(-lambda*a2) = 0 so formula reduces correctly
p
# Frecuencias esperadas y observadas
E <- n * p;E
O <- as.numeric(table(cut(x, breaks = cuts, right = FALSE))); O

# Estadístico chi-cuadrado manual
chi2_stat <- sum((O - E)^2 / E)

# grados de libertad: k - 1 (lambda conocida)
df <- length(E) - 1
p_value <- 1 - pchisq(chi2_stat, df);p_value

list(O = O, E = E, chi2 = chi2_stat, df = df, p_value = p_value)

chisq.test(x = O, p = E, correct = FALSE, rescale.p = TRUE)
