# QQplot Norm
n = 10000
mu = 1
sigma = 2
X = rexp(n, rate = 3)#rnorm(n, mean = mu, sd = sigma)
qqplot(qnorm(ppoints(n), mean  = 0, sd = 1), X)
abline(a = 1, b = 2, col = 2, lwd = 2)

## QQplot log-Norm
n = 1000
lambda = 1
chi = 2
X = rexp(n, rate = 3)#exp(rnorm(n, mean = lambda, sd = chi))
qqplot(qnorm(ppoints(n), mean  = 0, sd = 1), log(X))
abline(a = 1, b = 2, col = 2, lwd = 2)

## QQplot log-Norm
n = 10000
a = 1
nu = 2
X = a + rexp(n, rate = nu)
p = seq(0,1,length.out = n)
qqplot(-log(1- p), X)
abline(a = 1, b = 1/2, col = 2, lwd = 2)


## KS 
?ks.test
n = 10000
mu = 1
sigma = 2
X = rnorm(n, mean = mu, sd = sigma)
?ks.test

ks.test(X, "pnorm", 0, 1)
ks.test(X, "pnorm", 1, 2)

## Chisq.test
n = 10000
mu = 1
sigma = 2
X = rnorm(n, mean = mu, sd = sigma)
?chisq.test
# Definir los límites de los intervalos (bins)
breaks <- qnorm(seq(0, 1, by = 0.2), mean = 0, sd = 1)  # 5 intervalos equitativos
# Calcular las frecuencias observadas
observed <- table(cut(X, breaks = breaks, include.lowest = TRUE))

# Calcular las frecuencias esperadas
expected <- diff(pnorm(breaks, mean = 1, sd = 2)) * length(sample)  # Proporciones * tamaño muestra

# Test de chi-cuadrado
chisq_test <- chisq.test(x = observed, p = expected / sum(expected))

# Resultado del test
print(chisq_test)
