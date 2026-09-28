#Desconocido
b0 = 2
b1 = 0.5
sigma = 1
# Conocido
n=100
x = runif(n, 0,10)


y = b0+b1*x + rnorm(n, 0, sigma)

plot(x,y)

modelo = lm(y~x)
summary(modelo)
