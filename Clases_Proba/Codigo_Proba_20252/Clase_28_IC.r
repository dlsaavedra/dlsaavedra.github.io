# IC mu, sigma conocido
n= 10
alpha = .1
k = qnorm(1-alpha/2)
sigma = 1
mu_real = 10

X = rnorm(n, mu_real, sd = sigma)
X_n = mean(X)
c( X_n - k*sigma/sqrt(n),  X_n + k*sigma/sqrt(n))

alpha2=.05
k2 = qnorm(1-alpha2/2)
c( X_n - k2*sigma/sqrt(n),  X_n + k2*sigma/sqrt(n))



m = 100000
suma = 0
for (i in 1:m){
  X_n = mean(rnorm(n, mu_real, sd = sigma))
  if ((mu_real > X_n - k*sigma/sqrt(n)) && (mu_real < X_n + k*sigma/sqrt(n))){
    suma = suma + 1
  }
}

print(suma/m)


# IC mu, sigma desconocido
n= 20
alpha = .05
k = qt(1-alpha/2, df = (n-1))
sigma = 1
mu_real = 10

X = rnorm(n, mu_real, sd = sigma)
X_n = mean(X)
S = sd(X) #sqrt(mean((X-X_n)**2)*n/(n-1))
c( X_n - k*S /sqrt(n),  X_n + k*S /sqrt(n))
#c( X_n - qnorm(1-alpha/2)*S /sqrt(n),  X_n + qnorm(1-alpha/2)*S /sqrt(n))

m = 100000
suma = 0
for (i in 1:m){
  X = rnorm(n, mu_real, sd = sigma)
  X_n = mean(X)
  S = sd(X)
  if ((mu_real > X_n - k*S/sqrt(n)) && (mu_real < X_n + k*S/sqrt(n))){
    suma = suma + 1
  }
}

print(suma/m)


# IC para sigma^2

n= 200
alpha = .05
sigma = 1
mu_real = 10

X = rnorm(n, mu_real, sd = sigma)
S2 = var(X)
c((n-1)*S2/qchisq(1-alpha/2, df = n-1), (n-1)*S2/qchisq(alpha/2, df = n-1))


m = 10000
suma = 0
for (i in 1:m){
  X = rnorm(n, mu_real, sd = sigma)
  S2 = var(X)
  
  if ((sigma > (n-1)*S2/qchisq(1-alpha/2, df = n-1)) && (sigma < (n-1)*S2/qchisq(alpha/2, df = n-1))){
    suma = suma + 1
  }
}

print(suma/m)



# IC Bernoulli

n= 100
p_true = 0.99
alpha = 0.05

X = rbinom(n, size = 1, prob = p_true)
X_n = mean(X)
k = qnorm(1-alpha/2)
c(X_n - k*sqrt(X_n*(1-X_n)/n), X_n + k*sqrt(X_n*(1-X_n)/n))



m = 100000
suma = 0
for (i in 1:m){
  X = rbinom(n, size = 1, prob = p_true)
  X_n = mean(X)
  
  if ((p_true > X_n - k*sqrt(X_n*(1-X_n)/n)) && (p_true < X_n + k*sqrt(X_n*(1-X_n)/n))){
    suma = suma + 1
  }
}

print(suma/m)



