nrep = 1e4

n = 1e4 # 
m = 1 # Bernoulli
p = 0.99
# Mean Z = m*p, var(Z) = m*(1-p)*p/n
Z = replicate(nrep, mean(rbinom(n, m, p)))
mean(Z)
var(Z)
hist(Z, freq = F, breaks = 50)
curve(dnorm(x, mean = m*p, sd = sqrt(m*(1-p)*p/n)), 
      add = T, col = "red")
