muestras = 1e05
m = 20
p = 0.7
#theta = rgamma(theta, alpha_0, beta_0)
Y = 5
A = rpois(muestras, (1-p)*m)
eta = A + Y
hist(eta, freq = FALSE)
#mean(eta<5)
#4*exp(-3)

library(manipulate)

manipulate(
  hist(rpois(muestras, (1-p)*m) + Y,
        col = "red", xlim=c(0,110), ylim = c(0,1/(m+1)+.2), freq = FALSE),
  m=slider(0,100))

curve(dbeta(x,alpha_prior + y, beta_prior + n - y))