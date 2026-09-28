# Poisson-Binomial -----

p.xy = function(x, y, p = 0.05, nu = 50){
  n.r = length(x)
  n.c = length(y)
  M = matrix(NA, ncol = n.c, nrow = n.r)
  for(i in 1:n.r){
    M[i,] = dbinom(y, size = x[i], prob = p)*dpois(x[i],
                                                   lambda = nu)
  }
  M
}
x = 0:100
y = 0:20
z = p.xy(x, y, p = 0.20, nu = 50)

Z = c(z)
X = rep(x, length(y))
Y = sort(rep(y, length(x)))
install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(x = X, y = Y, z = Z, type = "h", lwd=2, pch= "",
              xlab = "X", ylab = "Y", zlab = "", highlight.3d=TRUE, angle = 45)

install.packages("plotly")
library(plotly)

plot_ly(x=X, y=Y, z=Z, type="scatter3d", mode="markers", color=Z)

# Gamma Uniforme -----

f.xy = function(x, y, k = 3, nu = 2){
  n.r = length(x)
  n.c = length(y)
  M = matrix(NA, ncol = n.c, nrow = n.r)
  for(i in 1:n.r){
    M[i,] = dgamma(y, rate = nu, shape = k)*dunif(x[i], 0, y)
  }
  M
}
x = seq(0, 5, by = .01)
y = seq(0, 5, by = .01)
z = f.xy(x, y)

Z = c(z)
X = rep(x, length(y))
Y = sort(rep(y, length(x)))

library(plotly)
plot_ly(x=X, y=Y, z=Z, type="scatter3d", mode="markers", color=Z)

# Normal- Normal -----

f.xy = function(x, y, mu.x = 0, mu.y = 0, s.x = 1, s.y = 1,
                rho = 0){
  n.r = length(x)
  n.c = length(y)
  M = matrix(NA, ncol = n.c, nrow = n.r)
  for(i in 1:n.r){
    M[i,] = dnorm(x[i], mean = mu.x, sd = s.x) * dnorm(y, mean = mu.y
                                                       + rho*s.y*(x[i]-mu.x)/s.x, sd = s.y*sqrt(1-rho^2))
  }
  M
}
x = seq(-5, 5, by = .1)
y = seq(-5, 5, by = .1)
z = f.xy(x, y, mu.y = 1,rho = 0)
Z = c(z)
X = rep(x, length(y))
Y = sort(rep(y, length(x)))

plot_ly(x=X, y=Y, z=Z, type="scatter3d", mode="markers", color=Z)



# Generamos un data.frame para los tres valores de rho
rhos <- c(0, 0.5, 0.99)
# Generamos un plot por cada rho
plots <- lapply(rhos, function(rho) {
  z <- f.xy(x, y, rho = rho)
  X <- rep(x, length(y))
  Y <- sort(rep(y, length(x)))
  Z <- as.vector(z)
  
  plot_ly(x = X, y = Y, z = Z,
          type = "scatter3d", mode = "markers",
          name = paste0("rho = ", rho),
          color = Z, colors = colorRamp(c("blue", "red")))
})

# Unimos en un mismo layout
subplot(plots, nrows = 1, shareX = TRUE, shareY = TRUE, titleX = TRUE, titleY = TRUE)
