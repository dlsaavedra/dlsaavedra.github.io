#install.packages("mclust")
library(mclust)
set.seed(123)

data <- rbind(
  matrix(rnorm(10, mean = 0, sd = 1), ncol = 2),
  matrix(rnorm(10, mean = 2, sd = 1), ncol = 2)
)

plot(data)

?mclust
fit <- Mclust(data, G = 2)
summary(fit)

plot(fit,what = "classification")

points(data[1:5,], col = "green", lty = 2)

?mclustModelNames
plot(fit, what = "BIC")

mclustBIC(data)


fit$parameters


dens = densityMclust(data, G = 2)
summary(dens)

## Prioris

fit_prior <- Mclust(data, G = 1:9, 
                    prior = priorControl(mean = c(10, 10), 
                                                      #scale = cbind(c(100,0),c(0,.01))))
                                                      scale = diag(c(2,10))))
summary(fit_prior)

plot(fit_prior, what = "classification", xlim = c(-5,5), ylim = c(-5,5))

points(data[1:5,], col = "green", lty = 2)



plot(fit_prior, G = 1: 5, what = "BIC")

fit_prior$parameters

## Priori solo en la varianza

fit_no_prior_mean <- Mclust(data, G = 1: 5, 
                            prior = priorControl(shrinkage = 0,  
                                                 scale = diag(c(2,10))))
summary(fit_no_prior_mean)

plot(fit_no_prior_mean, what = "classification")

points(data[1:2,], col = "green", lty = 2)


plot(fit_no_prior_mean, G = 1: 5, what = "BIC")

fit$parameters


# Base de datos más grande

library(mclust)
set.seed(123)
data <- rbind(
  matrix(rnorm(100, mean = 0, sd = 0.2), ncol = 2),
  matrix(rnorm(20, mean = 1, sd = 0.2), ncol = 2)
)

plot(data)

fit <- Mclust(data, G = 1:9)
summary(fit)

plot(fit, G = 1:9,what = "classification")

points(data[1:50,], col = "green", lty = 2)

fit$BIC
plot(fit, what = "BIC")

fit$parameters


## Prioris

fit_prior <- Mclust(data, G = 1:5, prior = priorControl(mean = c(100, 100), 
                                                        scale = cbind(c(10,0),c(0,1))))
summary(fit_prior)

plot(fit_prior, what = "classification", xlim = c(-2,2), ylim = c(-2,2))

points(data[1:50,], col = "green", lty = 2)


plot(fit_prior, G = 1: 5, what = "BIC")

fit$parameters

## Priori solo en la varianza

fit_no_prior_mean <- Mclust(data, G = 1: 5, prior = priorControl(shrinkage = 0))
summary(fit_no_prior_mean)

plot(fit_no_prior_mean, what = "classification")

points(data[1:50,], col = "green", lty = 2)

fit_no_prior_mean$BIC
plot(fit_no_prior_mean, G = 1: 5, what = "BIC")

fit$parameters

