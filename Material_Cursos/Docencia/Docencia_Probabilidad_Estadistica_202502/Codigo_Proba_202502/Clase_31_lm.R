set.seed(123)

n = 100

b0 = 1
b1 = 3
sigma = 1

x = runif(n,-1,1)

y = b0 + b1*x + rnorm(n, mean = 0, sd = sigma)

plot(x,y, xlim = c(-2,2))

b1_hat = (sum(x*y) - n*mean(y)*mean(x))/(sum(x^2) - n*mean(x)^2);b1_hat
b0_hat = mean(y) - b1_hat*mean(x);b0_hat
sigma2_hat = mean((y - b0_hat - b1_hat*x)^2);sigma2_hat 
sqrt(sigma2_hat)

S2_hat = mean((y - b0_hat - b1_hat*x)^2) * n /(n-2);S2_hat
sqrt(S2_hat)

y_hat = b0_hat + b1_hat*x
plot(x,y, xlim = c(-2,2))
points(x, y_hat, col = "red", add = T)

SCR = sum((y_hat - mean(y))^2);SCR
SCE = sum((y - y_hat)^2);SCE

ml = lm(y ~ x);summary(ml)
