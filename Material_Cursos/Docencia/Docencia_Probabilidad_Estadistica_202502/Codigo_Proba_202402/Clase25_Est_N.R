n = 100
mu = 10
sigma = 1

m = 1000

est1 = numeric(m)
est2 = numeric(m)
est3 = numeric(m)
est4 = numeric(m)

for (j in 1:m){
  
  x = rnorm(n, mean = mu, sd = sigma)
  est1[j] = (max(x) + min(x))/2
  est2[j] = (quantile(x, probs = .25) + quantile(x, probs = .75))/2
  est3[j] = quantile(x, probs = 0.5)
  est4[j] = mean(x)
}

par(mfrow = c(2, 4))  # 2 filas, 4 columnas para los gráficos
hist(est1, main = paste("Histograma Data", "est1"), xlab = "Valores")
boxplot(est1, main = paste("Boxplot Data", "est1"), xlab = "Valores")
hist(est2, main = paste("Histograma Data", "est2"), xlab = "Valores")
boxplot(est2, main = paste("Boxplot Data", "est2"), xlab = "Valores")
hist(est3, main = paste("Histograma Data", "est3"), xlab = "Valores")
boxplot(est3, main = paste("Boxplot Data", "est3"), xlab = "Valores")
hist(est4, main = paste("Histograma Data", "est4"), xlab = "Valores")
boxplot(est4, main = paste("Boxplot Data", "est4"), xlab = "Valores")

resumen <- data.frame(
  Conjunto = paste("Data", 1:4),
  Media = sapply(list(est1, est2,
                      est3,est4), mean),
  Bias = mu-sapply(list(est1, est2,
                        est3,est4), mean),
  Desviacion_Estandar = sapply(list(est1, est2,
                                    est3,est4), sd)
)

print(resumen)
