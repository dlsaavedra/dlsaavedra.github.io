# Datos
#x1 <- c(5.1,4.9,5.0,5.3,5.2,4.8,5.4,5.0,5.1,5.2,5.0,4.9)
#x2 <- c(4.7,4.8,4.9,4.6,4.5,4.8,4.7,4.6,4.9,4.8)

sigma1 = 3
sigma2  = 3
x1 =rnorm(8, mean = -1, sd = sigma1);x1
x2 =rnorm(10, mean = 1, sd = sigma2);x2

hist(x1, col = rgb(0,0,1,.5), xlim = c(-30,30)); hist(x2, col = rgb(1,0,0,.5), add = T)
legend("topleft", c("x1", "x2"), col = c(rgb(0,0,1,.5), rgb(1,0,0,.5)), lty=1 )
# Estadísticos
mean(x1); var(x1); sd(x1); length(x1)
mean(x2); var(x2); sd(x2); length(x2)

# Estadístico Z
mean_diff <- mean(x1) - mean(x2)
se <- sqrt(sigma1^2/length(x1) + sigma2^2/length(x2))
Z_0 <- mean_diff / se;Z_0

# p-valor bilateral
p_value <- 2 * (1 - pnorm(abs(Z_0)));p_value
alpha = 0.05
if(p_value > alpha){
  print("No Rechazar H0")
  }else{print("Rechazar H0")}


#Región de No Rechazo

c(qnorm(alpha/2), qnorm(1-alpha/2))
if(Z_0 > qnorm(alpha/2) && Z_0< qnorm(1-alpha/2)){
  print("No Rechazamos")
}else{print("Rechazamos")}

# IC u_x - u_y
c(mean_diff - qnorm(1-alpha/2) * se,
  mean_diff + qnorm(1-alpha/2) * se)


#install.packages("BSDA")
library(BSDA)

z.test(x1, x2,
       sigma.x = sigma1,   # desviación estándar poblacional de X1
       sigma.y = sigma2,   # desviación estándar poblacional de X2
       alternative = "two.sided",
       mu = 0)




# Varianzas iguales
t.test(x1, x2, var.equal = TRUE)

# Cálculo manual complementario
n1 <- length(x1); n2 <- length(x2)
m1 <- mean(x1); m2 <- mean(x2)
s1sq <- var(x1); s2sq <- var(x2)
sp2 <- ((n1-1)*s1sq + (n2-1)*s2sq)/(n1+n2-2)
sp <- sqrt(sp2)
se_pooled <- sqrt(sp2*(1/n1 + 1/n2))
t_stat <- (m1 - m2) / se_pooled
df <- n1 + n2 - 2
t_crit <- qt(0.975, df)
ci <- (m1 - m2) + c(-1,1)*t_crit*se_pooled
list(mean_diff = m1-m2, t = t_stat, df = df, p_value = 2*pt(-abs(t_stat), df),
     CI = ci) 


# Prueba t de varianzas distintas
t.test(x1, x2, var.equal = FALSE)

# Intervalo de confianza y Prueba de Hipótesis (manual)
diff_mean <- mean(x1)-mean(x2)
se <- sqrt(var(x1)/length(x1) + var(x2)/length(x2))
# df de Welch (approx)
s1sq <- var(x1); s2sq <- var(x2); n1 <- length(x1); n2 <- length(x2)
df <- (s1sq/n1 + s2sq/n2)^2 / ( (s1sq^2)/(n1^2*(n1-1)) + (s2sq^2)/(n2^2*(n2-1)) )
ci <- diff_mean + qt(c(0.025,0.975), df)*se
# pooled sd para Cohen's d
sp <- sqrt(((n1-1)*s1sq + (n2-1)*s2sq)/(n1+n2-2))
d <- diff_mean / sp
list(diff_mean=diff_mean, se=se, df=df, ci=ci, T_0=d)



# Prueba de varianza 
var.test(x1, x2)  # test F
F_0 = s1sq/s2sq;F_0
# Grados de libertad
df1 <- length(x1) - 1
df2 <- length(x2) - 1
#REgión de No Rechazo
c(qf(alpha/2, length(x1) - 1, length(x2) - 1), qf(1-alpha/2, length(x1) - 1, length(x2) - 1))

# IC
c(1/qf(1-alpha/2, df1,  df2)* s1sq/s2sq, 1/qf(alpha/2,  df1,  df2) * s1sq/s2sq)


# Pvalue
p_val <- 2 * min(
  pf(F_0, df1, df2),              # cola izquierda
  1 - pf(F_0, df1, df2)           # cola derecha
); p_val

