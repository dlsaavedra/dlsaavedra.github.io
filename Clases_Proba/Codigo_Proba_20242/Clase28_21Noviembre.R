# Verdad
p = 0.4
#Muestra
n = 100
X = rbinom(n,1, prob = p)
# EMV
p_emv = mean(X)
#Hipótesis H_0: p = p_0 vs p!=p_0
p_0 = 0.5
I_0 = 1/sqrt(p_0*(1-p_0)/n)
#Significancia
alpha = 0.05
#Test estadístico
Z_0 = I_0*(p_emv - p_0);Z_0
#Intervalo de No rechazo
c(qnorm(alpha/2), qnorm(1-alpha/2))

# P valor
1-(pnorm(abs(Z_0))-pnorm(-abs(Z_0)))
