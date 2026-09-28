# Lectura 1 -----

1 - ppois(9, lambda = 9)

n = 1e7
A = rpois(n, lambda = 2)
B = rpois(n, lambda = 3)
C = rpois(n, lambda = 4)
Z = A + B + C
mean(Z>9)
