
# Ejercicio 4 ------
mc = 1e2
n = 1e6
resultado = rep(0,mc)
# Básica
pb <- txtProgressBar(min = 0, max = mc, style = 3)


for (i in 1:mc){
  set.seed(i)
  x=rnorm(n)
  resultado[i] = log(sum(x^2)/sum(x^4))
  setTxtProgressBar(pb, i)
}
close(pb)
cat("Valor esperanza (desv) del estimado :", mean(resultado),"(",sd(resultado),")", ", n =", n)

n_unico = n*mc
x=rnorm(n_unico)
resultado_unico = log(sum(x^2)/sum(x^4))

cat("Valor esperanza (desv) del estimado:", resultado_unico, ", n =", n_unico)
cat("Valor real:", -log(3))



# Ejercicio 5 ------
mc = 1e2
n = 1e6
resultado = rep(0,mc)
# Básica
pb <- txtProgressBar(min = 0, max = mc, style = 3)


for (i in 1:mc){
  set.seed(i)
  x=rnorm(n)
  resultado[i] = sum(x^2)/sum((x-1)^2)
  setTxtProgressBar(pb, i)
}
close(pb)
cat("Valor esperanza (desv) del estimado :", mean(resultado),"(",sd(resultado),")", ", n =", n)

n_unico = n*mc
x=rnorm(n_unico)
resultado_unico = sum(x^2)/sum((x-1)^2)

cat("Valor esperanza (desv) del estimado:", resultado_unico, ", n =", n_unico)
cat("Valor real:", 1/2)
