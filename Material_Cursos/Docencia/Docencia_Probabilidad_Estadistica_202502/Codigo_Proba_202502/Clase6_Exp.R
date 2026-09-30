# Instalar si no lo tienes
install.packages("readxl")

# Cargar la librería
library(readxl)

# Leer un archivo .xlsx
df <- read_excel("data/Sismos2021.xlsx")

#Tiempo entre sismos 
min = df$Tiempo/60
hist(min, breaks = 40, freq = F, xlab = "Minutos")

curve(dexp(x, rate = 1), col = "red", add = T, lwd =2)
curve(dexp(x, rate = 1.5), col = "green", add = T, lwd =2)
curve(dexp(x, rate = 0.5), col = "blue", add = T, lwd =2)

mean(min)
curve(dexp(x, rate = 1/mean(min)), col = "purple", add = T, lwd =2)
