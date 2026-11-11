library(lubridate)
day(date)
data = readxl::read_xlsx("data/Sismos2021.xlsx")

n = nrow(data)
hist(data$Tiempo/(60*24), freq = FALSE, breaks = 50)
theta = n/sum(data$Tiempo/(60*24))

curve(dexp(x, rate = theta), add = T, col = "red")


Fecha <- as.Date(data$Fecha, format="%Y-%m-%d")
sismos_por_dia_df <- as.data.frame(table(Fecha))
sismos_por_dia_df$dia = lubridate::day(sismos_por_dia_df$Fecha)
barplot(sismos_por_dia_df$Freq,
        names.arg = sismos_por_dia_df$dia,
        las = 2, # Rotar las etiquetas del eje X para mejor legibilidad
        col = "lightblue", 
        border = "black",
        main = "Número de sismos por día",
        xlab = "Fecha",
        ylab = "Número de sismos",
        cex.names = 0.7) # Ajusta el tamaño de las etiquetas



set.seed(1113) ## Semilla de simulaci´on 
#U = runif(20000, 2, 5) 

U = sample(1:10, 200, replace = TRUE)
hist(U, freq = FALSE, col = "gray", border = "white", 
     las = 1, nclass = 5, main = "", xlab = expression(Theta[U]))

table(U)
