# Instalar y cargar el paquete mclust
#install.packages("mclust")
library(mclust)

# Cargar los datos
data(faithful)

# Ver las primeras filas del conjunto de datos
head(faithful)
plot(faithful)
# Ajustar el modelo de mezcla de gaussianas a las características del géiser
fit <- Mclust(faithful)

# Resumen del modelo ajustado
summary(fit)

# Visualización de la clasificación de los datos según el modelo ajustado
plot(fit, what = "classification")

# Visualización de los valores de BIC para diferentes números de componentes
plot(fit, what = "BIC")
fit$BIC
mclustBIC(faithful)

## Prioir
fit_prior <- Mclust(faithful, G = 1:9, prior = priorControl(mean = c(100, 100), 
                                                        #scale = cbind(c(100,0),c(0,.01))))
                                                        scale = diag(c(1,1))))
# Visualización de la clasificación de los datos según el modelo ajustado
plot(fit_prior, what = "classification")

# Visualización de los valores de BIC para diferentes números de componentes
plot(fit_prior, what = "BIC")

# Priori Para la varianza

# Ajustar el modelo de mezcla de gaussianas a las características del géiser
fit <- Mclust(faithful, G = 1:10, prior = priorControl(shinkage = 0, 
                                                       #scale = diag(c(1,.1)),
                                                       scale = cbind(c(100,.5),c(0.5,100))))

# Resumen del modelo ajustado
summary(fit)
# Visualización de la clasificación de los datos según el modelo ajustado
plot(fit, what = "classification")

# Visualización de los valores de BIC para diferentes números de componentes
plot(fit, what = "BIC")


fit$BIC



