library(CARBayes)
library(MASS)
#Pregunta 2 A----

#### Set up a square lattice region
x.easting <- 1:4
x.northing <- 1:4
Grid <- expand.grid(x.easting, x.northing)
K <- nrow(Grid)

#### set up distance and neighbourhood (W, based on sharing a common border) matrices
distance <- as.matrix(dist(Grid))
W <-array(0, c(K,K))
W[distance==1] <- 1 	

#### Generate the covariates and response data
x1 <- rnorm(K)
x2 <- rnorm(K)
theta <- rnorm(K, sd=0.05)
phi <- mvrnorm(n=1, mu=rep(0,K), Sigma=0.4 * exp(-0.1 * distance))
logit <- x1 + x2 + theta + phi
prob <- exp(logit) / (1 + exp(logit))
trials <- rep(50,K)
Y <- rbinom(n=K, size=trials, prob=prob)


#### Run the BYM model
formula <- Y ~ x1 + x2
## Not run: 
?S.CARbym
model <- S.CARbym(formula=formula, 
                  family="binomial", trials=trials,
                  W=W, burnin=2000, n.sample=100000,
                  prior.tau2  = c(.001,.001))

dev.off()
plot(model$samples$beta[90000:98000,3], type = "l")
plot(model$samples$sigma2[90:98000], type = "l")
plot(model$samples$tau2[90:98000], type = "l")
plot(model$samples$psi[90000:98000,5], type = "l")
plot(model$samples$fitted[90000:98000, 4], type = "l")
## End(Not run)
?S.CARbym
#### Toy example for checking
model <- S.CARbym(formula=formula, family="binomial", trials=trials,
                  W=W, burnin=20, n.sample=50)


Y.hat = model$fitted.values

plot(Y,Y.hat,pch=19,cex=0.5, xlab="observedy",ylab="predictedy")

quant<-function(x){quantile(x,prob=c(0.025,0.5,0.975))} 
beta<-apply(model$samples$beta,2,quant);beta


plot(model$residuals$response)

#### Run the BYM model Leroux
formula <- Y ~ x1 + x2
## Not run: 
model_L <- S.CARleroux(formula=formula, family="binomial", trials=trials,
                  W=W, burnin=20000, n.sample=100000)
## End(Not run)

#### Toy example for checking
model_L <- S.CARleroux(formula=formula, family="binomial", trials=trials,
                  W=W, burnin=20, n.sample=50)

plot(model_L$samples$beta[90000:98000,3], type = "l")
plot(model_L$samples$sigma2[90:98000], type = "l")
plot(model_L$samples$tau2[90:98000], type = "l")
plot(model_L$samples$psi[90000:98000,5], type = "l")
plot(model_L$samples$fitted[90000:98000, 4], type = "l")

plot(Y,model_L$fitted.values,pch=19,cex=0.5, xlab="observedy",ylab="predictedy", col = "blue")
points(Y,model$fitted.values,pch=19,cex=0.5,col = "red", add=TRUE)

quant<-function(x){quantile(x,prob=c(0.025,0.5,0.975))} 
beta_L<-apply(model_L$samples$beta,2,quant);beta_L

plot(model_L$residuals$response)


#### Run the BYM model glm
formula <- Y ~ x1 + x2
## Not run: 
model_glm <- S.glm(formula=formula, family="binomial", trials=trials,
                 burnin=2000, n.sample=100000)
## End(Not run)

plot(model_glm$samples$beta[90000:98000,3], type = "l")

plot(model_glm$samples$fitted[9000:98000, 4], type = "l")

plot(Y,pch=1,cex=0.5, xlab="index",ylab="Y Value", col = "black")
points(model_glm$fitted.values,pch=2,cex=0.5,col = "blue")
points(model_L$fitted.values,pch=3,cex=0.5,col = "red")
points(model$fitted.values,pch=4,cex=0.5,col = "green")
legend("topright", "Data Points", 
       legend = c("Real", "Glm", "BYM_L","BYM"),
       col = c("black","blue", "red","green" ),
       pch = 1:4)
quant<-function(x){quantile(x,prob=c(0.025,0.5,0.975))} 
beta_glm<-apply(model_glm$samples$beta,2,quant);beta_L

# Residuals
plot(model$residuals$response, col = "green", ylim = c(-3, 15))
points(model_L$residuals$response, col = "red")
points(model_glm$residuals$response, col = "blue")
legend("topright", "Data Points", 
       legend = c("BYM", "BYM_L", "Glm"),
       col = c("green", "red", "blue"),
       pch = 1)


# Pregunta 2 B ------
# Viejas librer?as
 library(mapview)
# Nuevas librer?as
library(spdep); library(gstat)

library(RCurl)

Chile = st_read("../Ayudantia_1/Comunas/", "comunas")
Santiago = Chile %>% dplyr::filter( Region == "Región Metropolitana de Santiago"  )

### Extraemos los datos de casos covid acumulados en el tiempo
url = "datos_Mortalidad1000_RM_2020.csv"
Datos = read_csv(url)

head(Datos)
Datos_X = Datos %>%
  rename(`Comuna` = `Unidad territorial`)%>%
  rename(`Muertes` = `2020`)%>%
  mutate(Comuna2 = tolower(iconv(Comuna, to = "ASCII//TRANSLIT")))


#Juntamos los datos de las comunas con los casos covid
Data_final = Santiago %>%
  mutate(Comuna2 = tolower(iconv(Comuna, to = "ASCII//TRANSLIT")))%>%
  left_join(Datos_X, by = "Comuna2")





### Opciones de visualización ###
# Op1
mapview(Data_final["Muertes"])



library(spdep)
library(sp)
Data_final_sp = as_Spatial(Data_final)
Data_final_sp$Comuna.x # La posicion del vector es la identificacion
# Cada vez que querramos usar funciones de liberias sp__ debemos asegurarnos
# de que el objeto sea tipo as_Spatial
## Primero genreamos los vecinos

help(poly2nb)
vecinos1 = poly2nb(Data_final_sp) 
# Toma un objeto que tenga poligonos y lo transforma en un objeto de tipo vecinos
vecinos1
summary(vecinos1)
plot(Data_final_sp)
plot(vecinos1, coordinates(Data_final_sp), add = TRUE)
W_1_b = nb2listw(vecinos1, style = "B")
# Matriz de pesos:
matrixW_1_b = listw2mat(W_1_b)


#### Run the BYM model S.CARleroux
formula <- Data_final$Muertes ~ Data_final$st_area_sh
## Not run: 
model_L <- S.CARleroux(formula=formula, family="gaussian",
                  W=matrixW_1_b, burnin=2000, n.sample=100000)
## End(Not run)

#### Toy example for checking
model <- S.CARleroux(formula=formula, family="gaussian",
                  W=matrixW_1_b, burnin=20, n.sample=50)

plot(model_L$samples$beta[90000:98000,2], type = "l")
plot(model_L$samples$rho[9000:98000], type = "l")
plot(model_L$samples$tau2[90:98000], type = "l")
plot(model_L$samples$phi[90000:98000,5], type = "l")
plot(model_L$samples$nu2[90000:98000], type = "l")
plot(model_L$samples$fitted[90000:98000, 4], type = "l")


plot(Data_final$Muertes,model$fitted.values,pch=19,cex=0.5, xlab="observedy",ylab="predictedy")
plot(residuals(model))
quant<-function(x){quantile(x,prob=c(0.025,0.5,0.975))} 
beta<-apply(model$samples$beta,2,quant);beta



#### Toy example for checking
?S.glm
model_glm <- S.glm(formula=formula, family="gaussian",
                   burnin=2000, n.sample=100000)

plot(model_glm$samples$beta[90000:98000,2], type = "l")
plot(model_glm$samples$nu2[90000:98000], type = "l")
plot(model_glm$samples$fitted[90000:98000, 1], type = "l")

plot(Data_final$Muertes,model_glm$fitted.values,pch=19,cex=0.5, xlab="observedy",ylab="predictedy")
plot(residuals(model_glm))
quant<-function(x){quantile(x,prob=c(0.025,0.5,0.975))} 
beta<-apply(model_glm$samples$beta,2,quant);beta


plot(Data_final$Muertes,pch=1,cex=0.5, xlab="index",ylab="Y Value", col = "black")
points(model_glm$fitted.values,pch=2,cex=0.5,col = "blue")
points(model_L$fitted.values,pch=3,cex=0.5,col = "red")
legend("topright", "Data Points", 
       legend = c("Real", "Glm", "BYM_L"),
       col = c("black","blue", "red" ),
       pch = 1:4)
