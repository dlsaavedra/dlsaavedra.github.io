
# Ayudantia 7 Estadistica Espacial -------

library(tidyverse)
# Nuevas librer?as
#library(R2WinBUGS)
library(spBayes)
library(coda)


#setwd("C:/Users/danie/OneDrive - Universidad Católica de Chile/Desktop/Doctorado_Estadistica/2013_2/Ayudantia_Espacial/Ayudantias/Ayudantia_7")
setwd("/Users/danielsaavedramorales/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Desktop/Doctorado_Estadistica/2013_2/Ayudantia_Espacial/Ayudantias/Ayudantia_7/")

#Pregunta A------
datos <- tibble(x = c(0.056,6.257,1.204,4.346,
                      4.902,9.800,7.624,4.258,
                      2.835,5.497),
                y = c(6.682,3.859,6.327,0.546,
                      6.787,9.167,2.265,7.670,
                      5.431,0.363),
                Y = c(5.972,3.391,4.891,5.352,
                      4.423,3.057,4.553,4.365,
                      7.374,6.554))
Y <- datos %>% 
  select(Y) %>% 
  pull()
coords <- datos %>% 
  select(-Y) %>% 
  as.matrix()


# Bayesian Spatial regression Models
?spLM

n.samples  =10000
starting <- list("beta" = 0, "sigma.sq" = 50,
                 "tau.sq" = 1, 
                 "phi" = 10, "nu" = 3)

tuning <- list("sigma.sq" = 0.1, "tau.sq" = 0.1,
               "phi" = 0.1, "nu" = 0.1)

priors <- list("beta.Norm" = list(0,1000), 
              "sigma.sq.IG" = c(0.001,0.001),
              "tau.sq.IG" = c(0.001, 0.001),
              "phi.Unif" = c(0.001,100),
              "nu.Unif" = c(0.001,100))

# Corremos el modelo
modelo_spBayes <- spLM(Y ~ 1,
                       coords = coords,
                       starting = starting,
                       tuning = tuning,
                       priors = priors,
                       n.samples = n.samples,
                       cov.model = "exponential",
                       verbose = T)


burn.in <- 0.2*n.samples
?spRecover
##recover beta and spatial random effects
m.1 <- spRecover(modelo_spBayes, start=burn.in, verbose=FALSE)

samp = modelo_spBayes$p.theta.samples[burn.in:10000,]
apply(samp,2,quant)
round(summary(m.1$p.theta.recover.samples)$quantiles[,c(3,1,5)],2)
round(summary(m.1$p.beta.recover.samples)$quantiles[c(3,1,5)],2)
m.1.w.summary <- summary(mcmc(t(m.1$p.w.recover.samples)))$quantiles[,c(3,1,5)]


# Corremos el modelo Predictivo
modelo_spBayes_pred <- spLM(Y ~ 1,
                       coords = coords,
                       knots=c(6,6,0.1),
                       starting = starting,
                       tuning = tuning,
                       priors = priors,
                       n.samples = n.samples,
                       cov.model = "exponential",
                       verbose = T)

round(summary(window(modelo_spBayes_pred$p.beta.samples, start=burn.in))$quantiles[c(3,1,5)],2)
round(summary(window(modelo_spBayes_pred$p.theta.samples, start=burn.in))$quantiles[,c(3,1,5)],2)



m.1.pred<-spPredict(modelo_spBayes,pred.covars=matrix(rep(1,10), ncol = 1, nrow=10),
                    pred.coords=coords, start=0.5*n.samples)

y.hat_m<-apply(m.1.pred$p.y.predictive.samples,1,mean) 
quant<-function(x){quantile(x,prob=c(0.025,0.5,0.975))} 
y.hat<-apply(m.1.pred$p.y.predictive.samples,1,quant); y.hat
plot(Y,y.hat_m,pch=19,cex=0.5,xlab="observedy",ylab="predictedy")
cbind(Y,y.hat_m)


x.easting <- seq(0,10, by = 2)
x.northing <- seq(0,10, by = 2)
Grid <- expand.grid(x.easting, x.northing)
Grid_m = matrix(unlist(Grid), ncol = 2)
K <- nrow(Grid_m)

m.1.pred<-spPredict(modelo_spBayes,pred.covars=matrix(rep(1,K), ncol = 1, nrow=K),
                    pred.coords=Grid_m, start=0.5*n.samples)

y.hat_m<-apply(m.1.pred$p.y.predictive.samples,1,mean) 
y.hat<-apply(m.1.pred$p.y.predictive.samples,1,quant) 
B = cbind(Grid_m, y.hat_m)
z <- matrix(B[,3], nrow = length(x.easting), ncol = length(x.easting))
persp(x.easting, x.northing, z)
require(rgl)  
surface3d(x.easting, x.northing, z)


library(ggplot2)   
# Load ggplot2 package
ggp <- ggplot(data.frame(B), aes(V1, V2)) +                           # Create heatmap with ggplot2
  geom_tile(aes(fill = y.hat_m))
ggp 

#Pregunta B------


setwd("C:/Users/danie/OneDrive - Universidad Católica de Chile/Desktop/Doctorado_Estadistica/2013_2/Ayudantia_Espacial/Ayudantias/Ayudantia_7")


library(gstat)
library(tidyverse) |> suppressPackageStartupMessages()
no2 <- read_csv(system.file("external/no2.csv", 
                            package = "gstat"), show_col_types = FALSE)
library(sf)
# Linking to GEOS 3.11.1, GDAL 3.6.2, PROJ 9.1.1; sf_use_s2() is TRUE
crs <- st_crs("EPSG:32632")
st_as_sf(no2, crs = "OGC:CRS84", coords = 
           c("station_longitude_deg", "station_latitude_deg")) |>
  st_transform(crs) -> no2.sf

#1.-Cargar los estados de Alemania
read_sf("de_nuts1.gpkg") |> st_transform(crs) -> de


ggplot() + geom_sf(data = de) + 
  geom_sf(data = no2.sf, mapping = aes(col = NO2))+
  scale_color_viridis_c(option = "plasma")

Y <- no2 %>% 
  select(NO2) %>% 
  pull()

coords <- no2 %>% 
  select(station_longitude_deg,station_latitude_deg) %>% 
  as.matrix()

X <-no2 %>% 
  select(station_altitude) %>% 
  pull()


# Bayesian Spatial regression Models
?spLM

n.samples  =10000
starting <- list("beta" = c(0,1), "sigma.sq" = 50,
                 "tau.sq" = 1, 
                 "phi" = 10, "nu" = 3)

tuning <- list("sigma.sq" = 0.1, "tau.sq" = 0.1,
               "phi" = 0.1, "nu" = 0.1)

priors <- list("beta.Norm" = list(rep(0,2), diag(1000,2)), 
               "sigma.sq.IG" = c(0.001,0.001),
               "tau.sq.IG" = c(0.001, 0.001),
               "phi.Unif" = c(0.001,100),
               "nu.Unif" = c(0.001,100))

# Corremos el modelo
modelo_spBayes <- spLM(Y ~ X,
                       coords = coords,
                       starting = starting,
                       tuning = tuning,
                       priors = priors,
                       n.samples = n.samples,
                       cov.model = "exponential",
                       verbose = T)


burn.in <- 0.2*n.samples

##recover beta and spatial random effects
m.1 <- spRecover(modelo_spBayes, start=burn.in, verbose=FALSE)
round(summary(m.1$p.theta.recover.samples)$quantiles[,c(3,1,5)],2)
round(summary(m.1$p.beta.recover.samples)$quantiles[,c(3,1,5)],2)
m.1.w.summary <- summary(mcmc(t(m.1$p.w.recover.samples)))$quantiles[,c(3,1,5)]


plot(Y, m.1.w.summary[,1], xlab="Observed Y", ylab="Fitted Y",
     xlim=range(Y), ylim=range(m.1.w.summary), main="Spatial random effects")
arrows(Y, m.1.w.summary[,1], Y, m.1.w.summary[,2], length=0.02, angle=90)
arrows(Y, m.1.w.summary[,1], Y, m.1.w.summary[,3], length=0.02, angle=90)
lines(range(Y), range(Y))

#Modelo predicción

# Corremos el modelo Predictivo
modelo_spBayes_pred <- spLM(Y ~ X,
                            coords = coords,
                            knots=c(10,10,0.1),
                            starting = starting,
                            tuning = tuning,
                            priors = priors,
                            n.samples = n.samples,
                            cov.model = "exponential",
                            verbose = T)

round(summary(window(modelo_spBayes_pred$p.beta.samples, start=burn.in))$quantiles[,c(3,1,5)],2)
round(summary(window(modelo_spBayes_pred$p.theta.samples, start=burn.in))$quantiles[,c(3,1,5)],2)




#Predicción
#### Set up a square lattice region
x.easting <- 1:10
x.northing <- 1:10
Grid <- expand.grid(x.easting, x.northing)
K <- nrow(Grid)


m.1.pred<-spPredict(modelo_spBayes,pred.covars= cbind(rep(1, length(X)),X),
                    pred.coords=coords, start=0.5*n.samples)

y.hat<-apply(m.1.pred$p.y.predictive.samples,1,mean) 
y.hat<-apply(m.1.pred$p.y.predictive.samples,1,quant) 
plot(Y,y.hat[2,],pch=19,cex=0.5,xlab="observedy",ylab="predictedy")
