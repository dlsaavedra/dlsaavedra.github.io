#install.packages("rjags")
#Instalar jags desde https://sourceforge.net/projects/mcmc-jags/
# Ejemplos https://rstudio-pubs-static.s3.amazonaws.com/272658_ae4d482c86514674be17042c852ebbfc.html
##### load libraries
library(rjags)
library(coda)
setwd("C:/Users/danie/OneDrive - Universidad Católica de Chile/Desktop/Doctorado_Estadistica/2013_2/Ayudantia_Espacial/Ayudantias/Ayudantia_6/")
#setwd("/Users/danielsaavedramorales/Library/CloudStorage/OneDrive-UniversidadCatólicadeChile/Desktop/Doctorado_Estadistica/2013_2/Ayudantia_Espacial/Ayudantias/Ayudantia_6/")

################################################################## #
#First Model
###########################################################


#Data
dat <-list(x = c(6.62, 6.71, 5.07, 4.39, 5.68, 3.94, 5.83, 2.31, 3.60, 4.64,
           1.79, 3.12, 3.46, 8.25, 5.49, 6.49, 2.65, 9.14, 5.31, 6.58), y =
       c(9.06, 7.00, 8.59, 8.70, 8.64, 8.03, 9.27, 6.01, 7.92, 6.20, 6.39,
         9.10, 7.63, 6.75, 8.88, 8.44, 8.95, 5.66, 9.78, 8.09)) 
##### Initial values
inits <-list( mu=c(100,100), prec=c(1,1))

#### Alternatively, you can enter the mode directly in R:
cat("
model{
 for (i in 1:20){
 x[i] ~ dnorm (mu[1], prec[1])
 y[i] ~ dnorm (mu[2], prec[2])
 }
 mu[1] ~ dnorm (0, 0.0001)
 mu[2] ~ dnorm (0, 0.0001)
 prec[1] ~ dgamma (0.001, 0.001)
 prec[2] ~ dgamma (0.001, 0.001)
 s2[1] <- 1/prec[1]
 s2[2] <- 1/prec[2]
mu.diff <- mu[1] - mu[2]
var.ratio <- s2[1]/s2[2]

} ", file="HelloWorld_JAGS.txt")


?jags.model
## Set up the JAGS model.
jags.m <- jags.model( file = "HelloWorld_JAGS.txt", data=dat, inits=inits, n.chains=1, n.adapt=500 )

## specify parameters to be monitored
params <- c("mu", "s2", "mu.diff", "var.ratio")

## run JAGS and save posterior samples
?coda.samples
samps <- coda.samples( jags.m, params, n.iter=5000, thin = 1)
## summarize posterior samples
summary(samps)


plot(samps[,"mu[1]"])



################################################################## #
# Real data application: Counts of coal mining disasters in  
# Great Britain by year from 1851 to 1962. 
# ################################################################## 
y = c(4,5,4,1,0,4,3,4,0,6,3,3,4,0,2,6,3,3,5,4,5,3,1,4,4,1,5,5,3,4,2,5,2,2,3,4,2,1,3,2,2,
      1,1,1,1,3,0,0,1,0,1,1,0,0,3,1,0,3,2,2,0,1,1,1,0,1,0,1,0,0,0,2,1,0,0,0,1,1,0,2,3,3,
      1,1,2,1,1,1,1,2,4,2,0,0,0,1,4,0,0,0,1,0,0,0,0,0,1,0,0,1,0,1) 
n = length(y)

##### now prepare dat for JAGS
## N is the number of entries (e.g., 7)
## y is the outcome in the data ()

dat <- list("N" = n, "y" = y)  # names list of numbers

##### Initial values
inits <- list(lambda = c(1,2),K = 100)


##### define JAGS model within R
## The BUGS model is saved as a text or jag file. I used Notepad ++ to build the model
## but you can use any text editor. Please do not use Words.

#### Alternatively, you can enter the mode directly in R:
cat("
model {
for(i in 1:N){

y[i] ~ dpois(lambda[ind[i]])
ind[i] <- 1 + step(i-K-0.01)
pk[i] <- 1/N
}

 ### Define the priors
K ~ dcat(pk[]); 
lambda[1] ~ dgamma(0.001, 0.001)
lambda[2] ~ dgamma(0.001, 0.001)

ratio <- lambda[1]/lambda[2]
actual <- K + 1850

    }", file="Coal_JAGS.txt")



## Set up the JAGS model.
jags.m <- jags.model( file = "Coal_JAGS.txt", data=dat, inits=inits, n.chains=1, n.adapt=500 )

## specify parameters to be monitored
params <- c("lambda", "K", "ratio")

## run JAGS and save posterior samples
samps <- coda.samples( jags.m, params, n.iter=10000, thin = 2 )
## summarize posterior samples
summary(samps)

summary(window(samps, start= 201))  # Burn in of 200 Start at 201

plot(samps[, "K"])
