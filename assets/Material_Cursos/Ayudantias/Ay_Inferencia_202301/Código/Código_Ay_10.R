library(ggplot2)
library(dplyr)
library(optimx)

cauchy.mle<-function(x,start,eps=1.e-8,max.iter=50){ 
  if(missing(start))
    start<-median(x) 
    theta<-start 
    n<-length(x) 
    score<-sum(2*(x-theta)/(1+(x-theta)^2)) 
    iter<-1 
    conv<-T 
    while(abs(score)>eps&&iter<=max.iter){ 
      info<-sum((2-2*(x-theta)^2)/(1+(x-theta)^2)^2) 
      theta<-theta+score/info 
      iter<-iter+1 
      score<-sum(2*(x-theta)/(1+(x-theta)^2)) 
      } 
    if(abs(score)>eps){ 
      print("NoConvergence") 
      conv<-F 
      } 
    loglik<- -sum(log(1+(x-theta)^2)) 
    info<-sum((2-2*(x-theta)^2)/(1+(x-theta)^2)^2) 
    r<-list(theta=theta,loglik=loglik,info=info,convergence=conv, start = start) 
    r 
} 

cauchy.mle_Scoring<-function(x,start,eps=1.e-8,max.iter=50){ 
  if(missing(start))
    start<-median(x) 
  theta<-start 
  n<-length(x) 
  score<-sum(2*(x-theta)/(1+(x-theta)^2)) 
  iter<-1 
  conv<-T 
  while(abs(score)>eps&&iter<=max.iter){ 
    info<-n*.5
    theta<-theta+score/info 
    iter<-iter+1 
    score<-sum(2*(x-theta)/(1+(x-theta)^2)) 
  } 
  if(abs(score)>eps){ 
    print("NoConvergence") 
    conv<-F 
  } 
  loglik<- -sum(log(1+(x-theta)^2)) 
  info<-n*.5 
  r<-list(theta=theta,loglik=loglik,info=info,convergence=conv, start = start) 
  r 
} 
#Newton-Raphson
set.seed(seed = 1234)
n<-100
location = 5
datos<-rcauchy(n, location = location)#100observationswiththeta=5 
hist(datos, freq = F, breaks = "Freedman-Diaconis", xlim=range(-15:20), ylim = c(0,.5))
r<-cauchy.mle(datos,start=4.5,max.iter=1000)
curve(dcauchy(x,location = r$theta), add=TRUE,col="red")
r

## Mal inicio
set.seed(seed = 12345)
datos<-rcauchy(n, location = location)#100observationswiththeta=5 
hist(datos, freq = F, breaks = "Freedman-Diaconis", xlim=range(-15:20), ylim = c(0,.5))
r<-cauchy.mle(datos,start=3.5,max.iter=50)
curve(dcauchy(x,location = r$theta), add=TRUE,col="red")
r

#Fisher_Scoring
set.seed(seed = 1234)
n<-100
location = 5
datos<-rcauchy(n, location = location)#100observationswiththeta=5 
hist(datos, freq = F, breaks = "Freedman-Diaconis", xlim=range(-15:20), ylim = c(0,.5))
r<-cauchy.mle_Scoring(datos,start=4.5,max.iter=1000)
curve(dcauchy(x,location = r$theta), add=TRUE,col="red")
r

## Mal inicio
set.seed(seed = 12345)
datos<-rcauchy(n, location = location)#100observationswiththeta=5 
hist(datos, freq = F, breaks = "Freedman-Diaconis", xlim=range(-15:20), ylim = c(0,.5))
r<-cauchy.mle_Scoring(datos,start=3.5,max.iter=50)
curve(dcauchy(x,location = r$theta), add=TRUE,col="red")
r

df = data.frame(x = datos)
log.chauchy <- function(data, par) {    # Own function for residual sum of squares
  with(data, sum(2*(x-par)/(1+(x-par)^2)))
}
optim_output <- optim(par = 3.5,    # Applying optim
                      fn = log.chauchy,
                      data = df,
                      method = "CG")
optim_output
optimx_output <- optimx(par = 7.5,    # Applying optim
                      fn = log.chauchy,
                      data = df,
                      method = "CG")
optimx_output

## Regresion Poisson

R_Poisson.mle<-function(x,y,start,eps=1.e-8,max.iter=50){ 
  if(missing(start))
    start<-c(1,log(mean(y))) 
  theta<-start #(2 variables)
  n<-length(x) 
  lambda = exp(theta[1]+theta[2]*x)
  score<- c(sum(1*(y-lambda)), sum(x*(y-lambda)))
  iter<-1 
  conv<-T 

  while(max(abs(score))>eps&&iter<=max.iter){ 
    
    info = matrix(0, nrow = 2,  ncol=2, byrow = TRUE)
    for (i in 1:n) {
      info <- info + rbind(c(1,x[i]),c(x[i],x[i]**2))*lambda[i]
    }
    
    
    theta<-theta +solve(info)%*%score 
    iter<-iter+1 
    lambda = exp(theta[1]+theta[2]*x)
    score<- c(sum(1*(y-lambda)), sum(x*(y-lambda))) 
  } 
  if(max(abs(score))>eps){ 
    print("NoConvergence") 
    conv<-F 
  } 
  loglik<- exp(theta[1]+theta[2]*x)
  info = matrix(0, nrow = 2,  ncol=2, byrow = TRUE)
  for (i in 1:n) {
    info <- info + rbind(c(n,x[i]),c(x[i],x[i]**2))*lambda[i]
  } 
  r<-list(loglik=loglik,info=info,convergence=conv, start = start, theta=theta) 
  r 
} 



set.seed(seed = 12345)
n<-1000
covariables <- runif(n,0,1)
#covariables <- (1:n)/(n*.5)

b<- .3
a<- 2
lambda =exp(b+a*covariables)
respuesta <- c(1:n)
for (i in 1:n) {
  respuesta[i] <- rpois(1,lambda[i])
  
}
df <- data.frame(respuesta, covariables)
ggplot(df, aes(x=covariables, y=respuesta)) + geom_point()


##Newton
r<-R_Poisson.mle(covariables, respuesta, start=c(0, 10) ,max.iter=1000)
r
lambda_est <- exp(r$theta[1]+r$theta[2]*covariables)
est <- data.frame(Lambda = lambda , Lambda_est = lambda_est, Error = abs(lambda -lambda_est), Error_relativ = abs(lambda -lambda_est)/(lambda) * 100 )
est
sum(abs(est$Lambda-est$Lambda_est))
#GLM
poisson.model <- glm(respuesta ~ covariables, df, family = poisson(link = "log"))
summary(poisson.model)
lambda_est <- exp(poisson.model$coefficients[1]+ poisson.model$coefficients[2]*covariables)
est <- data.frame(Lambda = lambda, Lambda_est = lambda_est, Error = abs(lambda -lambda_est), Error_relativ = abs(lambda-lambda_est)/(lambda) * 100 )
est
sum(abs(est$Lambda-est$Lambda_est))
