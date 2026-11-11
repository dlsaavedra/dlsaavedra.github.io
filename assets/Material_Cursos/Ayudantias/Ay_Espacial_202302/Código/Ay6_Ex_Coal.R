rm(list=ls())
quant025=function(x){quantile(x,0.025)}
quant975=function(x){quantile(x,0.975)} 
# full conditional of K

full = function(k,theta,lambda,y,n,alpha1,beta1,alpha2,beta2){ 
  theta^(alpha1-1+ifelse(k>1,sum(y[1:k]),0))*exp(-(beta1+k)*theta)*lambda^(alpha2-1+ifelse(k<n,sum(y[(k+1):n]),0))*exp(-(beta2+n-k)*lambda) } 
################################################################## #
# Real data application: Counts of coal mining disasters in  
# Great Britain by year from 1851 to 1962. 
# ################################################################## 
y = c(4,5,4,1,0,4,3,4,0,6,3,3,4,0,2,6,3,3,5,4,5,3,1,4,4,1,5,5,3,4,2,5,2,2,3,4,2,1,3,2,2,
      1,1,1,1,3,0,0,1,0,1,1,0,0,3,1,0,3,2,2,0,1,1,1,0,1,0,1,0,0,0,2,1,0,0,0,1,1,0,2,3,3,
      1,1,2,1,1,1,1,2,4,2,0,0,0,1,4,0,0,0,1,0,0,0,0,0,1,0,0,1,0,1) 
n = length(y)
seg1=y[1:40]
seg2=y[41:112] 
summary(y)

summary(seg1)        
summary(seg2)

par(mfrow=c(1,1))
plot(1851:1962,y,type="h",xlab="years",ylab="",main="",pch=2) 
title("Counts of coal mining disasters in Great Britain")
points(1891,0,pch=16,col=2) 
text(1910,6,paste("Sample mean up to 1891 = ",round(mean(y[1:41]),2),sep="")) 
text(1910,5.7,paste("Sample mean after 1891  = ",round(mean(y[42:n]),2),sep=""))


# hyperparameters# ---------------
alpha1 = 0.001 
beta1  = 0.001 
alpha2 = 0.001 
beta2 = 0.001
######################################### # Gibbs Sampler #########################################
set.seed(123456) 
# MCMC set up
k = 2          # Initial value for m 
M0     = 1000   # Burn-in
M      = 2000   # posterior draws 
niter  = M+M0 
draws  = matrix(0,niter,4)
time   = system.time( for (iter in 1:niter){ 
  theta = rgamma(1,ifelse(k>1,sum(y[1:k]),0)+alpha1, k+beta1)   
  lambda    = rgamma(1,ifelse(k<n,sum(y[(k+1):n]),0) + alpha2, n-k+beta2)   
  fulls  = NULL   
  for (j in 1:n)  {   
    fulls = c(fulls,full(j,theta,lambda,y,n,alpha1,beta1,alpha2,beta2))
    }
  fulls = fulls/sum(fulls)   
  k     = sample(1:n,size=1,prob=fulls)   
  draws[iter,] = c(theta,lambda, theta/lambda,k) })


draws = draws[(M0+1):niter,]  
summary = round(cbind(apply(draws,2,mean),sqrt(apply(draws,2,var)), 
                      apply(draws,2,quant025),apply(draws,2,quant975)),4)
summary[4,c(1,3,4)]=1850+round(summary[4,c(1,3,4)]);summary
# Posterior summaries # ------------------- 
ind   = seq(1,M,by=M/1000)  
par(mfrow=c(2,3))
plot(ind,draws[ind,1],xlab="iteration",ylab="",main=expression(theta),type="l") 
plot(ind,draws[ind,2],xlab="iteration",ylab="",main=expression(lambda),type="l") 
plot(ind,1850+draws[ind,4],xlab="iteration",ylab="",main="k",type="l") 
acf(draws[,1],main="")
acf(draws[,2],main="") 
acf(draws[,4],main="")

par(mfrow=c(1,2))
plot(ind,draws[ind,1]/draws[ind,2],xlab="iteration",ylab="",main="theta/lambda",type="l") 
acf(draws[,3],main="")


par(mfrow=c(1,3))
hist(draws[,1],xlab="",main=expression(theta),prob=TRUE);box() 
hist(draws[,2],xlab="",main=expression(lambda),prob=TRUE);box()
hist(draws[,3],xlab="",main="theta/lambda",prob=TRUE);box()


par(mfrow=c(1,2)) 
ns = rep(0,n)
for (i in 1:n){
  ns[i] = mean(draws[,4]==i) }

plot(1850+(1:n),ns,type="h",xlab="year",ylab="Probability",main="k") 
abline(h=1/n,lty=2) 
plot(table(1850+draws[,4])/M,type="h",xlab="year",main="k",ylab="Probability") 
abline(h=1/n,lty=2)

