rm(list=ls())
####### complete formula #######
fv = function(x,mu,sigma) 
  (1/(sqrt(2*pi)*sigma))*exp(-(x-mu)^2/(2*sigma^2))

fy = function(t,v) 
  gamma(0.5*(v+1))/(sqrt(v*pi)*gamma(0.5*v))*(1+t^2/v)^(-(v+1)*0.5)

ar = function(n,sigma,v)
{
  mu=0
  ### find maximum of M ###
  xm = seq(-1,1,by=0.01)
  Ym = fy(xm,v)
  Vm = fv(xm,mu,sigma)
  #plot(xm,Vm,col="red")
  #points(xm,Ym,col="blue")
  M=rep(NA,0.5*length(xm))
  M = Ym/Vm
  #plot(M)
  m=max(M)

  ### generate Y ###
  Y=rep(NA,n)
  i=0
  j=0
  while(i<n)
    {
    U=runif(1)
    V=rnorm(1)
    j=j+1
    if(U<(fy(V,v)/fv(V,mu,sigma)/m))
      {
      i=i+1
      Y[i]=V
    }
  }
  #plot(Y)
  out=list(Y=Y,efficiency=i/j)
  return(out)
  }   


####### kernel #######
fv1 = function(x,mu,sigma) exp(-(x-mu)^2/(2*sigma^2))

fy1 = function(t,v) (1+t^2/v)^(-(v+1)*0.5)

ar1 = function(n,sigma,v)
{
  mu=0
  ### find maximum of M ###
  xm = seq(-1,1,by=0.01)
  Ym = fy1(xm,v)
  Vm = fv1(xm,mu,sigma)
  #plot(xm,Vm,col="red")
  #points(xm,Ym,col="blue")
  M=rep(NA,0.5*length(xm))
  M = Ym/Vm
  #plot(M)
  m=max(M)
  
  ### generate Y ###
  Y=rep(NA,n)
  i=0
  j=0
  while(i<n)
  {
    U=runif(1)
    V=rnorm(1)
    j=j+1
    if(U<(fy1(V,v)/fv1(V,mu,sigma)/m))
    {
      i=i+1
      Y[i]=V
    }
  }
  #plot(Y)
  out=list(Y=Y,efficiency=i/j)
  return(out)
}   

####### test and comparison #######  

t = ar(10000,1,4)$Y
t1 = ar1(1000,1,4)$Y
t2 = rt(1000,4)

ar(1000,1,4)$efficiency
ar1(1000,1,4)$efficiency

par(mfrow=c(1,3))
breaks1=seq(-100,100,2)

hist(t,breaks=breaks1,xlim = c(-10,10),main = "ar complete")
hist(t1,breaks=breaks1,xlim = c(-10,10),main = "ar kernel")
hist(t2,breaks=breaks1,xlim = c(-10,10),main = "rt")  

hist(t,freq=F,ylim=c(0,0.4),main = "ar complete")
points(density(t),col="red",ty="l")
points(density(t1),col="blue",ty="l")
points(density(t2),col="purple",ty="l")
