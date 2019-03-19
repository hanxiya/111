rm(list=ls())

####### metropolis hastings #######
mh=function(n,x0,p,mu,sigma)
{
  x=rep(NA,n+10000)
  #alpha=rep(NA,n)
  logalpha=rep(NA,n+10000)
  x[1]=x0
  i=1
  while(i<n+10000)
  {
    i=i+1
    xnew=rnorm(1,0,sigma)
    logalpha[i]=min(0,log(p(xnew))+log(dnorm(x[i-1],mu,sigma))-
                      log(p(x[i-1]))-log(dnorm(xnew,mu,sigma)))
    if(runif(1)<exp(logalpha[i])){
      x[i]=xnew
    }else{
      x[i]=x[i-1]}
  }
  alpha=exp(logalpha)[10001:n+10000]
  out=list(alpha=alpha,mean=mean(alpha),x=x[10001:n+10000])
  return(out)
}

####### 产生服从已知分布的随机数 #######
f = function(x) 0.4*dnorm(x,3,1)+0.3*dnorm(x,5,3)+0.3*dnorm(x,0,2)
alpha_mean = mh(100000,1,f,0,5)$mean
alpha = mh(100000,1,f,0,5)$alpha
x=mh(110000,1,f,0,5)$x
acf(x)


####### 有效率 #######
#install.packages("coda")
library(coda)
e = effectiveSize(x)/length(x)

####### 均值 方差 峰度系数 偏度系数  #######
mean(x)
var(x)

#install.packages("moments")
library(moments)

skewness(x)
kurtosis(x)

#install.packages("fBasics")
#library(fBasics)
#skewness(x) 
#kurtosis(x)     

data.frame(Ex = mean(x),Varx = var(x),sk = skewness(x),ku = kurtosis(x))

####### 直方图 #######

c = curve(f,from=-10,to=10,ty="l") #已知分布的概率密度

hist(x,freq=F,ylim = c(0,0.25))
lines(c,col="blue")
lines(density(x),col="red")
legend("topright",c("f(x)","mt"),lty=c(1,1),col=c("blue","red"),ncol=1)





