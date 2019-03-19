rm(list=ls())

######## metropolis algorithm #######
n=100000
mtp=function(n,x0,p,sigma)
{
  x=rep(NA,n+10000)
  #alpha=rep(NA,n)
  logalpha=rep(NA,n+10000)
  x[1]=x0
  i=1
  while(i<n+10000)
  {
    i=i+1
    xnew=rnorm(1,x[i-1],sigma)
    #alpha[i]=min(1,p(xnew)/p(x[i-1]))
    logalpha[i]=min(0,log(p(xnew))-log(p(x[i-1])))
    if(runif(1)<exp(logalpha[i])){
      x[i]=xnew
    }else{
      x[i]=x[i-1]}
  }
  alpha=exp(logalpha)[10001:n+10000]
  out=list(alpha=alpha,mean=mean(alpha),x=x[10001:n+10000])
  return(out)
}

####### df=5 的t分布 #######
p1 = function(x) (1+0.2*x^2)^(-3)
set.seed(1)
alpha1=mtp(100000,1,p1,10)$alpha
mtp(100000,1,p1,1)$mean

x1=mtp(110000,1,p1,0.05)$x
x2=mtp(110000,1,p1,0.5)$x
x3=mtp(110000,1,p1,1)$x
x4=mtp(110000,1,p1,5)$x
x5=mtp(110000,1,p1,10)$x
x6=mtp(110000,1,p1,15)$x
plot(x1,ty="l")

summary(x1)

hist(x1,freq=F,ylim = c(0,0.5))
points(density(x1),ty="l",col="red")

par(mfrow=c(2,3))
acf(x1)
acf(x2)
acf(x3)
acf(x4)
acf(x5)
acf(x6)





a=numeric(10)
for(j in 1:10)
  a[j]=mtp(100000,1,p1,j)$mean



####### df=3 的t分布 #######
p2 = function(x) (1+(x^2)/3)^(-2)
set.seed(1)
alpha2=mtp(110000,1,p2)$alpha[10001:110000]
mean(alpha2)

x2=mtp(110000,1,p)$x[10001:110000]

plot(x2)
summary(x2)

hist(x2,freq=F,ylim = c(0,0.5))
points(density(x2),ty="l",col="red")

acf(x2)

####### df=7 的t分布 #######
p3 = function(x) (1+(x^2)/7)^(-4)
set.seed(1)
alpha3=mtp(110000,1,p3)$alpha[10001:110000]
mean(alpha3)

x3=mtp(110000,1,p3)$x[10001:110000]

plot(x3)
summary(x3)

hist(x3,freq=F,ylim = c(0,0.5))
points(density(x3),ty="l",col="red")

acf(x3)

