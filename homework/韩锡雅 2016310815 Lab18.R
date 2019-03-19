rm(list=ls())
####### 已知分布的概率密度函数 #######

#install.packages("mvtnorm")
library(mvtnorm)

x=matrix(rnorm(10000,0,5),ncol=2)

p1=function(x) dmvnorm(x,mean=c(1,1),sigma=matrix(c(1,0.3,0.3,1),2))
p2=function(x) dmvt(x,delta = rep(-1, 2),sigma=matrix(c(1,0.7,0.7,1),2),df=3,log=FALSE)
p=function(x) 
{0.4*dmvnorm(x,mean=c(1,1),sigma=matrix(c(1,0.3,0.3,1),2))+
    0.6*dmvt(x,delta = rep(-1, 2),sigma=matrix(c(1,0.7,0.7,1),2),df=3,log=FALSE)}



p_figure=function(x,y) 
{
  0.4*dmvnorm(cbind(x,y),mean=c(1,1),sigma=matrix(c(1,0.3,0.3,1),2))+
    0.6*dmvt(cbind(x,y),delta = rep(-1, 2),
             sigma=matrix(c(1,0.7,0.7,1),2),df=3,log=FALSE)}


####### 已知分布的概率图像 #######

#install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(x[,1],x[,2],p(x))

#install.packages("rgl")
library(rgl)
plot3d(x[,1],x[,2],p(x))

plot3d(x[,1],x[,2],p1(x))
plot3d(x[,1],x[,2],p2(x))

####### metropolis hastings #######
mmht=function(dim,n,x0,p,delta,sigma,df)
{
  x=matrix(rep(NA,dim*(n+10001)),ncol=dim)
  
  alpha=rep(NA,n+10001)
  x[1,]=x0
  
  i=1
  while(i<(n+10000))
  {
    i=i+1
    xnew=rmvt(n=1,sigma,df,delta=delta)
    alpha[i]=min(1,(p(xnew)*dmvt(x[i-1,],delta,sigma=sigma,df,log=FALSE)/p(x[i-1,])/dmvt(xnew,delta,sigma=sigma,df,log=FALSE)))
    
    if(runif(1)<alpha[i]){
      x[i,]=xnew
    }else{
      x[i,]=x[i-1,]}
  }

  out=list(alpha_all=alpha,
           alpha_selected=alpha[10001:(n+10000)],
           mean=mean(alpha[10001:(n+10000)]),
           x_all=x,
           x_selected=x[10001:(n+10000),])
  return(out)
}


####### result #######
x0=matrix(c(1,1),1)
sigma=matrix(c(1,0.8,0.8,1),2)
delta=rep(-1,2)
df=3

t1=Sys.time()
result=mmht(2,10000,x0,p,delta,sigma,df)
t2=Sys.time()
t2-t1


x=result$x_all
xs=result$x_selected
(alpha=result$mean)

acf(xs)

par(mfrow=c(2,1))
plot(x[,2],ty="l")
abline(v=10000,col="red",lty=2)
plot(x[,1],ty="l")
abline(v=10000,col="red",lty=2)

xf = seq(-5, 5,0.1)
yf = seq(-5, 5,0.1)
zf=outer(xf,yf,p_figure)

image(xf,yf,zf)
contour(xf,yf,zf, add = TRUE,col="blue")
points(x,pch=".",cex=2)
ncp=-1
df=3
####### metropolis hastings within gibbs #######
mmhg=function(dim,n,x0,p,ncp,df)
{
  x=matrix(rep(NA,dim*(n+10001)),ncol=dim)
  
  alpha=matrix(rep(NA,dim*(n+10001)),ncol=dim)
  x[1,]=x0
  
  for(i in 1:(n+10000))
  {
    x[i+1,]=x[i,]
    
    for(j in 1:dim)
    {
      xtemp=x[i+1,]
      xtemp[j]=rt(n=1,df,ncp)
      alpha[i+1,j]=min(1,p(xtemp)*
                         dt(x[i+1,j],df,ncp,log=FALSE)/
                         p(x[i+1,])/
                         dt(xtemp[j],df,ncp,log=FALSE))
      
      if(runif(1)<alpha[i+1,j]){
        x[i+1,]=xtemp
      }
    }
  }
  out=list(alpha_all=alpha,
           alpha_selected=alpha[10001:(n+10000),],
           mean=mean(alpha[10001:(n+10000),]),
           x_all=x,
           x_selected=x[10001:(n+10000),])
  return(out)
}

####### result1 #######
x0=matrix(c(1,1),1)
ncp=-0.7
df=3

t3=Sys.time()
result1=mmhg(2,100000,x0,p,ncp,df)
t4=Sys.time()
t4-t3

x1=result1$x_all
xs1=result1$x_selected
(alpha1=result1$mean)
acf(xs1)
par(mfrow=c(2,1))
plot(x1[,2],ty="l")
abline(v=10000,col="red",lty=2)
plot(x1[,1],ty="l")
abline(v=10000,col="red",lty=2)

xf = seq(-5, 5,0.1)
yf = seq(-5, 5,0.1)
zf=outer(xf,yf,p_figure)

image(xf,yf,zf)
contour(xf,yf,zf, add = TRUE,col="blue")
points(x1,pch=".",cex=3)

