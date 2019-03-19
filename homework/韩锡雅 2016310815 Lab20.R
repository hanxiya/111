rm(list=ls())
library(mvtnorm)

############### sigma ÒÑÖª ###############

############### log likelyhood£º log p(y|beta) ###########
alpha = function(x,b) x%*%matrix(b)
P = function(x,b) 1/(1+exp(-alpha(x,b)))
py_log = function(x,y,b) sum(y*log(P(x,b)))+sum((1-y)*log(1-P(x,b)))

############ log prior£ºlog p(beta) ###############
pbeta_log=function(b)
{
  
  n=nrow(matrix(b))
  out = dmvnorm(t(matrix(b)),rep(0,n), diag(n),log=TRUE)
  return(out)
}

############## log posterior£ºp(theta|y) ##############

posterior_log=function(x,y,b) py_log(x,y,b)+pbeta_log(b)


####### metropolis hastings within gibbs #######
mhwg=function(n,p,mu,sigma,xp,yp)
{
  dim=ncol(xp)
  x=matrix(rep(NA,dim*(n+10001)),ncol=dim)
  
  logalpha=matrix(rep(NA,dim*(n+10001)),ncol=dim)
  b0=rep(0,dim)
  x[1,]=t(matrix(b0))
  
  for(i in 1:(n+10000))
  {
    x[i+1,]=x[i,]
    
    for(j in 1:dim)
    {
      xtemp=x[i+1,]
      xtemp[j]=rnorm(1,mu[j],sigma)
      logalpha[i+1,j]=min(0,p(xp,yp,xtemp)+
                         dnorm(x[i+1,j],mu[j],sigma,log=T)-
                         p(xp,yp,x[i+1,])-
                         dnorm(xtemp[j],mu[j],sigma,log=T))
      
      if(runif(1)<exp(logalpha[i+1,j])){
        x[i+1,]=xtemp
      }
    }
  }
  out=list(alpha_all=exp(logalpha),
           alpha_selected=exp(logalpha)[10001:(n+10000),],
           mean=mean(exp(logalpha)[10001:(n+10000),]),
           x_all=x,
           x_selected=x[10001:(n+10000),])
  return(out)
}



x1 = as.matrix(rep(1,32))
x2 = mtcars$am
x3 = mtcars$mpg
x = cbind(x1,x2,x3)
y = as.matrix(mtcars$vs)

x
y

mu=rep(0,3)
result=mhwg(10000,posterior_log,mu,0.1,x,y)
apply(result$x_selected,2,mean)


################## sigma Î´Öª ####################
rm(list=ls())
library(mvtnorm)


############### log likelyhood£º log p(y|beta) ###########
alpha = function(x,b) x%*%matrix(b)
P = function(x,b) 1/(1+exp(-alpha(x,b)))
py_log = function(x,y,b) sum(y*log(P(x,b)))+sum((1-y)*log(1-P(x,b)))

############ log prior£ºlog p(beta) ###############
pbeta_log=function(b)
{
  
  n=nrow(matrix(b))
  out = dmvnorm(t(matrix(b)),rep(0,n), diag(n),log=TRUE)
  return(out)
}

############## log p(sigma^2) ##############
psigma_log = function(sigma2,df,ncp)
{
  dchisq(1/(sigma2),df,ncp,log=TRUE)
}

############## log posterior£ºp(beta,sigma^2|y) ##############

posterior_log=function(x,y,b,sigma2,df,ncp) py_log(x,y,b)+pbeta_log(b)+psigma_log(sigma2,df,ncp)


####### metropolis hastings within gibbs #######
mhwg1=function(n,p,mu,sigma,df,ncp,df1,ncp1,xp,yp)
{
  dim=ncol(xp)
  x=matrix(rep(NA,dim*(n+10001)),ncol=dim)
  y=matrix(rep(NA,(n+10001)))
  
  logalpha=matrix(rep(NA,(dim+1)*(n+10001)),ncol=dim+1)
  b0=rep(0,dim)
  x[1,]=t(matrix(b0))
  y[1]=1
  for(i in 1:(n+10000))
  {
    x[i+1,]=x[i,]
    y[i+1]=y[i]
    for(j in 1:dim)
    {
      xtemp=x[i+1,]
      xtemp[j]=rnorm(1,mu[j],sigma)
      logalpha[i+1,j]=min(0,p(xp,yp,xtemp,y[i],df,ncp)+
                            dnorm(x[i+1,j],mu[j],sigma,log=T)-
                            p(xp,yp,x[i+1,],y[i],df,ncp)-
                            dnorm(xtemp[j],mu[j],sigma,log=T))
      if(logalpha[i+1,j]=="NaN"|is.na(logalpha[i+1,j]))
      {
        logalpha[i+1,j]=log(1E-10)
      }
      if(runif(1)<exp(logalpha[i+1,j])){
        x[i+1,]=xtemp
      }
    }
      ytemp=rchisq(1,df1,ncp1)
      logalpha[i,dim+1]=min(0,p(xp,yp,x[i+1,],ytemp,df,ncp)+
                              dchisq(y[i],df1,ncp1,log=T)-
                              p(xp,yp,x[i+1,],y[i],df,ncp)-
                              dchisq(ytemp,df1,ncp1,log=T))
      if(logalpha[i,dim+1]=="NaN"|is.na(logalpha[i,dim+1]))
      {
        logalpha[i,dim+1]=log(1E-10)
      }
      if(runif(1)<exp(logalpha[i,dim+1])){
        y[i+1,]=ytemp
      }

  }
  out=list(alpha_all=exp(logalpha),
           alpha_selected=exp(logalpha)[10001:(n+10000),],
           mean=mean(exp(logalpha)[10001:(n+10000),]),
           x_all=x,
           x_selected=x[10001:(n+10000),],
           y_all=y,
           y_selected=y[10001:(n+10000)])
  return(out)
}

x1 = as.matrix(rep(1,32))
x2 = mtcars$am
x3 = mtcars$mpg
x = cbind(x1,x2,x3)
y = as.matrix(mtcars$vs)
mu=rep(0,3)
mu=c(-12,-3,1)
result1=mhwg1(10000,posterior_log,mu,10,10,0,15,5,x,y)
apply(result1$x_selected,2,mean)

glm(y~x[,2:3],family="binomial")

xp=x
yp=y

