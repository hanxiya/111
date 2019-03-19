rm(list=ls())

############### log likelyhood£º log p(y|theta) ###########
py_log = function(y,x,theta,sigma)          
{
  out = sum(dnorm(y, x*theta, sigma, log = TRUE))
  return(out)
}   

############ log prior£ºlog p(theta) ###############
ptheta_log=function(theta)
{
  out = dnorm(theta,0, 30,log=TRUE)
  return(out)
}

############## log posterior£ºp(theta|y) ##############

p=function(y,x,theta,sigma) py_log(y,x,theta,sigma)+ptheta_log(theta)

####### metropolis hastings #######
mh=function(n,x0,p,mu,sigma,xp,yp,sigma2)
{
  x=rep(NA,n+10000)
  #alpha=rep(NA,n)
  logalpha=rep(NA,n+10000)
  x[1]=x0
  i=1
  while(i<n+10000)
  {
    i=i+1
    xnew=rnorm(1,mu,sigma)
    logalpha[i]=min(0,p(yp,xp,xnew,sigma2)+dnorm(x[i-1],mu,sigma,log=TRUE)-
                      p(yp,xp,x[i-1],sigma2)-dnorm(xnew,mu,sigma,log=TRUE))
    if(runif(1)<exp(logalpha[i])){
      x[i]=xnew
    }else{
      x[i]=x[i-1]}
  }
  alpha=exp(logalpha[10001:n+10000])
  out=list(alpha_all=exp(logalpha),
           alpha_selected=exp(logalpha[10001:n+10000]),
           mean=mean(exp(logalpha[10001:n+10000])),
           x_all=x,
           x_selected=x[10001:n+10000])
  return(out)
}


############### random data ###############
beta=5
x=matrix(runif(100)*10)
epsilon=rnorm(100,0,1)
y=beta*x+epsilon
lm(y~x+0)

################ test ####################
result=mh(100000,0,p,5,0.1,x,y,1)
result$mean
theta_all=result$x_all
theta_selected=result$x_selected

theta_hat=mean(theta_selected)
theta_hat
lm(y~x+0)$coefficients

acf(theta_selected)
plot(theta_all,ty="l",ylim = c(0,10))
abline(v=10000,lty=2,col="red")



