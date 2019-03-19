
par(mfrow = c(2,2))

#1.正态分布
x = seq(-5,5,0.1)
fx = dnorm(x)  
cbind(x,fx)
plot(x, fx, type = "l", col = 1) 

x = seq(-5,5,0.1)      
Fx = pnorm(x)              
plot(x,Fx,                 
     type = "l",           
     col = 2,    
     lwd = 3)             

hist(Fx,col = 8)

y = runif(1000)
x = qnorm(y)
plot(x,y,col = 4)

#2.指数分布            
x = seq(0,5,0.1)
fx = dexp(x)  
cbind(x,fx)
plot(x, fx, type = "l", col = 1)                

x = seq(0,5,0.1)      
Fx = pnorm(x)              
plot(x,Fx,                 
     type = "l",           
     col = 2,    
     lwd = 3)               

hist(Fx,col = 8)

y = runif(1000)
x = qexp(y)
plot(x,y,col = 4)

#3.伽马分布            
x = seq(0,5,0.1)
fx = dgamma(x,shape = 2)  
cbind(x,fx)
plot(x, fx, type = "l", col = 1)                 

x = seq(0,5,0.1)      
Fx = pgamma(x,shape = 2)              
plot(x,Fx,                 
     type = "l",           
     col = 2,    
     lwd = 3)               

hist(Fx,col = 8)

y = runif(1000)
x = qgamma(y,shape = 2)
plot(x,y,col = 4)

#4.泊松分布           
x = seq(0,20,1)
fx = dpois(x,lambda = 10)  
cbind(x,fx)
plot(x, fx, type = "l", col = 1)                 

x = seq(0,20,1)      
Fx = ppois(x,lambda = 10)              
plot(x,Fx,                 
     type = "l",           
     col = 2,    
     lwd = 3)               

hist(Fx,col = 8)

y = runif(1000)
x = qpois(y,lambda = 10)
plot(x,y,col = 4)

#5.贝塔分布          
x = seq(0,1,0.01)
fx = dbeta(x,2,5)  
cbind(x,fx)
plot(x, fx, type = "l", col = 1)                 

x = seq(0,1,0.01)    
Fx = pbeta(x,2,5)              
plot(x,Fx,                 
     type = "l",           
     col = 2,    
     lwd = 3)               

hist(Fx,col = 8)

y = runif(1000)
x = qbeta(y,2,5)
plot(x,y,col = 4)

#6.二项分布        
x = seq(40,100,1)
fx = dbinom(x,100,0.7)  
cbind(x,fx)
plot(x, fx, type = "l", col = 1)                 

x = seq(40,100,1)    
Fx = pbinom(x,100,0.7)              
plot(x,Fx,                 
     type = "l",           
     col = 2,    
     lwd = 3)               

hist(Fx,col = 8)

y = runif(100)
x = qbinom(y,100,0.7) 
plot(x,y,col = 4)

#7.几何分布         
x = seq(0,20,1)
fx = dgeom(x,0.5)  
cbind(x,fx)
plot(x, fx, type = "l", col = 1)                 

x = seq(0,20,1)   
Fx = pgeom(x,0.5)             
plot(x,Fx,                 
     type = "l",           
     col = 2,    
     lwd = 3)               

hist(Fx,col = 8)

y = runif(100)
x = qgeom(y,0.5)
plot(x,y,col = 4)

#8.负二项分布        
x = seq(10,70,1)
fx = dnbinom(x,100,0.7)  
cbind(x,fx)
plot(x, fx, type = "l", col = 1)                 

x = seq(10,70,1)   
Fx = pnbinom(x,100,0.7)              
plot(x,Fx,                 
     type = "l",           
     col = 2,    
     lwd = 3)               

hist(Fx,col = 8)

y = runif(100)
x = qnbinom(y,100,0.7) 
plot(x,y,col = 4)

#9.均匀分布    
x = seq(0,10,0.01)
fx = dunif(x,0,10)  
cbind(x,fx)
plot(x, fx, type = "l", col = 1)                 

x = seq(0,10,0.01) 
Fx = punif(x,0,10)              
plot(x,Fx,                 
     type = "l",           
     col = 2,    
     lwd = 3)               

hist(Fx,col = 8)

y = runif(100)
x = qunif(y,0,10)  
plot(x,y,col = 4)

#10.卡方分布      
x = seq(0,30,0.01)
fx = dchisq(x,10)  
cbind(x,fx)
plot(x, fx, type = "l", col = 1)                 

x = seq(0,30,0.01)
Fx = pchisq(x,10)              
plot(x,Fx,                 
     type = "l",           
     col = 2,    
     lwd = 3)               

hist(Fx,col = 8)

y = runif(100)
x = qchisq(y,10)  
plot(x,y,col = 4)

#11.柯西分布      
x = seq(-5,5,0.1)
fx = dcauchy(x,0,1)  
cbind(x,fx)
plot(x, fx, type = "l", col = 1)                 

x = seq(-5,5,0.1)
Fx = pcauchy(x,0,1)             
plot(x,Fx,                 
     type = "l",           
     col = 2,    
     lwd = 3)               

hist(Fx,col = 8)

y = runif(100)
x = qcauchy(y,0,1)
plot(x,y,col = 4)

#12.威布尔分布     
x = seq(0,10,0.1)
fx = dweibull(x,1,1)  
cbind(x,fx)
plot(x, fx, type = "l", col = 1)                 

x = seq(0,10,0.1)
Fx = pweibull(x,1,1)            
plot(x,Fx,                 
     type = "l",           
     col = 2,    
     lwd = 3)               

hist(Fx,col = 8)

y = runif(100)
x = qweibull(y,1,1)
plot(x,y,col = 4)

#13.t分布    
x = seq(-5,5,0.1)
fx = dt(x,2)  
cbind(x,fx)
plot(x, fx, type = "l", col = 1)                 

x = seq(-5,5,0.1)
Fx = pt(x,2)             
plot(x,Fx,                 
     type = "l",           
     col = 2,    
     lwd = 3)               

hist(Fx,col = 8)

y = runif(100)
x = qt(y,2)  
plot(x,y,col = 4)

#14.F分布   
x = seq(0,5,0.1)
fx = df(x,5,10)  
cbind(x,fx)
plot(x, fx, type = "l", col = 1)                 

x = seq(0,5,0.1)
Fx = pf(x,5,10)              
plot(x,Fx,                 
     type = "l",           
     col = 2,    
     lwd = 3)               

hist(Fx,col = 8)

y = runif(100)
x = qf(y,5,10)  
plot(x,y,col = 4)

#15.超几何分布
N = 1000
M = 50
k = 100
x = seq(0,15,1)
fx = dhyper(x,M,N,k)  
cbind(x,fx)
plot(x, fx, type = "l",col = 1)                 


Fx = phyper(x,M,N,k)               
plot(x,Fx,                 
     type = "l",           
     col = 2,    
     lwd = 3)               

hist(Fx,col = 8)

y = runif(100)
x = qhyper(y,M,N,k)    
plot(x,y,col = 4)

#16.logistic分布
x = seq(-5,5,0.1)
fx = dlogis(x,0,1)  
cbind(x,fx)
plot(x, fx, type = "l",col = 1)                 


Fx = plogis(x,0,1)               
plot(x,Fx,                 
     type = "l",           
     col = 2,    
     lwd = 3)               

hist(Fx,col = 8)

y = runif(100)
x = qlogis(y,0,1)  
plot(x,y,col = 4)

#17.对数正态分布
x = seq(0,5,0.1)
fx = dlnorm(x,0,1)  
cbind(x,fx)
plot(x, fx, type = "l",col = 1)                 


Fx = plnorm(x,0,1)               
plot(x,Fx,                 
     type = "l",           
     col = 2,    
     lwd = 3)               

hist(Fx,col = 8)

y = runif(100)
x = qlnorm(y,0,1)  
plot(x,y,col = 4)

#18.wilcoxon分布
x = seq(-1,10,1)
fx = dwilcox(x,4,6)  
cbind(x,fx)
plot(x, fx,col = 1)                 

Fx = pwilcox(x,4,6)               
plot(x,Fx,                 
     type = "l",           
     col = 2,    
     lwd = 3)               

hist(Fx,col = 8)

y = runif(100)
x = qwilcox(y,4,6)   
plot(x,y,col = 4)

#19.Signed Rank分布
x = seq(0,10,1)
fx = dsignrank(x,100) 
cbind(x,fx)
plot(x, fx,col = 1)                 

Fx = psignrank(x,100)              
plot(x,Fx,                 
     type = "l",           
     col = 2,    
     lwd = 3)               

hist(Fx,col = 8)

y = runif(100)
x = qsignrank(y,100)  
plot(x,y,col = 4)

###############20.birthday分布
x = 10:500
Fx = NULL
for(i in 1:length(x))
{
  Fx[i] = pbirthday(x[i],coincident = 5)
}
plot(x,Fx,type = "l")

hist(Fx,col = 8)

y = runif(100)
x = NULL
for(i in 1:length(y))
{
  x[i] = qbirthday(y[i],coincident = 5)
}

plot(x,y,col = 4)


