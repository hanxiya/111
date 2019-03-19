step = c(6537,6363,6177,6018,5714,
         5324,4870,4836,4645,4584)            #运动步数

loglikelyhood = function(x,mu,sigma)          #正态分布 对数似然函数
{
  out = sum(dnorm(x, mu, sigma, log = TRUE))
  return(out)
}   
dloglikelyhood = function(x,mu,sigma)         #对数似然函数一阶导函数
{
  out = -sum((mu-x)/sigma^2)
  return(out)
}   
d2loglikelyhood = function(x,sigma)           #对数似然函数二阶导函数
{
  out = -length(x)/sigma^2
  return(out)
}   

muS = 2000:9000                               #均值取多个值,并固定标准差1000
LS = NULL
for(i in 1:length(muS))
{
  LS[i] = loglikelyhood(step, muS[i], 1000)
}
cbind(muS,LS)                                 #每个均值对应的似然
plot(muS,LS,type = "l" ,xlim=c(5000,6000),ylim=c(-83,-80))
abline(v = 5400,lty=2,col=2)
abline(v = 5600,lty=2,col=2)
#由图，mu极值点大约在5400~5600

#在mu为5400~5600间绘一阶导函数图
muS1 = 5400:5600 
dLS = NULL
for(i in 1:length(muS1))
{
  dLS[i] = dloglikelyhood(step, muS1[i], 1000)
}
cbind(muS1,dLS)                                 #每个均值对应的似然一阶导
plot(muS1,dLS,type = "l" ,xlim=c(5400,5600),ylim=c(-0.001,0.001))
abline(h = 0,lty=2,col=2)
abline(v = 5500,lty=2,col=2)
abline(v = 5550,lty=2,col=2)
#进一步由图，mu极值点大约在5500~5550

#寻找对数似然函数极大值点
mu = rep(NA,20)
mu0 = 20

epsilon<-1E-10
mu = mu0
while(abs(dloglikelyhood(step,mu,1))>epsilon)
{
  mu = mu-dloglikelyhood(step,mu,1)/d2loglikelyhood(step,1)  
  print(mu)
}
round(dloglikelyhood(step,mu,1000),3)

plot(muS,LS,type = "l" )
abline(v=mu,lty=2,col=2)
abline(h=loglikelyhood(step,mu,1000),lty=2,col=2)

