step = c(6537,6363,6177,6018,5714,
         5324,4870,4836,4645,4584)            #运动步数

loglikelyhood = function(x,mu,sigma)          #正态分布的对数似然函数
{
  out = sum(dnorm(x, mu, sigma, log = TRUE))
  return(out)
}   

len0 = 10                                     #给定步长初始值
mu1 = 1000
mu2 = 7000
sigma1 = 100
sigma2 = 1000                                 #给定mu、sigma初始范围
mubest = NULL
sigmabest = NULL
for(t in 1:50)                                #设定循环次数
{ 
  len = len0*0.5^(t+1)                        #每次步长缩短二分之一
  muS = seq(mu1-len,mu2+len,len)              #mu的范围每次缩小为上一mu最佳值加减步长
  LS = NULL
  sigmabest[t] = (sigma1+sigma2)/2            #给定第一个sigma值为sigma范围的中值
  for(i in 1:length(muS))
  {
    LS[i] = loglikelyhood(step, muS[i], sigmabest[t])
  }
    mubest[t] = mu1 = mu2 = muS[which.max(LS)]#得出mu最佳值
    
  sigmaS = seq(sigma1-len,sigma2+len,len)     #sigma的范围每次缩小为上一sigma最佳值加减步长               
  LS1 = NULL
  
  for(j in 1:length(sigmaS))
    {
       LS1[j] = loglikelyhood(step,mubest[t],sigmaS[j])
    }
  
  sigmabest[t+1] = sigma1 = sigma2 = sigmaS[which.max(LS1)]
                                               #得出sigma最佳值
}
  
mubest[t]  
sigmabest[t+1]

