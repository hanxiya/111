lm(y~x[,2:5])
b = rep(1,5)

rm(list=ls())	

### DGP ###
bS = matrix(seq(0.1,0.5,by = 0.1))    #设定一组beta为0.1,0.2,0.3,0.4,0.5
bS
x = matrix(c(rep(1,10),runif(40)),10) #随机生成x阵
x
sigma = 0.01
epsilon = matrix(rnorm(10,0,sigma))
epsilon
y = x%*%bS+epsilon                    #根据beta，x，epsilon生成观测值y
y
### OLS下的牛顿迭代法解beta ###
yhat = function(x,b)                  
{
  x%*%b
}                              

func = function(x,y,b)          #y的对数似然函数
{
  sum((y-yhat(x,b))^2)
}

grad = function(x,y,b)
{
  out=t((-(y[1]-yhat(x,b)[1]))%*%(-x[1,]))
  for(i in 2:n)
  {
    out = out + t((-(y[i]-yhat(x,b)[i]))%*%(-x[i,]))
  }
  return(out*2)
}

hess = function(x,y,b,p,n)
{
  h = matrix(rep(NA,(p-1)^2),p-1)
  for(i in 1:p-1)
  {
    for(j in 1:p-1)
    {
      h[i,j]=2*sum(x[,i+1]*x[,j+1])
    }
  }
  
  r1 = 2*sum(x[,2])
  for(i in 3:p-1)
  {
    r1 = cbind(r1,2*sum(x[,i+1]))
  }
  he = matrix(c(r1,h),p,byrow = TRUE)
  
  c1 = 2*n
  for(i in 2:p-1)
  {
    c1 = rbind(c1,2*sum(x[,i+1]))
  }
  out = cbind(c1,he)
  return(out)
}

nralgo = function(x,y,b0,p,n){
  epsilon = 1E-10
  MaxIter = 500 
  q = 1
  bnew = b0-solve(hess(x,y,b0,p,n),grad(x,y,b0)) 
  change = abs(func(x,y,bnew)-func(x,y,b0))
  while( (q <= MaxIter) && (change > epsilon) )
  { 
    q = q+1 
    bold = bnew 
    bnew = bold-solve(hess(x,y,b0,p,n),grad(x,y,bold)) 
    change = abs(func(x,y,bnew)-func(x,y,bold)) 
  }
  return(bnew)
}

b0 = rep(0,5)
p = 5
n = 10
x
y
sigma
nralgo(x,y,b0,p,n)



b = round(nralgo(x,y,b0,p,n),2)
data.frame(bS,b)



