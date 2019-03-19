rm(list=ls())

alpha = function(x,b) x%*%b
P = function(x,b) 1/(1+exp(-alpha(x,b)))

func = function(x,y,b) sum(y*log(P(x,b)))+sum((1-y)*log(1-P(x,b)))

grad = function(x,y,b)
{
  p=ncol(x)
  g=matrix(numeric(p))
  for(i in 1:p)
    g[i] = sum((y-P(x,b))*x[,i])
  return(g)
}
hess = function(x,b)
{
  p=ncol(x)
  n=nrow(x)
  h = matrix(numeric(p^2),p)
  
  for(m in 1:n){
    h0 = x[m,] %*% t(x[m,]) *(-exp(-alpha(x,b)[m]))/(1+exp(-alpha(x,b)[m]))^2
    h = h + h0
  }
  return(h)
}

######### hess 牛顿迭代 ##########
nralgo = function(x,y,b0){
  t1=Sys.time()
  epsilon = 1E-10
  MaxIter = 1000
  g=NULL
  n = 1
  bnew = b0-solve(hess(x,b0),grad(x,y,b0))
  g[1]=func(x,y,bnew)
  change = abs(func(x,y,bnew)-func(x,y,b0))
  while( (n <= MaxIter) && (change > epsilon) )
  { 
    n = n+1 
    bold = bnew 
    bnew = bold-solve(hess(x,bold),grad(x,y,bold))
    g[n]=func(x,y,bnew)
    change = abs(func(x,y,bnew)-func(x,y,bold))
  }
  t2=Sys.time()
  t=t2-t1
  plot(1:n,g,type = "l",ylab = "func",xlab = "times")
  out=list(b=bnew,iter=n,time=t)
  return(out)
}

################ 拟牛顿 BFGS ###############
p=ncol(x)
B=array(rep(0,p*p*100),c(p,p,100))
BB=array(rep(0,p*p*100),c(p,p,100))
I=diag(1,ncol(x))
B0=I
B[,,1]=B0
b=matrix(rep(0,p*100),p)
b[,1]=rep(0,p)
q=matrix(rep(0,p*100),p)
r=matrix(rep(0,p*100),p)
s=matrix(rep(0,p*100),p)
m=matrix(rep(0,100))
for(k in 1:100)
{
q[,k]=-solve(B[,,k])%*%grad(x,y,b[,k])

a0=10
c1=0.1 ###控制参数，人工核定
c2=0.9
gamma=0.5 ####缩减参数

m[k]=t(q[,k])%*%grad(x,y,b[,k])
bb=matrix(rep(0,p*10000),p)
bb[,1]=b[,1]
j=1
a=matrix(rep(NA,100))
a[1]=a0
while(func(x,y,bb[,j])>(func(x,y,b[,k])+a[j]*c1*m[k])
     &t(grad(x,y,bb[,j]))%*%q[,k]< c2*t(grad(x,y,b[,k]))%*%q[,k]  )
{
  j=j+1
  a[j]=gamma*a[j-1]
  bb[,j]=b[,k]+a[j]*q[,k]
}

b[,k+1]=bb[,j]

r[,k]=grad(x,y,b[,k+1])-grad(x,y,b[,k])
s[,k]=b[,k+1]-b[,k]
B[,,k+1]=B[,,k]+(r[,k]-B[,,k]%*%s[,k])%*%t(r[,k]-B[,,k]%*%s[,k])/ 
  as.vector((t(r[,k]-B[,,k]%*%s[,k])%*%s[,k]))
}
 
############# 代入数据 #############

y = as.matrix(mtcars$vs)
y
x0 = rep(1,32)
x1 = mtcars$mpg
x2 = mtcars$am
x = cbind(x0,x1,x2)
x
b0=c(0,0,0)
nralgo(x,y,b0)



