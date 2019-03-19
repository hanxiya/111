rm(list=ls())

############ nm方法寻找极值 #################

#参数f为负的logit对数似然函数，xf为f中的自变量，yf为因变量
#参数b0为初始点矩阵
#参数n为维数
#stoprule：参数maxiter为最大迭代次数，epsilon为面积下界


nmlogit = function(f,b0,xf,yf,n,maxiter,epsilon)
{
  t1=Sys.time() #记录初始时间
  
  alpha<-1 
  gamma<-2 
  rho<-0.5 
  sigma<-0.5 
  
  simplex = b0 
  iter = 0
  
  y = rep(NA,n+1)
  for(i in 1:(n+1)){y[i]<-f(xf,yf,simplex[,i])}
  
  s = 0.5*abs(det(rbind(simplex,rep(1,n+1))))
  
  ynew=min(y)
  
  while ((s>epsilon)&(iter<maxiter)){
    iter = iter+1
    sortf = sort(y,decreasing = FALSE,index.return=TRUE) 
    simplex = simplex[,sortf$ix]     #将simplex按照函数值大小重新排序
    y = sortf$x
    x0 = apply(simplex[,1:n],1,mean) #找中点
    xr = x0+alpha*(x0-simplex[,(n+1)])
    yr = f(xf,yf,xr)
    
    ###Case 1###
    
    if((y[1]<=yr)&&(yr<y[n])){
      simplex[,n+1] = xr 
      y[n+1] = yr 
    }
    
    ###Case 2###
    
    if(yr<y[1]){
      
      xe = x0+gamma*(xr-x0)  
      ye = f(xf,yf,xe)
      if(ye<yr){
        simplex[,n+1] = xe
        y[n+1] = ye 
      }
      else {
        simplex[,n+1] = xr 
        y[n+1] = yr 
      }
    }
    
    ###Case 3###
    
    if(yr>=y[n]){
      
      xc = x0+rho*(simplex[,n+1]-x0)  
      yc = f(xf,yf,xc)
      if(yc<y[n+1]){
        simplex[,n+1] = xc 
        y[n+1] = yc 
      }
      else {
        
        for (j in 2:(n+1)){
          simplex[,j] = simplex[,1]+sigma*(simplex[,j]-simplex[,1])
          y[j]<-f(xf,yf,simplex[,j])
        }
      }

    }      
    
    s = 0.5*abs(det(rbind(simplex,rep(1,n+1))))
  }
  t2=Sys.time() #记录结束时间
  t=t2-t1 #时间差
  out = list(xbest = simplex[,which.min(y)],iter = iter,time=t) 
  return(out)
}

########
epsilon = 1E-10 #面积下界
maxiter = 1000  #最大次数


########### 牛顿迭代法 ############

alpha1 = function(x,b) x%*%b
P = function(x,b) 1/(1+exp(-alpha1(x,b)))

func = function(x,y,b) sum(y*log(P(x,b)))+sum((1-y)*log(1-P(x,b))) #logit对数似然函数

grad = function(x,y,b) #一阶导矩阵
{
  p=ncol(x)
  a=matrix(numeric(p))
  for(i in 1:p)
    a[i] = sum((y-P(x,b))*x[,i])
  return(a)
}

hess = function(x,b)   #二阶导矩阵
{
  p=ncol(x)
  n=nrow(x)
  h = matrix(numeric(p^2),p)
  
  for(m in 1:n){
    h0 = x[m,] %*% t(x[m,]) *(-exp(-alpha1(x,b)[m]))/(1+exp(-alpha1(x,b)[m]))^2
    h = h + h0
  }
  return(h)
}


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


######## 负的logit对数似然函数 ########

f = function(x,y,b) 
{
  alpha=x%*%b
  P=1/(1+exp(-alpha))
  -(sum(y*log(P))+sum((1-y)*log(1-P)))
}

######### 数据1 对数似然函数的参数xf1,yf1 ######### 

yf1 = mtcars$vs
yf1
x0 = rep(1,32)
x1 = mtcars$mpg
x2 = mtcars$am
xf1 = cbind(x0,x1,x2)
xf1

######### 代入数据1 nm方法 以glm验证 #########
set.seed(2)
b0 = matrix(rnorm(12,0,1),3)     #随机取初始点
nmlogit(f,b0,xf1,yf1,3,1000,1E-10)
bnm1=nmlogit(f,b0,xf1,yf1,3,1000,1E-10)$xbest #nm结果
bglm1 = glm(yf1 ~ xf1[,2:3], family = binomial("logit"))$coefficients #glm结果
data.frame(bnm=bnm1,bglm=bglm1) #结果对比

####### 代入数据1 牛顿迭代法 #######

bf1=c(0,0,0)
nralgo(xf1,yf1,bf1)
bnt1=nralgo(xf1,yf1,bf1)$b    
tnt1=nralgo(xf1,yf1,bf1)$time
tnm1=nmlogit(f,b0,xf1,yf1,3,1000,1E-10)$time

######## 数据1  nm&牛顿迭代的结果、速度、时间比较 ####### 
iternm1=nmlogit(f,b0,xf1,yf1,3,1000,1E-10)$iter
iternt1=nralgo(xf1,yf1,bf1)$iter

data.frame(bnm=bnm1,bnt=bnt1,bglm=bglm1)  #beta结果
data.frame(iternm=iternm1,iternt=iternt1)#迭代次数
data.frame(tnm=tnm1,tnt=tnt1)            #用时

#结论：达到同样精度，牛顿迭代比nm 收敛速度快，用时短，结果准确（以glm为标准）



