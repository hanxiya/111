setwd("E:/R DATA")
rm(list=ls())

alpha = function(x,b) x%*%b
P = function(x,b) 1/(1+exp(-alpha(x,b)))

func = function(x,y,b) sum(y*log(P(x,b)))+sum((1-y)*log(1-P(x,b)))

grad = function(x,y,b)
{
  p=ncol(x)
  a=matrix(numeric(p))
  for(i in 1:p)
  a[i] = sum((y-P(x,b))*x[,i])
  return(a)
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
#x%*%((P(x,b0)-P(x,b0)*P(x,b0))*x)

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

#data=read.table("admit.txt",header = T)
 

ADMIT	= c(0,1,1,1,0,1,1,0,1,0,0,0,1,0,1) #研究生是否被录取
GRE	= c(380,660,800,640,520,760,	560,400,540,700,800,440,760,700,700) #GRE成绩
GPA	= c(3.61,3.67,4,3.19,2.93,3,2.98,3.08,3.39,3.92,4,3.22,4,3.08,4) #绩点
RANK = c(3,3,1,4,4,2,1,2,3,2,4,1,1,2,1) #毕业学校排名
data = data.frame(ADMIT,GRE,GPA,RANK)
data

y = as.matrix(data$ADMIT)
y
x0=matrix(rep(1,nrow(data)))
x = cbind(x0,as.matrix(data[,2:4]))
x
b0 = matrix(rep(0,4))
nralgo(x,y,b0)
b1 = nralgo(x,y,b0)$b

b1
### glm函数 ###
glm_b = glm(ADMIT ~ GRE+GPA+RANK, family = binomial("logit"),data = data)
summary(glm_b)
b2 = glm_b$coefficients
data.frame(b1,b2)






############################## mtcars

y = as.matrix(mtcars$vs)
y
x0 = rep(1,32)
x1 = mtcars$mpg
x2 = mtcars$am
x = cbind(x0,x1,x2)
x
glm_b3 = glm(y ~ x[,2:3], family = binomial("logit"))
b4 = glm_b3$coefficients

b3 = matrix(rep(0,3))
b5 = nralgo(x,y,b3)
data.frame(b5,b4)

####################################################
####################################################
mtcars

glm(vs ~ cyl+drat+wt, family = binomial("logit"),data = mtcars)

x1 = as.matrix(rep(1,32))
x2 = mtcars$cyl
x3 = mtcars$drat
x4 = mtcars$wt
x = cbind(x1,x2,x3,x4)
y = as.matrix(mtcars$vs)

x
y
b0 = rep(0,4)
nralgo(x,y,b0)

############################################### 以上

b0=c(0,0,0)
epsilon = 1E-10
MaxIter = 100000
b=NULL
n = 1
bnew = b0-solve(hess(x,b0),grad(x,y,b0))
b[1]=sum(x%*%bnew)
change = abs(func(x,y,bnew)-func(x,y,b0))
while( (n <= MaxIter) && (change > epsilon) )
{ 
  n = n+1 
  bold = bnew 
  bnew = bold-solve(hess(x,bold),grad(x,y,bold))
  b[n]=sum(x%*%bnew)
  change = abs(func(x,y,bnew)-func(x,y,bold))
}
plot(1:n,b,type = "l",ylab = "y=x*beta",xlab = "times")

x1 = as.matrix(rep(1,30))
x2 = as.matrix(1:30,1)
x3 = as.matrix(seq(0.1,3,by=0.1))
x4 = as.matrix(seq(0.2,6,by=0.2))
x = cbind(x1,x2,x3,x4)
y = as.matrix(c(rep(0,15),rep(1,15)))
b0 = as.matrix(rep(100,4))
glm(y ~ x2+x3+x4, family = binomial("logit"))

install.packages("corrplot")
library(corrplot)
?corrplot


nralgo = function(x,y,b){
  epsilon = 1E-16
  MaxIter = 50000
  b=NULL
  n = 1
  bnew = b0-solve(0.1*I,grad(x,y,b0))
  b[1]=sum(x%*%bnew)
  change = abs(func(x,y,bnew)-func(x,y,b0))
  while( (n <= MaxIter) && (change > epsilon) )
  { 
    n = n+1 
    bold = bnew 
    bnew = bold-solve(I,grad(x,y,bold))
    b[n]=sum(x%*%bnew)
    change = abs(func(x,y,bnew)-func(x,y,bold))
  }
  plot(1:n,b,type = "l",ylab = "y=x*beta",xlab = "times")
  return(bnew)
}


I = diag(rep(1,ncol(x)))
x


#作业：近似替代二阶导hess，比较结果 收敛速度 计算难度（达到同样精度所用时间）


