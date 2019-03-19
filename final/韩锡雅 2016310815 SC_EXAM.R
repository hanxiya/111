rm(list = ls())
setwd("F:/QQPCmgr/Desktop")

####### package #######

#install.packages("rvest")
library(rvest)

####### 爬取数据 赶集网北京市昌平区二手房信息 #######

DataScrap = function(url){
  
  library(rvest)
  web = url %>% html_session %>% 
    read_html(encoding="utf-8") %>% 
    html_nodes("div.f-main-list>div>div")
  
  #提取标题
  Title = web %>% html_nodes("dl>dd>a") %>% html_attr("title")
  #提取单价
  Unit = web %>% html_nodes("dl>dd>div.time") %>% html_text %>% 
    gsub(pattern="[^0-9]",replacement="") %>% as.numeric
  #提取面积
  Area = web %>% html_nodes("dl>dd[data-huxing]") %>% 
    html_attr("data-area") %>% 
    gsub(pattern="[^0-9]",replacement="")
  #提取价格
  Price = web %>% html_nodes("dl>dd>div.price>span:first-child") %>% html_text
  #提取户型
  Type = web %>% html_nodes("dl>dd[data-huxing]") %>% html_attr("data-huxing")
  #提取朝向
  Tmp = web %>% html_nodes("dl>dd[data-huxing]>span") %>% html_text
  Orientation = Tmp[seq(from=5,to=length(Tmp),by=9)]
  #提取楼层
  Floor = Tmp[seq(from=7,to=length(Tmp),by=9)] %>% 
    gsub(pattern="\n",replacement="")
  #提取装修
  Decoration = Tmp[seq(from=9,to=length(Tmp),by=9)]
  #提取地址
  Add = web %>% html_nodes("dl>dd>span.area") %>% html_text %>% 
    gsub(pattern="\n",replacement=" ") %>% 
    gsub(pattern=" ",replacement="")
  #信息合并
  result = data.frame(
    Title=Title,
    Unit=Unit,
    Area=Area,
    Price=Price,
    Type=Type,
    Decoration=Decoration,
    Floor=Floor,
    Orientation=Orientation,
    Add=Add,
    stringsAsFactors=FALSE)

  return(result)
}

mydata=DataScrap("http://bj.ganji.com/fang5/changping/")
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o2"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o3"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o4"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o5"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o6"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o7"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o8"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o9"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o10"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o11"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o12"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o13"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o14"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o15"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o16"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o17"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o18"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o19"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o20"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o21"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o22"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o23"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o24"))
mydata=rbind(mydata,DataScrap("http://bj.ganji.com/fang5/changping/o25"))

mydata
write.csv(mydata,file="mydata.csv")
sec_hand = read.csv("mydata.csv")

### 二手房单价分析 ###
unit=sec_hand$Unit
# 数值变量summary函数 #
mySummary = function(x)
{
  x = as.numeric(x)
  
  if(is.numeric(x))
  {
    lst1 = list(mean = round(mean(x),1),
                var = round(var(x),1),
                sd  = round(sd(x),1),
                max = max(x),
                min = min(x),
                range = max(x) - min(x),
                med = median(x),
                length = length(x),
                type = typeof(x),
                class = class(x))
    out = unlist(lst1)
    return(out)
  }
}

mySummary(unit) #从二手房单价中得到信息
par(mfrow=c(1,2))
boxplot(unit)
hist(unit,breaks=25,freq = F)
lines(density(unit),col="red")
#单价出现双峰，但整体大致有正态分布轮廓

### 二手房装修、朝向以及对单价的影响单价分析 ###
dec = sec_hand$Decoration  #装修
ori = sec_hand$Orientation #朝向
table(dec)
boxplot(unit~dec)
barplot(table(dec))
table(ori)
boxplot(unit~ori)
barplot(table(ori))


### 二手房户型、楼层词频统计 ###
#install.packages("jiebaR")
#install.packages("wordcloud2")
library(jiebaR)
library(wordcloud2)

typ=sec_hand$Type #户型
typ1 = table(typ) #统计词频  
typ1 = sort(typ1, decreasing = TRUE) #降序排序 
typ1=data.frame(typ1)
wordcloud2(typ1,size=0.7,shape = "circle",minRotation = -pi/12, maxRotation = -pi/12, 
           rotateRatio = 1) 

flo=sec_hand$Floor#楼层
flo1 = table(flo) #统计词频  
flo1 = sort(flo1, decreasing = TRUE) #降序排序 
flo1=data.frame(flo1)
wordcloud2(flo1,size=0.5,shape = "circle",minRotation = -pi/12, maxRotation = -pi/12, 
           rotateRatio = 1) 

### 二手房面积分析 ###
area=sec_hand$Area #面积
mySummary(area)
par(mfrow=c(1,2))
hist(area) #右偏分布
boxplot(area)

### 二手房总价分析 ###
price = as.numeric(sec_hand$Price) #总价
mySummary(price)
par(mfrow=c(1,2))
hist(price,breaks=50)
#总价呈明显的右偏分布

#install.packages("STAR")
library(STAR)
hist(rinvgauss(1000,mu=536.4,sigma2 = 0.006),breaks=50) #一个逆高斯(IG)分布
#总价密度图与逆高斯分布密度图相近，猜想总价服从一个逆高斯分布

####### 对price进行密度估计 #######
#对于总价price，假定服从逆高斯分布，参数mu，sigma2未知

#逆高斯分布的对数似然函数
ig_loglikelyhood = function(x,mu,sigma2)
{
  #sum(-0.5*log(2*pi)-1.5*log(x)-0.5*log(sigma2)-(x-mu)^2/(2*mu^2*sigma2*x))
  sum(dinvgauss(x,mu,sigma2,log=T))
}

#逆高斯分布的对数似然函数 一阶导函数
fun=expression(-0.5*log(2*pi)-1.5*log(x)-0.5*log(sigma2)-(x-mu)^2/(2*mu^2*sigma2*x))
D(fun,"x") #表达式求导
dig_loglikelyhood = function(x,mu,sigma2) 
{
  sum(-(1.5*(1/x)+(2*(x-mu)/(2*mu^2*sigma2*x)-(x-mu)^2*(2*mu^2*sigma2)/(2*mu^2*sigma2*x)^2)))
}

#对于price的分布，先假定sigma2=0.1，寻找mu的最佳值
muS = seq(1,1000,1)           
LS = NULL
for(i in 1:length(muS))
{
  LS[i] = ig_loglikelyhood(price,muS[i], 0.1)
}
cbind(muS,LS) 
which.max(LS)
muS[which.max(LS)]
plot(muS,LS,type = "l")
abline(v = 530,lty=2,col=2)
abline(v = 540,lty=2,col=2)
shapeS[which.max(LS)]
#mu最佳536

#缩小mu范围，重复以上步骤
muS = seq(535,537,0.01)           
LS = NULL
for(i in 1:length(muS))
{
  LS[i] = ig_loglikelyhood(price,muS[i], 0.1)
}
cbind(muS,LS) 
which.max(LS)
muS[which.max(LS)]
LS[which.max(LS)]
plot(muS,LS,type = "l")
abline(v = 536,lty=2,col=2)
abline(v = 537,lty=2,col=2)
abline(v = muS[which.max(LS)],lty=2,col=2)
abline(h = LS[which.max(LS)],lty=2,col=2)
#mu最佳536.42

#固定mu=536.42，寻找sigma2最佳值
mu=536.42
sigma2S = seq(0.00001,100)       
LS = NULL
for(i in 1:length(sigma2S))
{
  LS[i] = ig_loglikelyhood(price,mu,sigma2S[i])
}
cbind(sigma2S,LS)                            
plot(sigma2S,LS,type = "l" )
which.max(LS)
sigma2S[which.max(LS)]
LS[which.max(LS)]

#缩小sigma2范围，重复以上步骤
sigma2S = seq(0.00001,1.00001,0.00001)       
LS = NULL
for(i in 1:length(sigma2S))
{
  LS[i] = ig_loglikelyhood(price,mu,sigma2S[i])
}
cbind(sigma2S,LS)                            
which.max(LS)
sigma2S[which.max(LS)]
LS[which.max(LS)]
plot(sigma2S,LS,type = "l" ,xlim=c(0.002,0.004),ylim=c(-7880,-7875))
abline(v = 0.003,lty=2,col=2)
abline(v = 0.0035,lty=2,col=2)
abline(v = sigma2S[which.max(LS)],lty=2,col=2)
abline(h = LS[which.max(LS)],lty=2,col=2)
#sigma2最佳0.00326

#结果
mu=536.42
sigma2=0.00326

#生成mu=536.42，sigma2=0.00326的逆高斯分布随机数并排序
ig_rand=rinvgauss(10000,mu,sigma2)

hist(price,freq=F,ylim=c(0,0.0025))      #总价频率直方图
lines(density(price),col="blue")         #总价密度图
points(density(ig_rand),col="red",ty="l")#逆高斯随机数密度图
legend("topright",c("density of price","density of IG"),
       lty=c(1,1),col=c("blue","red"),ncol=1)

#结论：总价price服从一个均值536.42，方差0.00326的逆高斯分布


####### 逆高斯回归 迭代加权最小二乘估计 #######
##逆高斯回归模型的系数估计函数##
IG_reg = function(y,X)
{
  D=1
  mu=(y+mean(y))/2
  eta=log(mu)
  deltaD=1
  while(max(abs(deltaD))>1e-08)
  {
    W=diag(1/mu)
    z=eta+(y-mu)/mu
    beta=solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%z
    eta=c(X%*%beta)
    mu=exp(eta)
    Dold=D
    D=sum((y-mu)^2)/(y*mu^2)
    deltaD=Dold-D
    chi2=sum((y-mu)^2/mu^3)
  }
  fitted=exp(X%*%beta)
  e=y-fitted
  residuals=scale(e)
  sse=sum((y-fitted)^2)
  n=length(y)
  p=ncol(X)-1
 # aic=n*log(sse)+2*(p+1)-n*log(n)
  aic=n*log(2*pi)+n*log(sse/n)+n+2*(p+1)

  out=list(coe=beta,fitted=fitted,
           residuals=residuals,sse=sse,
           aic=aic,chi2=chi2)
  return(out)
}

#因变量
y = price 
#自变量
x = as.numeric(typ)
X=model.matrix(~x) #自变量矩阵

#结果
fit_IG = IG_reg(y,X)
coe_IG = fit_IG$coe

fit_glm = glm(y~x,family=inverse.gaussian(link=log))
summary(fit_glm)
coe_glm = fit_glm$coef

#install.packages("gamlss")
library(gamlss)
fit_gamlss = gamlss(y~x,family=IG,mu.link=log,sigma.link=log)
summary(fit_gamlss)
coe_gamlss = fit_gamlss$mu.coefficients

#系数对比
data.frame(WLS=coe_IG,glm=coe_glm,gamlss=coe_gamlss)

#拟合效果对比
par(mfrow=c(1,3))
plot(y,fit_IG$fitted,main="WLS",ylab="fitted",xlab="y")
lines(1:8000,1:8000,col="red")
plot(y,fitted(fit_glm),main="glm",ylab="fitted",xlab="y")
lines(1:8000,1:8000,col="red")
plot(y,fitted(fit_gamlss),main="gamlss",ylab="fitted",xlab="y")
lines(1:8000,1:8000,col="red")

#残差效果图
par(mfrow=c(2,2))
plot(fit_glm)
plot(fit_gamlss)

#残差顺序图
par(mfrow=c(1,3))
plot(1:1100,fit_IG$residuals,main="WLS",xlab="index",ylab="residuals",ylim=c(-10,10))
abline(h=0,col="red")
plot(1:1100,fit_glm$residuals,main="glm",xlab="index",ylab="residuals",ylim=c(-10,10))
abline(h=0,col="red")
plot(1:1100,fit_gamlss$residuals,main="gamlss",xlab="index",ylab="residuals",ylim=c(-10,10))
abline(h=0,col="red")

## AIC值对比
fit_IG$aic
fit_glm$aic
fit_gamlss$aic

data.frame(WLS=fit_IG$aic,glm=fit_glm$aic,gamlss=fit_gamlss$aic,row.names="AIC")


#####################
#####################