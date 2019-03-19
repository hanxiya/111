##########
#刻度grid#
##########
x = c(3,10,12,17,25) 
y = c(29,4,1,5,15)  
plot(x,y,type="o",pch=15,yaxt="n",xaxt="n",  
     main="mygrid",xlab="x",  
     ylab="y")                              #作图时横、纵轴不显示刻度线和刻度标签                             

x.text = seq(5,25,5)                        #根据数据范围设置坐标轴刻度 
y.text = seq(5,30,5)

mygrid = function(x,y)                      #网格线，参数为设置好的横纵轴刻度
{
  axis(1,at=x.text,labels=x.text) 
  axis(2,at=y.text,labels=y.text) 
  abline(v=x,lty = 2,lwd=1,col = "grey")
  abline(h=y,lty = 2,lwd=1,col = "grey")
}

mygrid(x.text,y.text)

##########
#分屏作图#
##########
x = rexp(1000)
fx = dexp(x)

split.screen(c(1,2))                      
split.screen(c(2,1),2)             

screen(1)                             
hist(x)  

screen(3)                             
plot(x,fx) 
text(x=4, y=.8, expression(fx = lambda*exp(-lambda*x)))

screen(4)  
plot((1:10)^2,type="l") 
close.screen(all = TRUE)     

##########
#作图补充#
##########

windows()
par(mfrow = c(2,4),las=1,adj=1,bty="o")

##########
# 条形图 #
##########
data = 1:10
barplot(data,main = "条形图")

colors = c("grey","white")
months = c("一月","二月","三月","四月","五月")
cities = c("北方","南方")
Values = matrix(c(5,10,12,16,14,18,15,20,22,27),nrow = 2,ncol = 5,byrow = TRUE)
barplot(Values,main = "降水总量",names.arg = months,xlab = "月份",ylab = "降水量",
        col = colors)
legend("topleft", cities, cex = 1, fill = colors)

##########
#  饼图  #
##########
x = c(30,12)
labels <- c("南方","北方")
pie(x,labels,main = "降水量")

##########
# 直方图 #
##########
v <- c(4,9,13,21,14,8,36,22,12,31,33,19,28)
hist(v, main="直方图", xlab = "x", ylab="y",col = "grey",xlim = c(0,40), ylim = c(0,4))

##########
# 散点图 #
##########
x = 1:10
y = x^2
plot(x,y,type = "b",col = 2,lwd = 2,main = "散点图")

##########
# 箱线图 #
##########
x1 = rnorm(100,0,1)
x2 = rt(100,3)
x = data.frame(x1,x2)
boxplot(x,col = c("grey","pink"),main = "箱线图")

##########
#  QQ图  #
##########
x = rnorm(100,0,1)
qqnorm(x)
qqline(x,col=2,lwd=2)

##########
# 茎叶图 #
##########
x = c(10,20,11,15,45,62,52,18,4,7,59,62,33,15,6,25,95)
stem(x)
















