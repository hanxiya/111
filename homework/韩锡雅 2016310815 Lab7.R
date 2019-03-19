windows()
par(mfrow = c(2,4))

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











