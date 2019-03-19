###apply函数族###
#apply(X, MARGIN, FUN, ...)
#行或列遍历操作函数，适用于array(matrix)
a = matrix(1:24,4,6)
a
apply(a,1,sum)
apply(a,1,function(x) x^2)        #margin=1对行操作
apply(a,2,prod)
apply(a,2,sqrt)                   #margin=2对列操作

ary = array(1:120,c(4,6,5))          
ary
apply(ary,c(1,3),mean)

#lapply(X, FUN, ...)
#遍历函数，适用于list或vector
lst = list(a=1:10,b=4:6,c=9:24)    
lst
lapply(lst, sum)
lapply(lst, length)

#sapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)
#遍历函数，将结果整理以向量、矩阵、列表的形式输出,适用于list或vector
#simplify=F时与lapply完全相同
sapply(lst, sum,simplify = FALSE)
sapply(lst, sum,simplify = TRUE)   #返回结果被简化

#rapply(X, FUN, how = c("unlist", "replace", "list"), ...)
#lapply的递归版，适用于list
lst$d = list(d1 = matrix(1:4,2,2),d2 = 1:5)
lst
rapply(lst, sum)
rapply(lst, sum, how = "replace")
rapply(lst, sum, how = "list")
rapply(lst, length)
rapply(lst, length, how = "replace")
rapply(lst, length, how = "list")

#mapply(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE,USE.NAMES = TRUE)
#多参数版本的sapply()
x=1:10
y=5:14
mapply(function(x,y) x^2+y^2,x,y)

lst1 = list(a = 1:10,b = 4:6,c = 9:24)  
lst2 = lapply(lst1, function(x) x-2)
mapply(function(x,y) x^2+y^2, lst1, lst2)

#tapply(X, INDEX, FUN = NULL, ..., default = NA, simplify = TRUE)
#根据index对x分组进行函数操作，适用于不规则数组
age = c(24,25,36,37)
sex = c('f', 'm', 'f','m')
tapply(age, sex, mean)

data = data.frame(num = 1:10, 
                  age = rep(c(20,30),5), 
                  sex = rep(c("f","m"), each = 5),
                  sal = seq(1000,10000, length.out = 10))
data
tapply(data$sal, data$age, mean)              #不同年龄组工资均值
tapply(data$sal, data$sex, mean)              #不同性别组工资均值
tapply(data$sal, data[c('age','sex')], mean)  #不同年龄和性别组工资均值

#sweep(x, MARGIN, STATS, FUN = "-", check.margin = TRUE, ...)
#输入数组,通过清除汇总统计数据,返回获得的数组。
ary1 = array(1:24,c(4,3,2))
x = apply(ary1,1,mean)
x
sweep(ary1,1,x)

#aggregate(x, ...)
#将数据拆分为子集,计算并返回每个子集的汇总统计信息
data
aggregate(x = data$sal, by = list(data$age), FUN = "mean")
aggregate(x = data$sal, by = list(data$sex), FUN = "max")
aggregate(x = data$sal, by = list(data$age, data$sex), FUN = "mean")
