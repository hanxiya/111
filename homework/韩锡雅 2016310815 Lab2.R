###作业2###

##向量vector##

#向量的创建#
#函数c()
a = c(1,2,3,4)
a
b = c('a','b','c')
b
c = c(TRUE,FALSE,TRUE,FALSE)
c
#冒号
d = 1:10
d 
#函数seq()
e = seq(1,100,length.out = 12)
e
#函数rep()
f = rep(1, 10)
f

#增加向量元素#
a = c(a,c(1,2))        #a增加2个元素
a
a = append(a,3)        #在向量最后追加一个新元素
a
a = append(a,c(4,5))   #在向量后追加连个元素
a
a = append(a,0,4)      #在第4个元素后追加一个新元素0
a

#向量的访问#
#通过方括号访问向量里的元素
a[1]
b[1]
c[2]
d[c(1,3,4)]
e[c(2,3)]

#向量的一些函数#
#长度
length(a)
#求和函数sum()
sum(a)
#最大值函数max()
max(a)
which.max(a)
#最小值函数min()
min(a) 
which.min(a)
#均值函数mean()
mean(a)  
#中位数函数median()
median(a)  
#方差函数var()
var(a)
#标准差函数 sd()
sd(a)  
#向量元素值连乘积prod()
prod(d)  

#转换为...#
#矩阵
a1 = as.matrix(a)
a1
class(a1)
#数组
a2 = as.array(a)
a2
class(a2)
#数据框
a3 = as.data.frame(a)
a3
class(a3)
#列表
a4 = as.list(a)
a4
class(a4)

#特点#
#向量是用于存储数值型、字符型或逻辑型数据的一维数组。
#是最基本的数据容器，里面的数据必须是同一类型。


##矩阵matrix##

#矩阵的创建#
x = matrix(c(1,2,3,4),nrow = 2,ncol = 2) 
x
y = diag(1,3)                #对角矩阵
y

#增加矩阵元素#
z = c(0,0)
cbind(x,z)
rbind(x,z)

#矩阵元素的引用#
x[1,1]
x[2]
x[1,]
x[,2]

#矩阵的一些函数#
#矩阵转置
t(x)
#矩阵的逆
solve(x)  
#矩阵行列式计算
det(x) 
#特征值&特征向量
eigen(x)  
#矩阵的秩
qr(x)$rank
#矩阵维度
dim(x)
#矩阵行数
nrow(x)
#矩阵列数
ncol(x)
#矩阵行和
rowSums(x)
#矩阵列和
colSums(x)
#矩阵行均值
rowSums(x)
#矩阵列均值
colMeans(x)

#转换为...#
#向量
x1 = as.vector(x)
x1
class(x1)
#数组
x2 = as.array(a)
x2
class(x2)
#数据框
x3 = as.data.frame(x)
x3
class(x3)
#列表
x4 = as.list(x)
x4
class(x4)

#特点#
#矩阵是二维数组
#矩阵不能存储不同类型元素，元素类型会强制转换
z=matrix(1:4,2,2)
z
z[4]="a"
typeof(z)  



##数组array##

#数组的创建#
ary=array(1:24,c(2,3,4))                    
ary

#数组元素的增加#
A = array(1:10)
B = array(10:1)
cbind(A,B)

#数组元素的引用#
ary[1,1,1]
ary[2]
ary[1,,]
ary[1,1,]

#数组的一些函数(与向量矩阵部分相同)#
sum(ary)
min(ary[1,,])
max(ary[1,,])
prod(ary[1,1,])
dim(ary)
length(ary)
mean(ary)  
median(ary)  
var(ary)
sd(ary)  

#转换为...#
#向量
ary1 = as.vector(ary)
ary1
class(ary1)
#矩阵
ary2 = as.matrix(ary)
ary2
class(ary2)
#数据框
ary3 = as.data.frame(ary)
ary3
class(ary3)
#列表
ary4 = as.list(ary)
ary4
class(ary4)

#特点#
#数组是多维的同一类型元素的集合。


##数据框##

#数据框的创建#
name=c("zhang","li","wang","zhao")
age=c(19,28,17,14)
sal=c(200,100,5000,20000)
data=data.frame(name,age,sal) 
data

#数据框元素增加#
sex=c("m","f","f","m")
cbind(data,sex)  
#rbind(data,c("wu",20,3000,"m"))
data$sex = c("m","f","f","m")
data

#数据框元素引用#
data$name[1]
data[1,1]

#数据框的一些函数#
dim(data)
class(data)
nrow(data)
ncol(data)
str(data)
rownames(data)
colnames(data)
names(data)
head(data)
tail(data)

#转换为...#
#向量
data1 = as.vector(data$name)
data1
class(data1)
#矩阵
data2 = as.matrix(data)
data2
class(data2)
#数组
data3 = as.array(data)
data3
class(data3)
#列表
data4 = as.list(data)
data4
class(data4)

#特点#
##矩阵中不能存放不同类型的元素，数据框解决了这种问题。


##列表list##

#列表的创建#
info=list(zhang=17,li=28,wang=99)
info

#列表元素添加#
info$zhang=matrix(1:4,2)
info$wang=data.frame(married="no" ,work="yes")
info$li=list(age=28,edu="cufe")
info$zhao="shandong"
info$zhao=NULL
info[[1]][1,2]=9999
info

#列表元素引用#
info[["li"]]
info$zhang
info[[2]][1]

#列表的一些函数#
class(info)
names(info)
#unlist()生成一个包含info1所有元素的向量,展平数据列表。
info1 = list( a = 3, b = 5, c= 9 )
info1
v = unlist(info1)
v
class(v)
#lapply()返回一个长度与X一致的列表，每个元素为FUN计算出的结果，且分别对应到X中的每个元素。
lapply(info1,log)     
lapply(info1,exp)
#lapply的递归形式：rapply
info1[["b"]]=list(b1=10,b2=20)
rapply(info1, log, how = "replace")
rapply(info1, log, how = "unlist")
rapply(info1, function(x) x^2, how="replace")

#转换为...#
#向量
info1 = unlist(info)
names(info1)=NULL
c(info2)
class(info1)
#矩阵
info2 = unlist(info)
names(info2)=NULL
matrix(info2,nrow=4)
class(info2)
#数组
info3 = as.array(info)
info3
class(info3)
#数据框
info4 = as.data.frame(info)
info4
class(info4)

#特点
#列表包含不同类型的元素，还可以包含一个矩阵或一个函数作为它的元素。

