###作业：利用R验证高等代数中矩阵的20条性质###

##性质1：秩(A+B)<=秩(A)+秩(B)
A = matrix(c(2,5,1,9,12,3,16,24,18),3)
B = matrix(c(5,8,19,26,15,12,31,8,7),3)   #定义矩阵A、B      
S = A + B                                 
rA = qr(A)$rank
rB = qr(B)$rank                           #计算矩阵的秩
qr(S)$rank <= qr(A)$rank + qr(B)$rank     #判断

##性质2：A+(B+C)=(A+B)+C
C = matrix(1:9,3)                         #定义矩阵C
A+(B+C) == (A+B)+C                        #判断

##性质3：A+B=B+A
A + B == B + A                            #判断

##性质4：A+0=A
ze = matrix(0,3,3)                        #构造零矩阵
A + ze == A                               #判断

##性质5：(AB)C=A(BC)
(A %*% B) %*% C == A %*% (B %*% C)        #判断

##性质6：矩阵乘法不适合交换律，即一般来说，AB不等于BA。
A %*% B == B %*% A                        #判断

##性质7：(k+l)A=kA+lA
k = 2
l = 3                                     #赋值
( k + l ) * A == k * A + l * A            #判断

##性质8：k(A+B)=kA+kB
k * ( A + B ) == k * A + k * B            #判断

##性质9：k(AB)=(kA)B=A(kB)
k * ( A %*% B ) == ( k * A ) %*% B    
k * ( A %*% B ) == A %*% ( k * B )        #判断

##性质10：矩阵乘法分配律：A(B+C)=AB+AC
A %*% ( B + C ) == A %*% B + A %*% C      #判断

##性质11:(AB)'=B'A'
P = A %*% B                               #计算矩阵成乘积
Ptrans = t(P)
Atrans = t(A)
Btrans = t(B)                             #矩阵转置
Ptrans == Btrans %*% Atrans               #判断

##性质12：|AB|=|A||B|
det(P) == det(A) * det(B)                 #计算行列式并判断

##性质13：秩(AB)<=min[秩(A)，秩(B)]
qr(P)$rank <= min(rA,rB)                  #判断

##性质14：当矩阵A行列式不等于零时，A的逆等于A的伴随矩阵比A的行列式。
det(A) != 0                               #矩阵A可逆
Ainv = solve(A)                           #计算逆矩阵
library(LoopAnalyst)                 
Aad = make.adjoint(A)                     #计算伴随矩阵
d = det(A)                                #计算行列式
Ainv 
Aad/d                                     
                                          #比较后，二者相等

##性质15：矩阵A转置的逆矩阵等于A逆的转置。
solve(Atrans)                             #计算逆矩阵
t(Ainv)                                   #转置
                                          #比较后，二者相等

##性质16：矩阵A与B乘积的逆等于B的逆乘A的逆。
solve(P)                                  #计算逆矩阵
solve(B) %*% solve(A)                     #判断

##性质17：A是一个s*n的矩阵，如果P是s*s可逆矩阵，Q是n*n可逆矩阵，那么秩(A)=秩(PA)=秩(AQ)。
A
P
det(P)                                    #矩阵P可逆
Q = matrix(c(9,1,3,5,7,4,6,10,15),3)      #定义矩阵Q
Q
det(Q)                                    #矩阵Q可逆
rA == qr(P %*% A)$rank            
rA == qr(A %*% Q)$rank                    #计算秩并判断                   
                                          
##性质18：矩阵特征值的乘积等于矩阵行列式。
ev = eigen(A)$values                      #计算特征值
prod(ev)                                  #特征值的乘积
det(A)                                    #行列式
                                          #比较后，二者相等
##性质19：相似矩阵的特征值相等。
det(B)                                    #矩阵B可逆
Asim = solve(B) %*% A %*% B               #计算A相似矩阵
eigen(Asim)$values                        #相似阵的特征值  
ev                                        #A的特征值
                                          #比较后，二者相等
##性质20：A的逆矩阵的行列式等于A的行列式的倒数。
det(Ainv)                                 #A逆矩阵的行列式
1/det(A)                                  #A行列式倒数
                                          #比较后，二者相等
all.equal(det(Ainv),1/det(A))
 








