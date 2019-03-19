

## The image matrix for training sample. 256x1707
azip <- read.table("azip.dat")

## The true digits given in the training sample. lenght = 1707
dzip <- as.numeric(read.table("dzip.dat"))

## The testing image matrix. 256x2007
testzip <- read.table("testzip.dat")

## The true digits for the testing sample. length = 2007
dtest <- read.table("dtest.dat")



## Find the optimum basis function.

test.idx <- 1:100
basis.max <- 1:80
nCorrectOut <- numeric(length(test.idx))
resid.norm <- matrix(0, 10, 1, dimnames = list(0:9,"resid"))
for(b in basis.max)
{
  nCorrect <- 0
  for(k in test.idx)
  {
    for(i in 0:9)
    {
      img.mat <- azip[, i == dzip, drop = FALSE]
      img.matSVD <- svd(img.mat)
      resid.norm[i+1, ] <- norm(matrix(lm(testzip[, test.idx[k]]~
                                            0+img.matSVD$u[, 1:basis.max[b]])$resid), "F")
    }
    if((which.min(resid.norm)-1)  == dtest[k])
    {
      nCorrect <- nCorrect+1
    }
  }
  nCorrectOut[b] <- nCorrect
  print(b)
}
plot(nCorrectOut[1:80],ty="l",xlab="No. of bases",ylab="correct specification rate")
