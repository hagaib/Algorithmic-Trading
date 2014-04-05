## The size of our training data
MAXDATA <- 4000
SHIFT <- 30000

## Reading using 'data.table'
require(data.table)
file <- "data/EUR-USD_BID_1MIN-N63619143.csv"
data <- as.data.frame(fread(file))

## Getting close prices
cl <- data[10]

## Turn data into a vector
cl.v <- c(cl)[[1]]

## Filter 0's
cl.nz <- cl.v[cl.v!=0]

## Truncate data
cl.tr <- cl.nz[(SHIFT+1):(SHIFT+MAXDATA)]

## Outputs:
## 
print(str(cl.tr))

## Producing the case matrix
n <- length(cl.tr)
m <- 100
x <- array(dim=c(n,m))
for (i in 1:m) {
  x[,i] <- c(rep(NA, i), diff(cl.tr, lag=i))
}

## Truncating the matrix to avoid NA's
x <- x[(m+1):(n-1),]

## Producing the response vector
diffs <- diff(cl.tr, lag=1)[(m+1):(n-1)]
diffs.sgn <- sign(diffs)

## we use 'factor' to force classification
y <- factor(diffs.sgn)

## Getting the package
require('randomForest')

## Constructing the model
## We set 'importance=TRUE' to asses importance
## of predictors
rf <- randomForest(x, y, importance=TRUE)

## This takes some time...
print(rf)
plot(rf)
# plot(rf$importance[,4])

## Producing the new case matrix
x.r <- array(dim=c(n,(m+1)))
for (i in 1:m) {
  x.r[,i] <- c(rep(NA, i), diff(cl.tr, lag=i))
}
x.r[,m+1] = runif(n, -0.001, 0.001)

## Truncating the matrix to avoid NA's
x.r <- x.r[(m+1):(n-1),]

## Constructing the new model
# rf.r <- randomForest(x.r, y, importance=TRUE)
# print(rf.r)

## Producing the new case matrix
x.f <- array(dim=c(n,(m+1)))
for (i in 1:m) {
  x.f[,i] <- c(rep(NA, i), diff(cl.tr, lag=i))
}

## Truncating the matrix to avoid NA's
x.f <- x.f[(m+1):(n-1),]
x.f[,m+1] <- diffs

## Constructing the new model
# rf.f <- randomForest(x.f, y, importance=TRUE)
# print(rf.f)

## The shift of our test data
SHIFT.T <- 60000

## Truncate test data
cl.tst <- cl.nz[(SHIFT.T+1):(SHIFT.T+MAXDATA)]
x.tst <- array(dim=c(n,m))
for (i in 1:m) {
  x.tst[,i] <- c(rep(NA, i), diff(cl.tst, lag=i))
}
x.tst <- x.tst[(m+1):(n-1),]

## Test response vector
diffs.tst <- diff(cl.tst, lag=1)[(m+1):(n-1)]
diffs.tst.sgn <- sign(diffs.tst)
y.tst <- factor(diffs.tst.sgn)

## Constructing the model
# rf.tst <- randomForest(x, y, importance=TRUE,
                       # xtest=x.tst, ytest=y.tst)

# print(rf.tst)

## Getting the package
require('adabag')

## Constructing the model
x.d <- as.data.frame(x)
x.d$y <- y
# boos <- boosting(y~., x.d)

## Truncate test data
x.d.tst = as.data.frame(x.tst)
x.d.tst$y <- y.tst

## Testing
# boos.pr <- predict.boosting(boos, x.d.tst)

