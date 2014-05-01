## .. EMAs package
## Main idea: use normalized distances from various moving averages as
## predictors, and use random forests to predict

## ----- Averaging Tools ----- ##
ema <- function(n)
{
  helper <- function(prev, cur)
  {
    ## 'prev' and 'cur', like output, are vectors of the following form:
    ## (value, mean, std, dist, ndist)
    value.cur <- cur[1]
    mean.prev <- prev[2]

    ## This is the first value
    if(is.na(mean.prev))
    {
      return(c(value.cur, value.cur, 0, 0, 0))
    }

    std.prev <- prev[3]
    dist.prev <- prev[4]
    ndist.prev <- prev[5]

    ## The new value is NA, we don't change the mean
    if(is.na(value.cur))
    {
      return(c(value.cur, mean.prev, std.prev, dist.prev, ndist.prev))
    }

    ## Calculate the new mean
    alpha <- 2 / (n+1)
    mean.cur <- alpha * value.cur + (1-alpha) * mean.prev

    ## Calculate the new variance
    var.prev <- std.prev^2
    var.cur <- (alpha * (value.cur - mean.cur)
                * (value.cur - mean.prev) + (1-alpha) * var.prev)
    std.cur <- var.cur^0.5

    ## Calculate the new distance
    dist.cur <- value.cur - mean.cur

    ## Calculate the new normalized distance
    std.cur <- max(std.cur, 0.0001)
    ndist.cur <- dist.cur / std.cur

    return(c(value.cur, mean.cur, std.cur, dist.cur, ndist.cur))
  }

  return(helper)
}

## ----- Reading Train Data ----- ##
## Read data; we run this only after the prepare data package ran
if(FALSE)
{
  print("Reading train data...")
  data <- readRDS("train.rds")
} else {
  print("Reading sample data...")
  data <- readRDS("sample.rds")
}

n <- nrow(data)
print(paste("...", n, "rows"))


## ----- Prepare Predictors Matrix ----- ##
print("Preparing predictors matrix...")

## We construct a matrix of predictors
## m is the number of predictors for each asset
m <- 99
m.all <- m * 5

## Prepare the predictors array
predictors <- array(dim=c(n,m.all))

asset.count <- 0
## Loop over assets
assets <- c('German5', 'German10', 'EUR_USD', 'TYc1', 'US5Y')
for (asset in assets)
{
  asset.c.name <- paste('close', asset, sep='_')
  asset.c.data <- data[,asset.c.name]

  ## Time for stupid R magic to create a list of vectors from our data
  lst <- mapply(c, asset.c.data,
                rep(NA, n), rep(NA, n), rep(NA, n), rep(NA, n), SIMPLIFY=FALSE)

  ## Get the EMA vector for each relevant period
  for (i in (2:(m+1)))
  {
    ema.i <- ema(i)
    ema.i.lst <- Reduce(ema.i, lst, init=rep(NA, 5), accumulate=TRUE)
    ind <- asset.count * m + (i-1)
    res <- sapply(ema.i.lst, function(v) v[length(v)])
    predictors[,ind] <- res[2:(n+1)]
  }

  print(paste("... done with predictors of", asset))
  asset.count <- asset.count + 1
}

## We don't make predictions in the last 2 values
predictors <- predictors[1:(n-2),]

## We replace NAs with 0s; not optimal, but I can't think of a better solution
# predictors[is.na(predictors)] <- 0


## ----- Prepare Response Vectors ----- ##
print("Preparing response vectors...")
response <- data.frame(idx=numeric(n-2))

## We actually prepare 5 response vectors, one for each asset.
## We begin with the naive alternative of the response vector: the sign of the
## difference between the next-next-open and the next-open.
for (asset in assets)
{
  asset.o.name <- paste('open', asset, sep='_')
  asset.o.data <- data[,asset.o.name]

  ## We prepare an asset-specific response vector, and the plug it into our
  ## response data frame with [,asset].
  response[,asset] <- sign(diff(asset.o.data)[2:(n-1)])

  ## We replace NA's with 999's; not optimal, but I can't think of a better
  ## solution
  response[,asset][is.na(response[,asset])] <- 999

  print(paste("... done with response of", asset))
}

## TODO:
## Later we may consider a more delicate response: instead of taking the simple
## sign, taking 1 for "significantly higher", -1 for "significantly lower" or 0
## for "more or less the same", where what defines "significant" is at least
## 20% of the current bar size (high - low).


## ----- Training Random Forest models ----- ##
print("Training random forest models...")
require('randomForest')
ntree <- 100

## Can't make it in a loop, doing it manually... damn R
resp.german5 <- factor(response[,'German5'])
rf.german5 <- randomForest(predictors, resp.german5, importance=TRUE,
                           ntree=ntree, na.action=na.omit)
print("... done with random forest of German5")

resp.german10 <- factor(response[,'German10'])
rf.german10 <- randomForest(predictors, resp.german10, importance=TRUE,
                            ntree=ntree, na.action=na.omit)
print("... done with random forest of German10")

resp.eurusd <- factor(response[,'EUR_USD'])
rf.eurusd <- randomForest(predictors, resp.eurusd, importance=TRUE,
                          ntree=ntree, na.action=na.omit)
print("... done with random forest of EUR_USD")

resp.tyc1 <- factor(response[,'TYc1'])
rf.tyc1 <- randomForest(predictors, resp.tyc1, importance=TRUE, ntree=ntree,
                        na.action=na.omit)
print("... done with random forest of TYc1")

resp.us5y <- factor(response[,'US5Y'])
rf.us5y <- randomForest(predictors, resp.us5y, importance=TRUE, ntree=ntree,
                        na.action=na.omit)
print("... done with random forest of US5Y")


