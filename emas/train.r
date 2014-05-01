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

    ## Calculate the new mean
    alpha <- 2 / (n+1)
    mean.cur <- alpha * value.cur + (1-alpha) * mean.prev

    ## Calculate the new variance
    std.prev <- prev[3]
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
for (asset in c('German5', 'German10', 'EUR_USD', 'TYc1', 'US5Y'))
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
predictors[is.na(predictors)] <- 0


## ----- Prepare Response Vectors ----- ##
print("Preparing response vectors...")
response <- data.frame(idx=numeric(n-2))

## We actually prepare 5 response vectors, one for each asset.
## We begin with the naive alternative of the response vector: the sign of the
## difference between the next-next-open and the next-open.
for (asset in c('German5', 'German10', 'EUR_USD', 'TYc1', 'US5Y'))
{
  asset.o.name <- paste('open', asset, sep='_')
  asset.o.data <- data[,asset.o.name]

  ## We prepare an asset-specific response vector, and the plug it into our
  ## response data frame with [,asset].
  response[,asset] <- sign(diff(asset.o.data)[2:(n-1)])

  ## We replace NAs with 0s; not optimal, but I can't think of a better solution
  response[,asset][is.na(response[,asset])] <- 0

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

rf <- data.frame(idx=numeric(n-2))
for (asset in c('German5', 'German10', 'EUR_USD', 'TYc1', 'US5Y'))
{
  resp <- factor(response[,asset])
  ## We use 'factor' to force classification

  ## We set 'importance=TRUE' to asses importance of predictors
  rf[,asset] <- randomForest(predictors, resp, importance=TRUE)

  ## TODO: currently we train the model for all values, including NA's replaced
  ## by 0's. Instead, the model should be trained on data which is not NA in
  ## the relevant asset (but it can be 0 on the rest).

  print(paste("... done with random forest of", asset))
}

