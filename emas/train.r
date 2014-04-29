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
    ndist.cur <- dist.cur / std.cur

    return(c(value.cur, mean.cur, std.cur, dist.cur, ndist.cur))
  }

  return(helper)
}


## ----- Train Model ----- ##

## Read data; we run this only after the prepare data package ran
data <- readRDS("train.rds")
print(str(data))

## We construct a matrix of predictors
n <- nrow(data)

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
    print(paste(asset, ind))
    res <- sapply(ema.i.lst, function(v) v[length(v)])
    predictors[,ind] <- res[2:(n+1)]
  }

  print(paste("Done with", asset))
  asset.count <- asset.count + 1
}


## TODO: create the response vector
## TODO: train a random forest

