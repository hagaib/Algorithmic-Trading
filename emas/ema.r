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

