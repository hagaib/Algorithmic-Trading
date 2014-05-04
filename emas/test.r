## .. EMAs package
## Main idea: use normalized distances from various moving averages as
## predictors, and use random forests to predict

## TODO: connect to API for testing
FUTURE <- 10
BID <- 2000000

## Loading API
source("get_data.r")
options(scipen = 10**3)
options(digits = 10)

## Load EMA averaging tool
source('emas/ema.r')

## Load model
model <- readRDS("eurusd.rds")
vipvars <- readRDS("indices.rds")

## Should contain only 1 Mil. money
my_assets()

## Should be empty
my_bids()

## Should be empty
my_data()

assets <- c('German5', 'German10', 'EUR_USD', 'TYc1', 'US5Y')
n_assets <- length(assets)
# m <- 100
# m.all <- m * n_assets
m.all <- 250-1

emas <- list()
emas.states <- array(dim=c(n_assets,m.all))
for(i in 1:m.all)
{
  emas.states[,i] <- rep(NA, n_assets)
}

date <- TRUE
timer <- 0
pnl <- 0

while(!is.na(date))
{
  ## Fetch last row
  row <- get_data()
  date <- row[1]
  ind <- 1

  ## Fetch last close
  asset.count <- 0
  state <- rep(NA, m.all)
  for(asset in assets)
  {
    close.name <- paste('close', asset, sep='_')
    close <- row[,close.name]
    asset.ema.indices <- vipvars[,asset]
    
    ## Let EMAs work
    for(i in asset.ema.indices)
    {
#       ind <- asset.count * m + i
      if (i != 0)
      {
        ema.i <- ema(i)
#         print(ind)
        emas.states[,ind] <- ema.i(emas.states[,ind], c(close, rep(NA, 4)))
        state[ind] <- emas.states[,ind][5]
        ind <- ind+1
      }
    }

    ## Number of bids
    # bids.name <- paste('n_bids', asset, sep='_')
    # state[ind] <- row[,bids.name]
    # state[ind][is.na(state[ind])] <- -1

    asset.count <- asset.count + 1
  }

  if(timer==0)
  {
#     print(state)
    pred <- predict(model, state, predict.all=TRUE)
    votes <- as.numeric(pred$individual)
    p <- as.numeric(as.character(pred$aggregate))
#     if(abs(mean(votes)) > 0.18)
    {
      my_bids(c(NA, NA, NA, NA, BID*p))
      if(p!=0)
      {
        timer <- FUTURE
      }
      pnl <- c(pnl, estimated_total_money()-1000000)
    }
  } else {
    timer <- timer - 1
  }
  
}

