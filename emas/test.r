## .. EMAs package
## Main idea: use normalized distances from various moving averages as
## predictors, and use random forests to predict

## TODO: connect to API for testing
FUTURE <- 30
BID <- 1000000

## Loading API
source("../get_data.r")
options(scipen = 10**3)
options(digits = 10)

## Load EMA averaging tool
source('ema.r')

## Load model
model <- readRDS("eurusd.rds")

## Should contain only 1 Mil. money
my_assets()

## Should be empty
my_bids()

## Should be empty
my_data()

assets <- c('German5', 'German10', 'EUR_USD', 'TYc1', 'US5Y')
m <- 100
m.all <- m * length(assets)

emas <- list()
emas.states <- array(dim=c(5,m.all))
for(i in 1:m.all)
{
  emas[[i]] <- ema(i)
  emas.states[,i] <- rep(NA, 5)
}

date <- TRUE
timer <- 0
pnl <- 0
while(!is.na(date))
{
  ## Fetch last row
  row <- get_data()
  date <- row[1]

  ## Fetch last close
  asset.count <- 0
  state <- rep(NA, m*length(assets))
  for(asset in assets)
  {
    close.name <- paste('close', asset, sep='_')
    close <- row[,close.name]
    
    ## Let EMAs work
    for(i in 1:m)
    {
      ind <- asset.count * m + i
      ema.i <- emas[[ind]]
      emas.states[,ind] <- ema.i(emas.states[,ind], c(close, rep(NA, 4)))
      state[ind] <- emas.states[,ind][5]
    }

    asset.count <- asset.count + 1
  }

  if(timer==0)
  {
    p <- as.numeric(as.character(predict(model, state)))
    my_bids(c(NA, NA, BID*p, NA, NA))
    if(p!=0)
    {
      timer <- 30
    }
    pnl <- c(pnl, estimated_total_money()-1000000)
  } else {
    timer <- timer - 1
  }
  
}

