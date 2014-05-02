## .. EMAs package
## Main idea: use normalized distances from various moving averages as
## predictors, and use random forests to predict

## TODO: connect to API for testing
FUTURE <- 15

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

m <- 100
emas <- list()
emas.states <- array(dim=c(5,m))
for(i in 1:m)
{
  emas[[i]] <- ema(i)
  emas.states[,i] <- rep(NA, 5)
}

date <- TRUE
while(!is.na(date))
{
  ## Fetch last row
  row <- get_data()
  date <- row[1]

  ## Fetch last close
  close.name <- "close_EUR_USD"
  close <- row[,close.name]
  
  ## Let EMAs work
  state <- rep(NA, m)
  for(i in 1:m)
  {
    ema.i <- emas[[i]]
    emas.states[,i] <- ema.i(emas.states[,i], c(close, rep(NA, 4)))
    state[i] <- emas.states[,i][5]
  }

  print(model.predict(state))
  
}

