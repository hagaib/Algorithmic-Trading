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

