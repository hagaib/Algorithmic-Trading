## learn.r
## This module is aimed for learning purposes

## ----- Constants ----- ##
## We'll denote constants by uppercase letters

## TRAIN is the amount of data (from each instrument) that we will train our
## modules upon
TRAIN <- 300000
TEST <- 100000

## ----- Data ----- ##
data <- readRDS("all_data_wide.rds")
# eurusd.opens <- data$open_EUR_USD[!is.na(data$open_EUR_USD)][1:TRAIN]
eurusd <- data.frame(o=data$open_EUR_USD, h=data$high_EUR_USD, l=data$low_EUR_USD,
                     c=data$close_EUR_USD)
eurusd.na <- is.na(eurusd$o) | is.na(eurusd$h) | is.na(eurusd$l) |
             is.na(eurusd$c)
eurusd.clean <- eurusd[!eurusd.na, c('o','h','l','c')]
eurusd.train <- eurusd.clean[1:TRAIN, c('o','h','l','c')]
eurusd.test <- eurusd.clean[(TRAIN+1):(TRAIN+TEST), c('o','h','l','c')]

## ----- Ape Model ----- ##
## Ape produces signals randomly
ape.pred <- function(train, test) {
  return(sample((-1:1), TRAIN, replace=TRUE))
}

## ----- Candle Model ----- ##
candle.pred <- function(train, test) {
  bars.diff <- test$c - test$o
  return(-sign(bars.diff))
}

## ----- Linear Ression Model ----- ##
linreg.learn <- function(data) {
  closes <- data$c
  n <- length(closes)
  m <- 10
  x <- array(dim=c(n,m))
  for (i in 1:m) {
    x[,i] <- c(rep(NA, (i-1)), closes[1:(n-i+1)])
  }
  # x <- x[m:(n-2),]
  x <- x[m:(n-6),]
  opens = data$o
  # odiff <- diff(opens, lag=1)[(m+1):(n-1)]
  odiff <- diff(opens, lag=5)[(m+1):(n-5)]
  data.learn <- as.data.frame(x)
  data.learn$y <- odiff
  mod <- lm(formula=y~., data=data.learn)
  return(mod)
}

linreg.pred <- function(train, test) {
  mod <- linreg.learn(train)
  closes <- test$c
  n <- length(closes)
  m <- 10
  x <- array(dim=c(n,m))
  for (i in 1:m) {
    x[,i] <- c(rep(NA, (i-1)), closes[1:(n-i+1)])
  }
  x <- x[m:(n-2),]
  data.test <- as.data.frame(x)
  pred <- predict(mod, data.test)
  spred <- c(rep(0,m-1),sign(pred))
  spred.each <- spred[seq(1, length(spred), 5)]
  spred.rep <- NULL
  for (each in spred.each) {
    spred.rep <- c(spred.rep, rep(each, 5))
  }
  return(spred.rep)
}

## ----- Results Calculation ----- ##
calc.profit <- function(opens, pred) {
  n <- length(opens)
  ## odiff is of length n-2 (after truncate)
  odiff <- diff(opens, lag=1)[2:(n-1)]
  ## truncate pred
  pred <- pred[1:(n-2)]
  ## Calc profits
  profits <- odiff * pred
  return(profits)
}

## ----- Test ----- ##
ape.profit.eurusd <- calc.profit(eurusd.test$o, ape.pred(eurusd.train, eurusd.test))
candle.profit.eurusd <- calc.profit(eurusd.test$o, candle.pred(eurusd.train, eurusd.test))
linreg.profit.eurusd <- calc.profit(eurusd.test$o, linreg.pred(eurusd.train, eurusd.test))
print(mean(linreg.profit.eurusd))
# linreg.profit.eurusd.2 <- calc.profit(eurusd.test$o, linreg.pred(eurusd.test, eurusd.test))
# print(mean(linreg.profit.eurusd.2))

