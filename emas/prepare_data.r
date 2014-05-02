## .. EMAs package
## Main idea: use normalized distances from various moving averages as
## predictors, and use random forests to predict

## ----- Data preperation ----- ##
## This is taken from Tal's example
## This should be run only once

TEST <- 10^5
SAMPLE <- 10^4

## get all of the data
data.all <- readRDS("../all_data_wide.rds")

## Print some details about the data
head(data.all)
dim(data.all) # 647 784

## split the data to training and testing:
data.train <- tail(data.all, -TEST)
saveRDS(data.train, "train.rds")
data.test <- tail(data.all, TEST)
saveRDS(data.test, "test.rds")  # this is used by the get_data function!	

## For coding and debugging, make a short sample of train
data.sample <- tail(data.train, SAMPLE)
saveRDS(data.sample, "sample.rds")

