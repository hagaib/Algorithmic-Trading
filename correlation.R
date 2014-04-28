data_all <- readRDS('all_data_wide.rds')
train_set <- 1:100000
data_close <- rep(0, nrow(data_all))

for(i in 1:5) {
  data_close <- data_all[c("close_German10", "close_German5" , 
                           "close_TYc1" , "close_US5Y" , "close_EUR_USD")]
}


names(data_close) <- c("GER10", "GER5" , "TY1" , "US5Y" , "EUR_USD")
lm_GER10_GER5 = lm(data_close$GER5[train_set] ~ data_close$GER10[train_set] , 
                   na.action = na.omit)

counter <- 1

names <- vector()
corrs <- list()

for (i in 1:4) {
  for ( j in (i+1):5) {
    
    corr <- cor.test( data_close[[i]] , data_close[[j]] )
    name <- paste(names(data_close)[i] , names(data_close)[j] )
    
    corrs <- c(corrs , corr)
    names <- c(names , name)
    
    print(name)
    print(corr)
    
    #plot(data_close[[i]][train_set] , data_close[[j]][train_set])
#     linear <- lm(data_close[[i]][train_set] ~ data_close[[j]][train_set] , 
#                  na.action = na.omit)
    
    #lin_vec[counter] <- linear
    
    
#     print(corr)
    
    #abline(lm_GER10_GER5, col="red")
  }
}
#plot(data_close$GER10[train_set] , data_close$GER5[train_set])
#abline(lm_GER10_GER5, col="red")