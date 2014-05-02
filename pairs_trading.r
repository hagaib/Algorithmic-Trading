source("get_data.r")

number_of_observations <- 5
ratios <- NULL
quotes <- 1
while(length(ratios) < number_of_observations) {
  row <- get_data()
  close1 <- row$close_German10
  close2 <- row$close_German5
  ratio <- close1 / close2
  if(!is.na(ratio)) {
    ratios <- c(ratios , ratio)  
  }
  quotes <- quotes + 1
}

date <- TRUE
timer <- 0
strategy <- 0
while(!is.na(date))
{
  ## Fetch last row
  row <- get_data()
  date <- row[1]
  
  close1 <- row$close_German10
  close2 <- row$close_German5
  ratio <- close2 / close1
  mean <- mean(ratios)
  sd <- sd(ratios)
  
  if( ! is.na(ratio)) {
    #calculate how many of each asset should be bought / sold
    cash <-as.numeric(my_assets()[1])
    units.german10 <- cash/2 / close1
    units.german5 <- cash/2 / close2
    
    #try to enter a position , only if you're not already in one
    if (strategy == 0) {
      if (ratio > mean + 2*sd) {
        #short German5 and long German10
        my_bids(c(units.german10 , units.german5 * -1 , NA , NA , NA))
        strategy <- 1
        cash.start <- as.numeric(estimated_total_money())
      } else if (ratio < mean - 2*sd) {
        #long German5 and short German10
        my_bids(c(units.german10 * -1 , units.german5 , NA , NA , NA))
        strategy <- -1
        cash.start <- as.numeric(estimated_total_money())
      }
    }
    
    # update ratios vector , by removing the earliest ratio and appending the latest ratio
    ratios <- ratios[-1]
    ratios[number_of_observations] <- ratio
  } 
  if ( strategy != 0 ) {    # if we're already in a position
    # calculate estimated profit from current position
    profit <- as.numeric(estimated_total_money())  - cash.start   
    #if upper or lower benchmark reached, close position
    if(profit > 10 || profit < -10) {
#       print( c(profit , strategy) )
      sell_everything()
      strategy <- 0
    }
  }
#   print(estimated_total_money())
}