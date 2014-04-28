##################
### Prepare data to work with
if(FALSE)
{
	# get all of the data
	all_data <- readRDS("all_data_wide.rds")
	head(all_data)
	dim(all_data) # 647 784
	
	# split the data to training and testing:
	n_rows_for_test <- 2000 # 10^5
	train <- tail(all_data, -n_rows_for_test)
	saveRDS(train, "train.rds")
	test <- tail(all_data, n_rows_for_test)
	saveRDS(test, "test.rds") # this is used by the get_data function!	
}


##################
### Start the work

# set your working directory to where "test.rds" is using setwd()
# getwd()


# getwd()
# list.files()
source("get_data.r")

options(scipen = 10**3)
options(digits = 10)
# we have 1 billion of everything:
my_assets()
# with no bids:
my_bids()

# at first, we see no new data, this is NULL:
my_data()

# Let's fetch 1 row of new data:
get_data()
get_data()

# now we have some data
my_data()

# Let's make a change in our assets:
# currently we have no bids:
my_bids()
# Let's buy 100 German5 (in short!),  and leave the other bids as they are:
my_bids(c(NA, -100, NA,NA,NA))
# our assets are the same:
my_assets()

# buy the assets
get_data()
my_assets()
my_bids()
estimated_total_money() # we don't seem to have made/lost any money

# let's move forward in time and see what happens:
# Now let's ask for 99 more rows:
for(i in 1:300) get_data()
# Now let's see the data we got thus far:
# options(digits = 5)
dim(my_data())
head(my_data())
tail(my_data())


# let's sell everything
my_assets() # we now have some assets, and less money
my_assets_enter_price()
my_assets_exit_price()
estimated_total_money() # we have lost 2 coins because we use short, and the price went up

# let's see how much we get:
sell_everything()
get_data()
my_assets() # we lost 2 coins...
my_assets_enter_price()
my_assets_exit_price()
# we earned 2 coins.


# What if we work with long?
my_assets() 
my_bids(c(NA, NA, 100,NA,NA))
for(i in 1:300) get_data()
my_assets_enter_price()
my_assets_exit_price()
estimated_total_money() # at first round, we will have the same ammount of money as we started with.
sell_everything()
get_data()
my_assets() # we lost more money...


# Let's try short again
my_assets() 
my_bids(c(NA, NA, -100,NA,NA))
for(i in 1:300) get_data()
my_assets_enter_price()
my_assets_exit_price()
estimated_total_money() # at first round, we will have the same ammount of money as we started with.
sell_everything()
get_data()
my_assets() # we lost even more money...



# you can NOT buy more than what you have in the bank...
my_assets() 
my_bids(c(NA, NA, 10^10,NA,NA))
a <- get_data()
my_assets() # we only got a limited ammount of stock...

# let's keep this position for 10 more time stemps
for(i in 1:10) get_data()
# and now let's sell everything:
sell_everything()
a <- get_data()
my_assets() 
# we've made a bunch of money thanks to that one position

# If I have no money, I can not buy new things
my_assets() 
my_bids(c(NA, NA, 10^10,NA,NA))
a <- get_data()
my_assets() # we only got a limited ammount of stock...
my_bids(c(10^10,NA,NA,NA,NA))
a <- get_data()
my_assets() # No new German10, since we had no money to buy it.
# however: if I sell the TYc1, I would have money to buy German10 and German5
my_bids(c(10^10,10^10,0,NA,NA))
a <- get_data()
my_assets() # No new German10, since we had no money to buy it.
estimated_total_money() # at first round, we will have the same ammount of money as we started with.







if(FALSE) {
	require(knitr)
# 	stitch_rhtml("creating_rds.r")	
	stitch_rhtml("get_data.r")	
	stitch_rhtml("api_example.r")	
	
}




















## ===================================
##  Games to ignore...

if(FALSE) {
	
	
	
	# going through all of the closes, omitting the NA's,
	# what are the latest closes:
	my_latest_closes()
	# and my assets?
	my_assets()
	# and combined they form my total estimated money:
	estimated_total_money()
	
	
	
	#################
	# We will now build a simple model on our last 10 observations, and use it to 
	# predict what will happen next, and use that for placing bids.
	# we assume we have 
	# 1000000 / 100
	
	EUR_USD_silly_bid <- function() {
		n_lm_window <- 10
		# X
		dates <- tail(as.numeric(my_data()$DATE), n_lm_window)
		# Y
		close_EUR_USD <- tail(my_data()$close_EUR_USD, n_lm_window)
		fit <- lm(close_EUR_USD~dates)
		
		s_fit <- summary(fit)
		new_EUR_USD_bid <- s_fit$r.squared * sign(coef(fit))[2] * 100
		
		unname(new_EUR_USD_bid)
		# 	plot(close_EUR_USD~dates)
		# 	abline(fit)
		# 	str(close_EUR_USD)
		# 	str(dates)	
	}
	
	
	# Let's keep doing the same thing k_runs times:
	k_runs <- 100
	what_did_we_bid <- numeric(k_runs)
	for(i in 1:k_runs) {
		EUR_USD_bid <- EUR_USD_silly_bid()
		
		what_did_we_bid[i] <- EUR_USD_bid
		new_bids <- c(rep(0,4), EUR_USD_bid)
		my_bids(new_bids)
		get_data()	
	}
	plot(what_did_we_bid, type = "l")
	abline(h = 0)
	my_assets()
	estimated_total_money()
	# this was not such a bright strategy after all...
	
	# you can NOT buy more than what you have in the bank... go bankrupt!
	new_bids <- c(rep(0,4), 10**10)
	my_bids(new_bids)
	get_data()	
	my_assets()
	my_bids(c(1, 0,0,0,0))
	sell_everything()
	get_data()	
	my_assets()
	
	
	
	
}