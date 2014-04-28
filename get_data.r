
###############################################
#' # Create get_data function!

# 
# get_data <- local({
# 	all_data <- readRDS("all_data.rds")
# 	# 	all_data <- all_data[1:100,]
# 	# 	length(all_data$Date) # 4204424
# 	unique_Date <- unique(all_data$Date)
# 	n_unique_Date <- length(unique_Date) # 949918
# 	# 	length(unique_Date) # 949918
# 	# 	min_data <- min(all_data$Date)
# 	# 	max_data <- max(all_data$Date)
# 	i_date <- 1
# 	
# 	function() {
# 		require(dplyr)
# 		temp_data_2 <- filter(all_data, `Date` == unique_Date[i_date])
# 		if(i_date > n_unique_Date) {return("END OF FILE")}
# 		# 		if(nrow(temp_data_2) == 0) {return(NULL)}
# 		# else
# 		i_date <<- i_date+1
# 		return(temp_data_2)
# 	}
# })

# system.time(get_data()) # around 0.3 sec for each run!
# 
# 
# head(all_data)
# get_data()
# for(i in 1:100) print(get_data())
# get_data
# 




price_of_selling_asset <- function(asset_name) {
	# asset_name:  German10  German5     TYc1     US5Y  EUR_USD 
	# asset_name = "German10"
	# my_assets_enter_price()["TYc1"]
	# my_assets_exit_price()["TYc1"]
	enter_price <- as.numeric(my_assets_enter_price()[asset_name])
	exit_price <- as.numeric(my_assets_exit_price()[asset_name])
	quantity <- my_assets()[asset_name]
	
	# 	enter_price <- 10
	# 	exit_price <- 12
	# 	quantity <- 1
	# 	quantity <- -1
	# 	enter_price <- 12
	# 	exit_price <- 10
	
	# calculate the money made (or lost):
	# both are identical, as quantity for short is negative:
	# 		Selling long:   open_price * quantity + (close_price -  open_price) * quantity
	# 		Selling short:  open_price * quantity - (close_price -  open_price) * |quantity|
	
	# for long:
	# how much we have  X  the price  X  how many times did the price rise
	money <- abs(quantity) * enter_price * (exit_price/enter_price)^sign(quantity)	
	as.numeric(money)
}


estimated_total_money <- function() {	
	my_assets  <- my_assets()
	money <- my_assets[1]
	for(i in 1:5) money <- money + price_of_selling_asset(names(my_assets)[-1][i])
	money
}


# actually sell the asset
sell_asset <- function(asset_name) {
	money <- price_of_selling_asset(asset_name)
	
	new_my_assets <- my_assets()
	new_my_assets[asset_name] <- 0
	new_my_assets["money"] <- new_my_assets["money"] + money
	# as.numeric changes the matrix to a vector so that it doesn't breake the assets.
	
	update_my_assets(new_my_assets)
	
	NULL
}



###############################################
#' # Create get_data function (for WIDE data)


# get_data_wide <- local({
get_data <- local({
	
	all_data <- readRDS("test.rds")
# 	all_data <- readRDS("all_data_wide.rds")
	# 	all_data <- all_data[1:100,]
	# 	length(all_data$Date) # 4204424
# 	unique_Date <- unique(all_data$DATE) # this should be the same as all_data$DATE !
	# n_unique_Date <- length(unique_Date) # 949918
	n_Date <- length(all_data$DATE)
		# we assume the file is sorted and uniqued...

	# 	length(unique_Date) # 949918
	# 	min_data <- min(all_data$Date)
	# 	max_data <- max(all_data$Date)
	i_date <- 0




	function() {
		
		# we moved in time!
		i_date <<- i_date+1
		
		
# 		temp_data_2 <- all_data[i_date, ]
		if(i_date > n_Date) {
			cat("END OF FILE")
			return(NULL)
		}
		# 		if(nrow(temp_data_2) == 0) {return(NULL)}
		# else
		
# 		my_bids(c(10,20,-5,-30,0)) 
# 		my_bids(c(100,100,100,100,100)) 
		not_na_bids_TF <- !is.na(my_bids())
		not_na_bids_TF_names <- names(not_na_bids_TF)[not_na_bids_TF]

		# this should ALWAYS be done.
		# Step 1: update exit (i.e.:open) prices
		# exit prices is the open bid for each stock			
		#get_data()
		# set current exit price:
		current_open_prices <- with(environment(get_data), {
			# 				ss_col_open <- with(environment(get_data), grepl("open", colnames(all_data)  ))
			ss_col_open <- grepl("open", colnames(all_data))  
			opens <- as.numeric(all_data[i_date,ss_col_open])
			names(opens) <- colnames(all_data)[ss_col_open]
			opens
		})		
		# if some of them are NA, use the previos exit price:
		na_current_open_prices <- is.na(current_open_prices)
		current_open_prices[na_current_open_prices] <- my_assets_exit_price()[na_current_open_prices]
		update_my_assets_exit_price(current_open_prices)
		# my_assets_exit_price()
		


		if(any( not_na_bids_TF )) {
			
			
# 			my_assets()
# 			update_my_assets(c(1000000, 2, 0,0,0,0))

			# Step 2: sell all of the non-NA bids
			for(i in not_na_bids_TF_names) sell_asset(i)

			# Step 3: update enter (i.e.:open for non NA) prices
			new_enter_price <- my_assets_enter_price()
			new_enter_price[not_na_bids_TF] <- my_assets_exit_price()[not_na_bids_TF]
			update_my_assets_enter_price(new_enter_price)

			# Step 4: if we do not have enough money - update the bids!
			cost_per_asset <- my_assets_enter_price()[not_na_bids_TF] * abs(my_bids())[not_na_bids_TF]
					# we use "abs" on my_bids, in order to allow shorts
			total_cost <- sum(cost_per_asset)
			my_money <- unname(my_assets()[1])

			if(total_cost > my_money) {
				#then we need to update the bids so that (total_cost == my_money)
				bid_reduction_factor <- my_money / total_cost
				new_bids <- my_bids()*bid_reduction_factor
				my_bids(new_bids)
			}
			
			# Step 5: buy the new assets 
			cost_per_asset <- my_assets_enter_price()[not_na_bids_TF] * abs(my_bids())[not_na_bids_TF]
			# we use "abs" on my_bids, in order to allow shorts
			total_cost <- sum(cost_per_asset)
			new_money <- my_money - total_cost
			new_stocks <- my_assets()[-1]
			new_stocks[not_na_bids_TF] <- new_stocks[not_na_bids_TF] + my_bids()[not_na_bids_TF] # what our new stocks are		
			update_my_assets(c(new_money, new_stocks))
			
			# Step 6:  reset the bids.
			my_bids(rep(NA,5)) 

			
		}



		# Step 7: updated exit prices again (this time, based on closes
		current_close_prices <- with(environment(get_data), {
			# 				ss_col_open <- with(environment(get_data), grepl("open", colnames(all_data)  ))
			ss_col_open <- grepl("close", colnames(all_data))  
			opens <- as.numeric(all_data[i_date,ss_col_open])
			names(opens) <- colnames(all_data)[ss_col_open]
			opens
		})		
		# if some of them are NA, use the previos exit price:
		na_current_close_prices <- is.na(current_close_prices)
		current_close_prices[na_current_close_prices] <- my_assets_exit_price()[na_current_close_prices]
		update_my_assets_exit_price(current_close_prices)
		# my_assets_exit_price()


		return(all_data[i_date, ])
	}
})

# system.time(get_data_wide()) # around 0.02 sec for each run!
# install.packages("microbenchmark")
# require("microbenchmark")
# # microbenchmark(get_data_wide()) # around 0.013 sec for each run!
# 
# head(all_data)
# get_data_wide()
# for(i in 1:100) print(get_data_wide())
# 


# Easily get my data (so far)
my_data <- function() {
	i_date <- with(environment(get_data), i_date)
	if(i_date==0) return(NULL)
	# else:
	with(environment(get_data), all_data[1:(i_date),])
}



# TODO: this one can be made faster by just saving an internal object with the closes and updating it...
# rm(ss_col_close)
my_latest_closes <- function() {
	i_date <- with(environment(get_data), i_date)
	if(i_date==0) return(NULL)
	# else:
	with(environment(get_data), {
		ss_col_close <- with(environment(get_data), grepl("close", colnames(all_data)  ))
		get_last_value <- function(x) { tail(na.omit(x), 1)  }
		apply(all_data[1:i_date,ss_col_close],2, get_last_value)
		})
}

# my_latest_closes()

# get_data_wide()
# my_data()
# with(environment(get_data), i_date)


# with(environment(get_data), dim(all_data))


############
# Next functions:
# set_position
# get_position





###############################################
#' # Create get_position function

my_assets <- local({
	assets_names <- c("money", "German10" ,"German5" ,"TYc1" ,"US5Y", "EUR_USD")
 	assets <- c(10^6, rep(0,5))
# 	assets <- rep(10^9, 6)
	# I start with 1 million of everything so to enable "shorting" a stock,
	# without the need to get into managing more complex types of assets.
	names(assets) <- assets_names
	
	function() {
		return(assets)
	}
})


my_assets_enter_price <- local({
	assets_names <- c("German10" ,"German5" ,"TYc1" ,"US5Y", "EUR_USD")
	assets_enter_price <- rep(0,5)
	# 	assets <- rep(10^9, 6)
	# I start with 1 million of everything so to enable "shorting" a stock,
	# without the need to get into managing more complex types of assets.
	names(assets_enter_price) <- assets_names
	
	function() {
		return(assets_enter_price)
	}
})

my_assets_exit_price <- local({
	assets_names <- c("German10" ,"German5" ,"TYc1" ,"US5Y", "EUR_USD")
	assets_exit_price <- rep(0,5)
	# 	assets <- rep(10^9, 6)
	# I start with 1 million of everything so to enable "shorting" a stock,
	# without the need to get into managing more complex types of assets.
	names(assets_exit_price) <- assets_names
	
	function() {
		return(assets_exit_price)
	}
})


# my_assets_enter_price()
# my_assets_exit_price()
# update_my_assets_enter_price(c(1,1,90,2,2))
# my_assets_enter_price()

# my_assets()


###############################################
#' # Create get_position function

# an internal function to update assets (should be used by get_data_wide)
update_my_assets <- function(new_assets) {
	names(new_assets) <- names(environment(my_assets)$assets)	
	environment(my_assets)$assets <- new_assets
	invisible(my_assets())
}

update_my_assets_enter_price <- function(new_assets_enter_price) {
	names(new_assets_enter_price) <- names(environment(my_assets_enter_price)$assets_enter_price)	
	environment(my_assets_enter_price)$assets_enter_price <- new_assets_enter_price
	invisible(my_assets_enter_price())
}

update_my_assets_exit_price <- function(new_assets_exit_price) {
	names(new_assets_exit_price) <- names(environment(my_assets_exit_price)$assets_exit_price)	
	environment(my_assets_exit_price)$assets_exit_price <- new_assets_exit_price
	invisible(my_assets_exit_price())
}

# my_assets()

###############################################
#' # Create bid_asset function

# + is buying
# - is selling
# all transactions go through money!
my_bids <- local({
	bids_names <- c("German10" ,"German5" ,"TYc1" ,"US5Y", "EUR_USD")
	bids <- rep(NA,5)
	names(bids) <- bids_names
	
# 	# round to plus minus 100 (vectorized)
# 	round_to_pm_100 <- function(x) {
# 		x<- ifelse(sign(x) == 1 & x>100, 100,x )
# 		x<- ifelse(sign(x) == -1 & x < -100, -100,x )
# 		x
# 	}
# # 	round_to_pm_100(c(-100, -101, 101, 100, 0, 55, -55))
# # 	sign(3)
# # 	sign(-3)
# # 	sign(0)
# 	
	function(new_bids) {
		# We can add conditions if the actions are valid...
		
		# if we do not place any bid, then we see what are the current bids...
		if(missing(new_bids)) {
			return(bids)			
		}
		
		# if bids are not of the correct length
		if(length(new_bids) != 5) {
			warning("The length of your bids is not 5, bids were not updated!")
			return(bids)
		}
		
		# if new_bids are missing names, let's put them in.
		if(is.null(names(new_bids))) names(new_bids) <- bids_names
		
# 		money <- my_assets()[1]
# 		if(money <= 0) {
# 			warning("You have no money left, you may only sell assets")
# 			# Making any positive bids 0:
# 			new_bids <- ifelse(new_bids >0, 0, new_bids)
# 		}
		
		# in any case we ask to sell more than we have - sell all that we have
# 		if(any(-new_bids > my_assets()[-1])) warning("You made bids which are to sell more than you have. Selling all of that asset")
# 		new_bids <- ifelse(-new_bids >  my_assets()[-1], my_assets()[-1], new_bids)

# 		# Let's force up to 100 units per of action
# 		if(any(abs(new_bids) > 100)) {
# 			warning("Some bids were beyond 100 units - and were rounded to 100 unit distance!")
# 		}		
# 		new_bids <- round_to_pm_100(new_bids)
		
		# else, place bids, and returns bids.
		bids <<- new_bids
		return(bids)
	}
})

# my_assets()
# my_bids(c(0,0,10000,0,0))
# my_bids(c(0,0,100,0,0))
# get_data()
# my_bids(c(0,10000,0,0,0))
# my_bids(c(0,0,-100,0,0))
# my_bids(c(0,0,-101,0,0))
# my_bids(c(0,0,-1000000000,0,0))
# my_bids(c(0,0,-1000000001,0,0))



sell_everything <- function() {	
	my_bids(rep(0,5))
}


# my_bids()
# my_bids(c(4,4,4))
# my_bids(c(10,-5,0,0,0,0))
# my_bids()



# TODO: update get_data_wide
# 		so it would update the assets based on the bids
#		this logic needs some more thinking...

