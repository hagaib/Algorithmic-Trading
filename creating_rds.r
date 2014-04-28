###############################################
#' # packrat

if(FALSE) {
	# http://rstudio.github.io/packrat/walkthrough.html
	# install.packages("devtools")
	require("devtools")
	devtools::install_github("rstudio/packrat")
	# install.packages("packrat")
	
	require(packrat)
	packrat::bootstrap()
	
	packrat::status()
	
	packrat::clean() 
	
	packrat::snapshot()
	
	packrat::restore()
}



###############################################
#' # Step 0: Getting some packages...

if (!require('installr')) install.packages('installr'); require('installr')
require2(lubridate) # for Dates manipulation
require2(data.table) # nice for faster loading of the files
# require2(knitr) # used for creating this doc

# compiling this document:
# spin("demo_analysis.r") # can take some time...
# 
# # change locale to work "nicely" with plotting and spinning:
# Sys.getlocale()
# # Sys.setlocale("LC_ALL", "en_US.UTF-8") # does NOT work on Windows
# # Source: http://stackoverflow.com/questions/8145886/change-time-locale-for-r
# Sys.setlocale("LC_TIME", "English")
# # Sys.setlocale("LC_TIME", "Hebrew")
# # https://stat.ethz.ch/pipermail/r-help/2010-March/232512.html
# 
# Sys.getlocale()
# 
# 



###############################################
#' # Step 1: Reading the data

#' ## Unzipping the file


#' set the working directory of the files:
getwd()
# setwd("D:\\Dropbox\\Teaching\\TAU\\2014 - b\\????? ?????? ????????\\01_intro_to_R\\temp")
#' see the files in here:
list.files()

#' we need 7z for unzipping fiels in .7z  ...
#' http://stackoverflow.com/questions/16096192/how-to-programmatically-extract-or-unzip-a-7z-7-zip-file-with-r

#' so let's just work with .zip
zip_file_1 <- "data.zip"
# csv_file_1 <- "preetie.verma@futuresfirst.in-ROY_US5Y_BID_1MIN-N63619129.csv"
# unzipps the file, and we get the name of the file inside it
# unzip_file_1 <- unzip(zipfile=zip_file_1, files=csv_file_1) 
unzip_file_1 <- unzip(zipfile=zip_file_1) 

unzip_file_1
list.files()
names(unzip_file_1) <- c("German10", "German5", "TYc1", "US5Y", "EUR-USD")

#' ## Reading the files

require(data.table)
all_data <- list()
for(i in seq_along(unzip_file_1)) {
	# i = 1
	file_name <- unname(unzip_file_1[i])
	data_name <- names(unzip_file_1[i])
	temp_data  = fread(file_name, 
#				nrows = 5, 
				stringsAsFactors  = FALSE)
	temp_data <- as.data.frame(temp_data)
	temp_data <- cbind(temp_data, asset = data_name)
	all_data <- rbind(all_data, temp_data)	
}


str(all_data)

#' Save the data file (we will do this again)
saveRDS(all_data, file="all_data.rds") 
# all_data <- readRDS("all_data.rds")
# all_data <- all_data[, c(2:12,1)]
#' Remove the unzipped excel files:
unlink(unzip_file_1)
#' And the temp data object:
rm(temp_data)

memory.size()
gc()
ls()




###############################################
#' # Step 2: Fixing the time


# require(installr)
require(lubridate)  # for Dates manipulation
require(data.table)  # nice for faster loading of the files

# head(all_data[,3])
# all_data[,3] = 
# x = head(all_data[ ,2])
# as.Date("02-JAN-2012", "%d-%b-%Y")
# as.Date(x, "%d-%b-%Y")
Sys.setlocale("LC_TIME", "English") # VERY important!
temp_date <- as.Date(all_data[ ,2], "%d-%b-%Y")
temp_date <- as.character(temp_date) # this can take a loooong time.
temp_time <- all_data[,3]
# head(temp_date)
# head(temp_time)
# paste(head(temp_date), head(temp_time))
# apply(cbind(head(temp_date), head(temp_time)), 1, paste , collapse = " ")
# str(temp_date)
# str(temp_time)
temp_date_time <- paste(temp_date, temp_time)
temp_date_time <- ymd_hms(temp_date_time)
# fix GMT offset:
temp_date_time = temp_date_time - all_data[,4]*3600 # I did not validate this!

all_data$Date <- temp_date_time
saveRDS(all_data, file="all_data.rds") 

rm(temp_date)
rm(temp_time)
rm(temp_date_time)
gc()
head(all_data)


###############################################
#' # Step 3: Removing some extra 

#
# https://github.com/hadley/dplyr
# devtools::install_github("hadley/dplyr")
# install.packages("dplyr")
# install.packages("Rcpp")
require(dplyr)
all_data <- readRDS("all_data.rds")


# TY = TY[,-3:-5]
# colnames(TY)= c("Product", "Date", "Trades", "Open","High", "Low", "Close", "Bids")
# colnames(all_data)
# TY = TY[!(TY$Open ==0 | TY$High ==0|  TY$Low ==0|  TY$Close ==0 | (TY$High/TY$Low -1) > 0.01     ),]
ss <- with(all_data,
		   (`Open Bid` ==0 | `High Bid` ==0|  `Low Bid` ==0|  `Close Bid` ==0 | (`High Bid`/`Low Bid` -1) > 0.01     )
		   )
ss <- !ss
system.time(filter(all_data,filter=ss)) # about twice as fast...
system.time(all_data[ss,])
all_data_2 <- filter(all_data,filter=ss)
head(all_data_2)
ss <- (complete.cases(all_data_2) & all_data_2$`No. Bids` != 0) 
sum(!ss) / length(ss) # this removes 45% of the DATA!
all_data_2 <- filter(all_data_2,filter=ss)

dim(all_data) # 4204424      13
dim(all_data_2) # 2199120      13


saveRDS(all_data_2, file="all_data_2.rds") 


# with(all_data_2[1:10**3,], plot(`Open Bid` ~Date, type="l"))


# system.time(filter(all_data[1:10**3,],filter=ss[1:10**3]))
# system.time(all_data[1:10**3,][1:10**3])


# require(devtools)
# devtools::install_github("hadley/dplyr")
# https://github.com/hadley/dplyr
# require(dplyr)







###############################################
#' # Step 4: Creating a "wide" table


# all_data <- all_data_2
all_data <- readRDS("all_data_2.rds")
head(all_data)
unique(all_data$Type) # "Intraday 1Min"

# let's remove some un-needed columns
all_data <- all_data[, -c(1:5)]
head(all_data)
# 	dput(colnames(all_data))
colnames(all_data) <- c("n_trades", "open", "high", "low", "close", 
						"n_bids", "asset", "DATE")

require(dplyr)
temp_data_1 <- filter(all_data, `asset` == "German10")
temp_data_2 <- filter(all_data, `asset` == "German5")
temp_data_3 <- filter(all_data, `asset` == "TYc1")
temp_data_4 <- filter(all_data, `asset` == "US5Y")
temp_data_5 <- filter(all_data, `asset` == "EUR-USD")
# Levels: German10 German5 TYc1 US5Y EUR-USD
head(temp_data_1)
head(temp_data_2)
head(temp_data_3)
# 	inner_join(
# 		head(temp_data_1),
# 		head(temp_data_2),
# 		by = "Date"
# 	)
# 	unique_1 <- unique(temp_data_1$Date)
# 	unique_2 <- unique(temp_data_2$Date)
# 	identical(unique_1,unique_2)
# 	length(unique_1)
# 	length(unique_2)
# 	unique_12 <- union(unique_1,unique_2)
# 	length(unique_12)

# create a vector with all unique Dates so that when we left_join we
# will keep getting the correct Dates in the final data.frame
unique_all_data_Date <- unique(all_data$DATE)
length(unique_all_data_Date) # 949918  | 647784

unique_all_data_Date_df <- data.frame(DATE = unique_all_data_Date)
head(unique_all_data_Date_df)

colnames(temp_data_1)
head(temp_data_1)

# change column names to include asset
colnames(temp_data_1)[1:6] <- paste(colnames(temp_data_1)[1:6], as.character(temp_data_1[1,7]), sep ="_")
colnames(temp_data_1)

colnames(temp_data_2)[1:6] <- paste(colnames(temp_data_2)[1:6], as.character(temp_data_2[1,7]), sep ="_")
colnames(temp_data_3)[1:6] <- paste(colnames(temp_data_3)[1:6], as.character(temp_data_3[1,7]), sep ="_")
colnames(temp_data_4)[1:6] <- paste(colnames(temp_data_4)[1:6], as.character(temp_data_4[1,7]), sep ="_")
colnames(temp_data_5)[1:6] <- paste(colnames(temp_data_5)[1:6], as.character(temp_data_5[1,7]), sep ="_")



temp_data_01 <- merge(
		unique_all_data_Date_df,
		temp_data_1,
		by = "DATE", all = TRUE)
head(temp_data_01)
length(unique_all_data_Date_df$DATE) == length(temp_data_01$DATE)
# 
# # merge them all together.
# temp_data_01 <- 
# 	left_join(
# 		unique_all_data_Date_df,
# 		temp_data_1,
# 		by = "DATE",
# 		copy = TRUE
# 	)

temp_data_012 <- 
	merge(
		temp_data_01,
		temp_data_2,
		by = "DATE",
		all = TRUE
	)

temp_data_0123 <- 
	merge(
		temp_data_012,
		temp_data_3,
		by = "DATE",
		all = TRUE
	)

temp_data_01234 <- 
	merge(
		temp_data_0123,
		temp_data_4,
		by = "DATE",
		all = TRUE
	)

temp_data_012345 <- 
	merge(
		temp_data_01234,
		temp_data_5,
		by = "DATE",
		all = TRUE
	)

head(temp_data_01)
tail(temp_data_01)
dim(temp_data_1)
dim(temp_data_01) # good!
dim(temp_data_012345)
head(temp_data_012345)

dim(unique_all_data_Date_df)
# dim(temp_data_0)
dim(temp_data_01)
dim(temp_data_012)
dim(temp_data_0123)
dim(temp_data_01234)
dim(temp_data_012345)

ss <- duplicated(temp_data_012345$DATE)
sum(ss) # 0 duplicated rows!
temp_data_012345[ss,][1:4,]
# Well, it seems like we have just plain duplicated rows here, arrg...
# http://stackoverflow.com/questions/13967063/remove-duplicate-rows-in-r

temp_data_012345_2<- temp_data_012345

# temp_data_012345_2 <- temp_data_012345[!duplicated(temp_data_012345$DATE),]

# dim(temp_data_012345_2)
# dim(unique_all_data_Date_df)
# good - we are back to 949918 rows...

# make all names use lower case:
colnames(temp_data_012345_2) <- gsub("-", "_", colnames(temp_data_012345_2))
# remove all columns with "asset" in them
temp_data_012345_2 <- temp_data_012345_2[,-grep("asset", colnames(temp_data_012345_2))]
head(temp_data_012345_2)



# Ordering the data by DATE !
oo <- order(temp_data_012345_2$DATE)
identical(seq_along(oo), oo)
# sum(seq_along(oo) != oo)
# head(which(seq_along(oo) != oo))
# temp_data_012345_2[head(which(seq_along(oo) != oo)), ]
# oo[1220:1230]
# temp_data_012345_2[c(1220:1224, 949747) ,]
# It does not need sorting
# Yap, it needs sorting also!
	# DATE n_bids_German10 n_trades_German10 open_German10 low_German10 high_German10 close_German10
	# 1220   2012-01-03 02:17:00               0                NA        138.22       138.22        138.22         138.22
	# 1221   2012-01-03 02:18:00               0                NA        138.22       138.22        138.22         138.22
	# 1222   2012-01-03 02:19:00               0                NA        138.22       138.22        138.22         138.22
	# 1223   2012-01-03 02:20:00               1                NA          0.00         0.00          0.00           0.00
	# 1224   2012-01-03 05:10:00               1                NA          0.00         0.00          0.00           0.00
	# 950767 2012-01-03 04:15:00      

# temp_data_012345_2 <- temp_data_012345_2[oo ,]


# save it...
saveRDS(temp_data_012345_2, file="all_data_wide.rds") 


