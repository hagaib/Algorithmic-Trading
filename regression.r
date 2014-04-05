### change this to your location ####
tmp = readRDS("all_data_wide.rds")
###

n = dim(tmp)[1]

act4 = !is.na(tmp$open_US5Y)
act5 = !is.na(tmp$open_EUR_USD)

n = dim(tmp)[1]

act5.y = (6:(n-1))[act5[(6:(n-1))]&act5[(7:n)]] # consecutive mins for EUR_USD
act4.y = (6:(n-1))[act4[(6:(n-1))]&act4[(7:n)]] # consecutive mins for US5Y


asses = c("German5","German10","TYc1","US5Y","EUR_USD")
opens = c("open_German5","open_German10","open_TYc1","open_US5Y","open_EUR_USD")
closes = c("close_German5","close_German10","close_TYc1","close_US5Y","close_EUR_USD")


#EUR_USD
act5.y=act5.y[1:(3*10^5)]
X = NULL
for (i in 1:5){
	for (j in 1:5){
		X = cbind (X, tmp[act5.y-i,closes[j]]-tmp[act5.y-i,opens[j]])
		colnames(X)[dim(X)[2]] = paste (asses[j],".-",i,sep="")
		gc()
}
}
X = cbind (X, is.na(X))
colnames(X)[(dim(X)[2]/2+1):dim(X)[2]] = paste(colnames(X)[1:(dim(X)[2]/2+1)],".NA",sep="")
X[is.na(X)] = 0
y = tmp[act5.y,closes[5]]-tmp[act5.y,opens[5]]
test.n = 100000
train = 200000
mod  =(lm(y~.,data=data.frame(X[1:train,],y=y[1:train])))	
print(summary(mod))
gc()
test.i=(train+1):(train+test.n)
pred = predict(mod, newdata = data.frame(X[test.i,]))
gc()
cat (train,":\n")
print(mean(y[test.i]^2))
print(mean((y[test.i]-pred)^2))
print((mean(y[test.i]^2)-mean((y[test.i]-pred)^2))/mean(y[test.i]^2))
cat("win-tie-lose:", sum((sign(pred)*sign(y[test.i]))>0), sum(sign(y[test.i])==0),sum((sign(pred)*sign(y[test.i]))<0),"\n")
cat("net:", sum(sign(pred)*sign(y[test.i])),"\n")
cat("buy by sign of pred:", sum(sign(pred)*(y[test.i])),"\n") # buy by sign 
cat("buy and hold:",tmp[act5.y,"close_EUR_USD"][max(test.i)]-tmp[act5.y,"open_EUR_USD"][min(test.i)])
rm(mod,pred)
gc()


#US5Y
X = NULL
for (i in 1:5){
	for (j in 1:5){
		X = cbind (X, tmp[act4.y-i,closes[j]]-tmp[act4.y-i,opens[j]])
		colnames(X)[dim(X)[2]] = paste (asses[j],".-",i,sep="")
}
}
X = cbind (X, is.na(X))
colnames(X)[(dim(X)[2]/2+1):dim(X)[2]] = paste(colnames(X)[1:(dim(X)[2]/2+1)],".NA",sep="")
X[is.na(X)] = 0
y = tmp[act4.y+1,opens[4]]-tmp[act4.y,opens[4]]
test.n = 100000
train = 100000
mod  =(lm(y~.,data=data.frame(X[1:train,],y=y[1:train])))	
print(summary(mod))
gc()
test.i=(train+1):(train+test.n)
pred = predict(mod, newdata = data.frame(X[test.i,]))
gc()
cat (train,":\n")
print(mean(y[test.i]^2))
print(mean((y[test.i]-pred)^2))
print((mean(y[test.i]^2)-mean((y[test.i]-pred)^2))/mean(y[test.i]^2))
print (cor(pred,y[test.i]))
cat("win-tie-lose:", sum((sign(pred)*sign(y[test.i]))>0), sum(sign(y[test.i])==0),sum((sign(pred)*sign(y[test.i]))<0),"\n")
cat("net:", sum(sign(pred)*sign(y[test.i])),"\n")
cat("buy by sign of pred:", sum(sign(pred)*(y[test.i])),"\n") # buy by sign 
cat("buy and hold:",tmp[act4.y+1,"open_US5Y"][max(test.i)]-tmp[act4.y,"open_US5Y"][min(test.i)])
rm(mod)
gc()




