# TP-FS

# Some useful functions

	FDS = function (X,Y){	
# X a numeric vector
# Y a factor having wo levels
		if(length(levels(Y)) !=2) stop("Y must have two levels")
		m = tapply(X,Y,mean)
		s = tapply(X,Y,sd)
		abs( (m[1]-m[2]) /(s[1] - s[2]))
}

# consider airquality data set
# Convert Ozone variable to binary one "Y" for polluted, "N" for non polluted
	DS = na.omit(airquality)
	DS$Ozone = cut(DS$Ozone,c(0,quantile(DS$Ozone,0.75),180),labels=c("N","Y"))

# FDS score between Ozone and Temperature
	FishT = FDS(DS$Temp,DS$Ozone)
	FishS =	FDS(DS$Solar.R,DS$Ozone)
	FishW =FDS(DS$Wind,DS$Ozone)
	FishM =FDS(DS$Month,DS$Ozone)
	FishD =FDS(DS$Day,DS$Ozone)
	
	
# get the p-value of the test comparing Temperature between Polluted and non polluted days
	tt = t.test(DS$Temp~ DS$Ozone,alternative="two.sided")
	print(tt$p.value)
	ss = t.test(DS$Solar.R~ DS$Ozone,alternative="two.sided")
	print(ss$p.value)
	ww = t.test(DS$Wind~ DS$Ozone,alternative="two.sided")
	print(ww$p.value)
	mm = t.test(DS$Month~ DS$Ozone,alternative="two.sided")
	print(mm$p.value)
	dd = t.test(DS$Day~ DS$Ozone,alternative="two.sided")
	print(dd$p.value)
#Score the four explanatory variables using FDS and p.value of the t.test
	fds.res = tt.res = NULL
	for(j in 1:5){ 
		fds.res[j]=FDS(DS[,j+1],DS$Ozone)
		tt = t.test(DS[,j+1]~ DS$Ozone,alternative="two.sided")
		tt.res[j] = tt$p.value
	}
	names(fds.res) = names(tt.res) = names(DS[,-1])
	#
	par(mfrow=c(1,2))
	barplot(fds.res)
	barplot(1-tt.res)
# sort now these vectors to get variables in increasing order of importance
	fds.res = sort(fds.res,decreasing=T)
	tt.res = sort(tt.res)
# get variables names in decreasing order of importance
	var.fds = names(fds.res)
	var.tt = names(tt.res)
# now use for instance CART to adjust a tree prediction model

# split the data in Learning and test samples
	n = nrow(DS)
	ind = sample(n, n/3)
	test = DS[ind,]
	learn = DS[-ind,]
		
# 
	library(rpart)
	err = NULL
	err2 = NULL
# lesvars contains the variables names in decreasing order of importance
# this may be changed, using the results obtained by fds, t.test, CART, or RF
	#avec test de fischer
	lesvars = var.fds
	for(j in 1:5) {
		mod = rpart(learn[,c("Ozone",lesvars[1:j])],minsplit=2)
		print(mod)
		prev = predict(mod,newdata=test[lesvars[1:j]],type="class")
		err[j] = mean(prev != test$Ozone)
	}
	# Observe the error evolution 	
	plot(err,type="l")
	# find the minimum of this curve
	combien = which.min(err)
	# Get the optimal subset of variables
	optvars = lesvars[1:combien]
	
	
#avec test de student
	lesvarstt = var.tt
	testCart = sort(lesvarstt,decreasing=F)
	lesvarstt = testCart
	for(j in 1:5) {
	  mod2 = rpart(learn[,c("Ozone",lesvarstt[1:j])],minsplit=2)
	  print(mod2)
	  prev2 = predict(mod2,newdata=test[lesvarstt[1:j]],type="class")
	  err2[j] = mean(prev2 != test$Ozone)
	}
# Observe the error evolution 	
	plot(err2,type="l")
# find the minimum of this curve
	combien2 = which.min(err2)
# Get the optimal subset of variables
	optvars2 = lesvarstt[1:combien2]
	
	testCart = sort(fds.res,decreasing=F)
	var.fds = names(fds.res)

##########################################
# variables importance with randomforest
##########################################
	library(randomForest)
	# run the randomForest asking for variables importance
	rf = randomForest(y=learn[,1],x=learn[var.fds[1:j]],importance=T)
# plot variables importance, the two indexes
	varImpPlot(rf)
# Get one index the Mean DecreaseAccuracy
	rf.res = importance(rf)[,3]
	rf.res = sort(rf.res)


#########################################
# Using Lasso
#########################################
# We come back to the original airquality data with Y continuous
	DS = na.omit(airquality)
	n = nrow(DS)
	ind = sample(n, n/3)
	test = DS[ind,]
	learn = DS[-ind,]

	library(lars)
	lrs = lars(y=learn[,1],x=as.matrix(learn[,-1]))

# Use the same process of variables subset selection as before
# 	1./ using the var importances obtained by random Forest
# 	2./ using another predictive model thar rpart (randomforest for example)
