# Lab3 : Clustering

# Consider the letter iris set

	DS = iris
	print(summary(DS))
# See letters distribution in the learnin sample
	gr = DS$Species
	DS = DS[,-5]
#############################
# kmeans clutering 
#############################
	km1 = kmeans(DS,3, nstart = 150)
	km2 = kmeans(DS,149)
	conf = table(gr,km1$cluster)
# clusters labels are arbitrary, they may be arranged in an optimal order
	y = solve_LSAP(conf, maximum = TRUE)
	conf = conf[,y]
# look now at conf
	print(conf)
# km1 contains a lot of informations about the clusters
	names(km1)
	km1$centers

# show both true groups and clusters obtained
	pairs(DS,pch=km1$cluster,col=iris$Species)

# Show distribution of the variables within each cluster
	par(mfrow=c(1,3))
	b = by(DS,km1$cluster,boxplot)

# #############################
# Hierachical clustering
###############################
	hc01 = hclust(dist(DS))
plot(hc01)
rect.hclust(hc01,4)

####
# How do results depend on the number of Clusters ?
###
# We will use some functions from the package cubt
	library(cubt)
# Split the data in two subsamples, n/3 2n/3 approximately
	K = 30 # maximum number of clusters to try
n = 150
	Ind.test = c(sample(1:50,n/9),sample(51:100,n/9),sample(101:150,n/9))
	Learn = iris[-Ind.test,-5]
	Test = iris[Ind.test,-5]
	gr.L = iris[-Ind.test,5]
	gr.T = iris[Ind.test,5]
	errs = devs.L = devs.T =rep(NA,K)
	colVar=function(xx) (nrow(xx) -1)* apply(xx,2,var)
	varintra= function(xx,Classe) rowSums(do.call(rbind,by(xx,Classe,colVar)))
	for(k in 2:K) {	
		km1 = kmeans(Learn,k)
		prev = predict.clus(km1$cluster,Learn,Test,"euclidean")
		errs[k] = error(gr.T,prev)[1]
		devs.T[k] =  sum(varintra(Test,prev),na.rm=T)
		devs.L[k] =  sum(varintra(Learn,km1$cluster),na.rm=T)
	}
# plot Within SS for Learn and Test samples
	plot(devs.L,type="l",xlab="Number of Clusters",ylab="Total Within SS")
	points(devs.T,type="l",col="blue")
# add a penalty to teh deviance
	pen = 10*sqrt(1:K)
	lim = range(c(devs.L, devs.T)+ pen,na.rm=T)
	plot(devs.L + pen,type="l",xlab="Number of Clusters",ylab="Total Within SS",ylim=lim)
	points(devs.T + pen ,type="l",col="blue")

# plot Mismach Error
	plot(errs,type="l")
