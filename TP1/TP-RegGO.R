# TP-reg.R
# Auteur B.Ghattas
	
rm(list = ls())
data(airquality)
	summary(airquality)
	pairs(airquality)
	dim(airquality)
	names(airquality)
#----------------------------------------
# Remove missing data and get sample size and dimension
	DS = na.omit(airquality); class(DS)
    DS=as.data.frame(scale(DS)); class(DS)
	dim(DS)
	n = nrow(DS);n
	p = ncol(DS);p
# First a Linear model
	mod1 = lm(Ozone ~ ., data=DS)
# plot(mod1) shows different plots 
	prev  = predict(mod1)
# Split the data in two subsamples
	seed=855329 # 1298765 #6754  # 1234
    Ind.test = sample(n,round(n/3))
	Learn = DS[-Ind.test,]
	Test  = DS[Ind.test,]
    Ind.learn=setdiff(1:n,Ind.test)
# Check dimensions
	print(dim(Learn))
	print(dim(Test))
# Learn the model using the learning sample & predict over the test sample
	mod1 = lm(Ozone ~ . + 1, data=Learn)
	prev = predict(mod1,newdata=Test)
    mod1  # ecrire les résultats
    summary(mod1) # ecrire les résultats sont forme plus detaillee
# Analyze residuals
	summary(prev-Test$Ozone)
	plot(prev-Test$Ozone,main='Residus de la regression');grid()
	
	xlim=c(1,n);ylim=c(min(DS$Ozone),max(DS$Ozone))
	plot(Test$Ozone, prev-Test$Ozone,col='red', main='Residus / Ozone pour les tests'); grid()
	# Une representation des données 
    xlim=c(1,n);ylim=c(min(DS$Ozone),max(DS$Ozone))
    plot(Ind.test,Test$Ozone,col='red', type='p', xlim=xlim,ylim=ylim);par(new=TRUE)
	plot(Ind.learn,DS$Ozone[Ind.learn],col='blue', type='p', xlim=xlim,ylim=ylim, xlab='',ylab='')
	
    
# compute Mean Squared error of prediction
	mse = mean((prev-Test$Ozone)^2)
# take its square root
	print(sqrt(mse))	

# Idees complémentaires
#   Travailler avec des donnnées centrées réduite pour analyesr les coefficients. 
#    Utiliser summary(mod1) pour chercher les variables inmportantes
    