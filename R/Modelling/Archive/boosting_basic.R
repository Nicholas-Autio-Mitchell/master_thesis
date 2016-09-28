## ========== ##
##  Boosting  ##
## ========== ##

## Load most current modelling data
#load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/modelling_data.rda") #27-11-2015
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/ready_to_boost.rda") #10-12-2015
data_in <- ready_to_boost$ds_locf_lags$ds_locf_L1
data_in[, dates := NULL]

## Create formula from data
rhs <- paste(colnames(data_in)[1:length(colnames(data_in))-1], collapse = " + ")
formula <- as.formula(paste("log_ret_DJI ~", rhs))

## Execute basic boosting model
bmod0 <- glmboost(formula, data_in)
plot(bmod0)
## Same with more iterations
dev.new()
bmod1 <- glmboost(formula, data_in, control = boost_control(mstop = 3000))
plot(bmod1, mar = c(0, 0, 2, 0))

## Using our model coefficients, we can predict a value for a given row of data
predict(bmod1[56], subset(data_in, select = -c(326))[666, ]) #56 steps, row 666
## See the actual value
subset(data_in, select = c(326))[666, ]


## ==================================== ##
##  Compare with linear model and plot  ##
## ==================================== ##

lmod0 <- lm(formula, data_in)
dev.new()

a <- head((sort(coef(lmod0), decreasing = TRUE)), n=20)
b <- head((sort(coef(bmod0, off2int = TRUE), decreasing = TRUE)), n = 20)
c <- head((sort(coef(bmod1, off2int = TRUE), decreasing = TRUE)), n = 20)
d <- seq(1:20)

plot(d, a, lwd = 3, type="b", xlab = "Coefficient magnitude ranking", ylab = "Coef. Magnitude")
lines(d, b, lwd = 3, col = "red")
lines(d, c, lwd = 3, col = "green")

dev.new()
plot(d, b, type = "l", lwd = 2, col = "red")
lines(c, type = "l", lwd = 2, col = "green")

dev.new()
plot(bmod1, xlim = c(0, 12000))


## ================================== ##
##  Cross-Validate to optimise mstop  ##
## ================================== ##

## We can start with the AIC method to give an indicator
bmod1_AIC <- AIC(bmod1)
plot(bmod1)
abline(v = mstop(bmod1_AIC), col = "lightgray")

## Bootstrap with 25 iterations
cvr <- cvrisk(bmod1, folds = cv(model.weights(bmod1)),
              papply = mclapply)

mstop(cvr)
plot(cvr)


## ============================== ##
##  Inspect correlation/variance  ##
## ============================== ##

nzv <- nearZeroVar(din, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]

dim(mdrrDescr)

[1] 528 342

nzv <- nearZeroVar(mdrrDescr)
filteredDescr <- mdrrDescr[, -nzv]
dim(filteredDescr)


descrCor <- cor(filteredDescr)
summary(descrCor[upper.tri(descrCor)])


highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
filteredDescr <- filteredDescr[,-highlyCorDescr]
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])

## ===================================== ##
##  Define function to view correlation  ##
## ===================================== ##

## Takes a long time to compute, so only good for small data sets
library(ggplot2)

#define a helper function (borrowed from the "ez" package)
ezLev=function(x,new_order){
	for(i in rev(new_order)){
		x=relevel(x,ref=i)
	}
	return(x)
}

ggcorplot = function(data,var_text_size,cor_text_limits){
	# normalize data
	for(i in 1:length(data)){
		data[,i]=(data[,i]-mean(data[,i]))/sd(data[,i])
	}
	# obtain new data frame
	z=data.frame()
	i = 1
	j = i
	while(i<=length(data)){
		if(j>length(data)){
			i=i+1
			j=i
		}else{
			x = data[,i]
			y = data[,j]
			temp=as.data.frame(cbind(x,y))
			temp=cbind(temp,names(data)[i],names(data)[j])
			z=rbind(z,temp)
			j=j+1
		}
	}
	names(z)=c('x','y','x_lab','y_lab')
	z$x_lab = ezLev(factor(z$x_lab),names(data))
	z$y_lab = ezLev(factor(z$y_lab),names(data))
	z=z[z$x_lab!=z$y_lab,]
	#obtain correlation values
	z_cor = data.frame()
	i = 1
	j = i
	while(i<=length(data)){
		if(j>length(data)){
			i=i+1
			j=i
		}else{
			x = data[,i]
			y = data[,j]
			x_mid = min(x)+diff(range(x))/2
			y_mid = min(y)+diff(range(y))/2
			this_cor = cor(x,y)
			this_cor.test = cor.test(x,y)
			this_col = ifelse(this_cor.test$p.value<.05,'<.05','>.05')
			this_size = (this_cor)^2
			cor_text = ifelse(
				this_cor>0
				,substr(format(c(this_cor,.123456789),digits=2)[1],2,4)
				,paste('-',substr(format(c(this_cor,.123456789),digits=2)[1],3,5),sep='')
			)
			b=as.data.frame(cor_text)
			b=cbind(b,x_mid,y_mid,this_col,this_size,names(data)[j],names(data)[i])
			z_cor=rbind(z_cor,b)
			j=j+1
		}
	}
	names(z_cor)=c('cor','x_mid','y_mid','p','rsq','x_lab','y_lab')
	z_cor$x_lab = ezLev(factor(z_cor$x_lab),names(data))
	z_cor$y_lab = ezLev(factor(z_cor$y_lab),names(data))
	diag = z_cor[z_cor$x_lab==z_cor$y_lab,]
	z_cor=z_cor[z_cor$x_lab!=z_cor$y_lab,]
	#start creating layers
	points_layer = layer(
		geom = 'point'
		, data = z
		, mapping = aes(
			x = x
			, y = y
		)
	)
	lm_line_layer = layer(
		geom = 'line'
		, geom_params = list(colour = 'red')
		, stat = 'smooth'
		, stat_params = list(method = 'lm')
		, data = z
		, mapping = aes(
			x = x
			, y = y
		)
	)
	lm_ribbon_layer = layer(
		geom = 'ribbon'
		, geom_params = list(fill = 'green', alpha = .5)
		, stat = 'smooth'
		, stat_params = list(method = 'lm')
		, data = z
		, mapping = aes(
			x = x
			, y = y
		)
	)
	cor_text = layer(
		geom = 'text'
		, data = z_cor
		, mapping = aes(
			x=y_mid
			, y=x_mid
			, label=cor
			, size = rsq
			, colour = p
		)
	)
	var_text = layer(
		geom = 'text'
		, geom_params = list(size=var_text_size)
		, data = diag
		, mapping = aes(
			x=y_mid
			, y=x_mid
			, label=x_lab
		)
	)
	f = facet_grid(y_lab~x_lab,scales='free')
	o = theme(
		panel.grid.minor = element_blank()
		,panel.grid.major = element_blank()
		,axis.ticks = element_blank()
		,axis.text.y = element_blank()
		,axis.text.x = element_blank()
		,axis.title.y = element_blank()
		,axis.title.x = element_blank()
		,legend.position='none'
	)
	size_scale = scale_size(limits = c(0,1),to=cor_text_limits)
	return(
		ggplot()+
		points_layer+
		lm_ribbon_layer+
		lm_line_layer+
		var_text+
		cor_text+
		f+
		o+
		size_scale
	)
}

#set up some fake data
library(MASS)
N=100

#first pair of variables
variance1=1
variance2=2
mean1=10
mean2=20
rho = .8
Sigma=matrix(c(variance1,sqrt(variance1*variance2)*rho,sqrt(variance1*variance2)*rho,variance2),2,2)
pair1=mvrnorm(N,c(mean1,mean2),Sigma,empirical=T)

#second pair of variables
variance1=10
variance2=20
mean1=100
mean2=200
rho = -.4
Sigma=matrix(c(variance1,sqrt(variance1*variance2)*rho,sqrt(variance1*variance2)*rho,variance2),2,2)
pair2=mvrnorm(N,c(mean1,mean2),Sigma,empirical=T)

my_data=data.frame(cbind(pair1,pair2))

ggcorplot(
	data = my_data
	, var_text_size = 30
	, cor_text_limits = c(2,30)
)



###################### ============================================================== ######################
######################  A basic boosting algorithm that can be run across all models  ######################
###################### ============================================================== ######################

library(doMC)
registerDoMC(4)
## library(doParallel) ## for Windows
##registerDoParallel

## Load data - to contains all the various imputations methods and lags
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/ready_to_boost.rda")

## "data in" -> the basic LOCF-imputed set with one lag
din <- ready_to_boost$ds_locf_lags$ds_locf_L1
din <- as.data.frame(din)
din$dates <- NULL
              
## din$dow_movement <- ifelse(din$log_ret_DJI >= 0, 1, 0)

## trainingSet <- createTimeSlices(y = din$dow_movement, initialWindow = 30, horizon = 1,
##                                 fixedWindow = TRUE, skip = 0)

nearZV <- nearZeroVar(din, saveMetrics = TRUE)

# predictors <- names(din)[names(din) != "to_predict"]

## Basic settings to alter for each model below
myControl <- trainControl(method = "timeslice", # take consequetive portions of the time-series
                          initialWindow = 40, # ~2 months of trading days
                          horizon = 5,
                          fixedWindow = TRUE,
                          ## fixedWindow = FALSE  # training set always start at first sample 
                          classProbs = TRUE,
                          summaryFunction = twoClassSummary)

myGrid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                    n.trees = seq(100, 5000, by = 500),
                    shrinkage = c(0.01, 0.1))

## ========================== ##
##  Generalised Linear Model  ##
## ========================== ##

## Create formula from data
rhs <- paste(colnames(din)[1:length(colnames(din))-1], collapse = " + ")
formula <- as.formula(paste("log_ret_DJI ~", rhs))

myControl <- trainControl(method = "timeslice", # take consequetive portions of the time-series
                              initialWindow = 40, # ~2 months of trading days
                              horizon = 1,
                              fixedWindow = TRUE)
                             
myGrid <- expand.grid(mstop = seq(500, 3000, by = 500),
                      prune = "AIC")
                      ##interaction.depth = seq(1, 7, by = 2),
                      ##n.trees = seq(100, 5000, by = 500),
                      ##shrinkage = c(0.01, 0.1))

## No scaling
glm_no_scaling <- train(formula,
                        data = din,
                        method = "glmboost",
                        tuneGrid = myGrid,
                        trControl = myControl,
                        verbose = FALSE)

## Scaling included
glm_scaled <- train(log_ret_DJI  ~ .,
                    data = din,
                    method = "glmboost",
                    tuneGrid = myGrid,
                    preProcess = c("center", "scale", "YeoJohnson"),
                    verbose = FALSE,
                    trControl = myControl)

## Scaling using Independent Components
glm_ica <- train(log_ret_DJI  ~ .,
                 data = din,
                 method = "glmboost",
                 metric = "ROC",
                 tuneGrid = myGrid,
                 preProcess = c("ica")
                 n.comp = 3             # number of components to extract (2 is common, 3 here for 3 dimensions)
                 verbose = FALSE,
                 trControl = myControl)


## ============================== ##
##  Stochastic Gradient Boosting  ##
## ============================== ##

## Create formula from data
rhs <- paste(colnames(din)[1:length(colnames(din))-1], collapse = " + ")
formula <- as.formula(paste("log_ret_DJI ~", rhs))

myControl <- trainControl(method = "timeslice", # take consequetive portions of the time-series
                              initialWindow = 40, # ~2 months of trading days
                              horizon = 5,
                              fixedWindow = TRUE)
                              #classProbs = TRUE,
                              #summaryFunction = twoClassSummary)

myGrid <- expand.grid(interaction.depth = seq(1, 7, by = 2),
                    n.trees = seq(100, 5000, by = 500),
                    shrinkage = c(0.01, 0.1),
                    n.minobsinnode = 10)

## No scaling
gbm_no_scaling <- train(formula,
                 data = din,
                 method = "gbm",
                 #metric = "ROC",
                 tuneGrid = myGrid,
                 verbose = FALSE,
                 trControl = myControl)

## Scaling included
gbm_scaled <- train(log_ret_DJI ~ .,
                 data = din,
                 method = "gbm",
                 metric = "ROC",
                 tuneGrid = myGrid,
                 preProcess = c("center", "scale", "YeoJohnson")
                 verbose = FALSE,
                 trControl = myControl)



## ========== ##
##  Boosting  ##  boosting_basic.R
## ========== ##

## Load most current modelling data
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/modelling_data.rda") #27-11-2015
data <- din
## Create formula from data
rhs <- paste(colnames(data)[1:length(colnames(data))-1], collapse = " + ")
formula <- as.formula(paste("log_ret_DJI ~", rhs))

## Execute basic boosting model
bmod0 <- glmboost(formula, data)
plot(bmod0)
## Same with more iterations
dev.new()
bmod1 <- glmboost(formula, data, control = boost_control(mstop = 3000))
plot(bmod1, mar = c(0, 0, 2, 0))
## Compare to linear model
lmod0 <- lm(formula, data)
dev.new()

a <- head((sort(coef(lmod0), decreasing = TRUE)), n=20)
b <- head((sort(coef(bmod0, off2int = TRUE), decreasing = TRUE)), n = 20)
c <- head((sort(coef(bmod1, off2int = TRUE), decreasing = TRUE)), n = 20)
d <- seq(1:20)

plot(d, a, lwd = 3, type="b", xlab = "Coefficient magnitude ranking", ylab = "Coef. Magnitude")
lines(d, b, lwd = 3, col = "red")
lines(d, c, lwd = 3, col = "green")

dev.new()
plot(d, b, lwd = 3, col = "red", xlim = 6, ylim = 0.008)
lines(c, lwd = 3, col = "green")

dev.new()
plot(bmod1, xlim = c(0, 12000))
