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
                          horizon = 1,
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
                      prune = "mstop")
                      ##interaction.depth = seq(1, 7, by = 2),
                      ##n.trees = seq(100, 5000, by = 500),
                      ##shrinkage = c(0.01, 0.1))

## No scaling
glm_no_scaling <- train(formula,
                        data = din,
                        method = "glmboost",
                        tuneGrid = myGrid,
                        trControl = myControl,
                        preProcess = c("center"))
                        #verbose = FALSE)

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
