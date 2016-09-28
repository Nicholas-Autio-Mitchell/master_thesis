###################### =========================================== ######################
######################  Code from earlier uses of {gbm} and caret  ######################
###################### =========================================== ######################

## this code is taken from 'boosting_basic.R'

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

## ============================== ##
##  Stochastic Gradient Boosting  ##
## ============================== ##

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
