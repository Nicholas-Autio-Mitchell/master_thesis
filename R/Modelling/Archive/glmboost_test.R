## ============================================================== ##
##  Create a simple model using glmboost that runs through caret  ##
## ============================================================== ##

## install as necessary!
library(mboost)
library(caret)
## Use multicore if you can!
library(doMC)
registerDoMC(4)

## ============= ##
##  Create data  ##
## ============= ##

## Let's say we are predicting a numeric value, based on the predictors
## 70 observations of 10 variables, assuming they are chronologically order (a time-series)

set.seed(666)                                                # the devil's seed
myData <- as.data.frame(matrix(rnorm(70*15, 2, .4), 70, 10)) #10 columns of random numbers
names(myData) <- c("to.predict", paste0("var_", seq(1, 9)))
# Have a ganders
str(myData)                             

## Create model output using the mboost package directly
glm_mboost <- glmboost(to.predict ~ .,  # predict against all variables
                       myData,          # supply our data
                       control = boost_control(mstop = 200)
                       )

## This is what I'd like to do with the output from the caret package!
plot(glm_mboost)
cvr <- cvrisk(glm_mboost)
plot(cvr)

## ========================================== ##
##  Set parameters for train() - using caret  ##
## ========================================== ##

## glmboost takes 'mstop' and 'prune' as inputs
myGrid <- expand.grid(mstop = seq(20, 250, 50),
                      prune = "AIC"    #this isn't actually required by the mboost package!
                      )
myControl <- trainControl(method = "timeslice", # take consequetive portions of the time-series
                          fixedWindow = TRUE, # If this is TRUE, we get the error
                          horizon = 1,
                          initialWindow = 20) # ~1 months of trading days
## fixedWindow = TRUE  --> 

## =============== ##
##  Run the model  ##
## =============== ##

glm_caret <- train(to.predict ~ ., data = myData,
                method = "glmboost",
                #metric = "MyGauss",
                trControl = myControl,
                tuneGrid = myGrid
                ##verbose = FALSE)
                )

## Maybe this will give you some idea about how to extract it
str(glm_caret)

## This is the best I can do, but the first plot doesn't come out right
x <- glm_caret$finalModel
plot(x)
cvr1 <- cvrisk(x)
plot(cvr1)

###################### =================================== ######################
######################  From another Q on cross.validated  ######################
###################### =================================== ######################

set.seed(44)

tr.control=trainControl(
  method="cv",
  number=10,
  classProbs=T,
  summaryFunction=twoClassSummary
)

tune.grid=expand.grid(
  .n.trees = c(50),
  .interaction.depth = c(11,12),
  .shrinkage = c(0.001),
  .n.minobsinnode = c(10)
)

set.seed(44)

GBMMod = train(
  x=train[,-which(colnames(train)=="outcome")],
  y=train$outcome,
  method="gbm",
  trControl=tr.control,
  tuneGrid=tune.grid,
  verbose=T,
  metric="ROC"
)

This returns the following results:

interaction.depth  ROC        Sens  Spec  ROC SD      Sens SD  Spec SD
11                 0.9057818  1     0     0.04106306  0        0      
12                 0.9040852  1     0     0.04803282  0        0

If I then select only the best parameters for tuneGrid, i.e. interaction.depth = 11 as follows:

tune.grid=expand.grid(
  .n.trees = c(50),
  .interaction.depth = c(11),
  .shrinkage = c(0.001),
  .n.minobsinnode = c(10)
)

the model returns a different ROC value for these exact same tuning parameters:

ROC        Sens  Spec  ROC SD      Sens SD  Spec SD
0.9110501  1     0     0.03660759  0        0

I would have expected the ROC value to be exactly the same, as the tuning parameters are exactly the same. What am I missing?
