## ===================================== ##
##  Webinar - Tutorial on Caret Package  ##
## ===================================== ##
## Source: https://www.youtube.com/watch?v=7Jbb2ItbTC4

lsos()

#' They treat the first level of the data in any factors as the factor of interest

set.seed(1)

## All columns, but exclude if people churn or not, which is what we want to predict
predictors <- names(churnTrain)[names(churnTrain) != "churn"]

## A stratified split, maintaining the rough percentage of classified data in training and test
inTrainingSet <- createDataPartition(churnTrain$churn, p = 0.75, list = FALSE)


## preProcess: http://topepo.github.io/caret/preprocess.html
#' The order of the steps are automatically optimised, so transformations are only performed after centering and scaling for example
#' Imputation is also possible - see documentation
#' Dummy variables can be added
#' Visualisation methods

## Boosting: look at three parameters (1 + 2 hyperparameters)
#' n_iterations
#' complexity (depth of tree)
#' shrinkage

## Need to make all predictors numeric - model does not accept factors as predictors
forGBM <- churnTrain
forGBM$churn <- ifelse(forGBM$churn == "yes", 1, 0)

## Example model
gbmFit <- gbm(formula = churn ~.,         # Use all predictors
              distribution = "bernoulli", # For classification
              data = forGBM               #
              n.trees = 2000,             # 2000 boosting iterations
              interaction.depth = 7       # How many splits in each tree
              shrinkage = 0.01,           # learning rate
              verbose = FALSE)            # Do no print details

## We need to try out the model though with resampling to optimise the parameters
## We get the re-sampled estimate performance, then compare performance profiles

## See video at 29:00 for algorithm of the 'train' function

gbmTune <- train(x = churnTrain[,predictors],
                y = churnTrain$churn,
                method = "gbm")
## Equivalent, using formula interface - this isn't maybe as memory efficient as it uses all predictors
##gbmTune <- train(churn ~., data = churnTrain, method = "gbm")

## GBM spits out a lot of information... we can enter "verbose = FALSE" in train function which is then passed to our method function i.e. gbm()

## We can use parallel processing, by setting the number of cores that we should use (using the )
library(doMC)
registerDoMC(4)                         # provide four cores

## We need to create the grid of our paramters
grid <- expand.grid(interaction.depth = seq(1, 7, by = 2), # tree depth, number of splits
                    n.trees = seq(100, 1000, by = 50),     # how many iterations
                    shrinkage = c(0.01, 0.1),
                    n.minobsinnode = 10)              # one slow and one fast learner
## If we hadn't specified our own grid, one is created, but only with three values per parameter

## Use the control function "trainControl()" to specify further parameters, like the number of ways to split data for cross-validation

ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

## twoClassSummary proces the area under the ROC curve and also the sensitivity and specificity
## We can provide additional info to tell it what to optimise for. We can say if we want to maximise or minimise
## We can provide a custom function/metric to evaluate for performance.

gbmTune <- train(churn ~., data = churnTrain,
                 method = "gbm",
                 metric = "ROC",
                 tuneGrid = grid,
                 verbose = FALSE,
                 trControl = ctrl)

## In summary, we have:
#' tree depths from 1 to 7
#' boosting iterations from 100 to 1000, at
#' two different learning rates
#' We receive a surface back with the performance of all combinations, a matrix with three columns relating to each of the three paramters we are working through
gbmTune
gbmTune$finalModel                      #Final fit based on optimal paramters

## Plot the comparison of all paramters from "grid"
ggplot(gbmTune) + theme(legend.position = "top")

## Now apply te optimal parameters on the test set - predict() creates an object
gbmPred <- predict(gbmTune, churnTest)
str(gbmPred)



## We can see the probability of the classes - class-probabilities
#' Returns a data.frame, each column referes to the particular class
gbmProbs <- predict(gbmTune, churnTest, type = "prob")
head(gbmProbs)

## Have a look at the confusion matrix (a custom version made in the caret package)
confusionMatrix(gbmPred, churnTest$churn)

## PRC package for ROC curves
rocCurve <- roc(response = churnTest$churn,
                predictor = gbmProbs[, "yes"],
                levels = rev(levels(churnTest$churn)))
rocCurve

## Display results
plot(rocCurve,
     print.thres = c(0.5, 0.2),
     print.thres.pch = 16,
     print.thres.cex = 1.2)


## Use proProc() on the data, because we use imputation, so it can measure the impact of the imputation, by comparing before and after (can that work with glmboost()?? We can't have NAs to run the model before and after imputation to make the comparison)









