###################### ======================================================== ######################
######################  Short tests for final comparisons in Empirical chapter  ######################
###################### ======================================================== ######################

library(doMC)
registerDoMC(1)

## Load data and extract the target data set
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/5_stochastic_gbm/subsets_lagged.rda")
x <- copy(subsets_lagged$Lag.3$combined)

## Set the outcome variables to binary response
x[DOW.DP > 0, "DOW.DP"]  <- 1
x[DOW.DP < 0, "DOW.DP"]  <- 0
x[, DOW.DP := as.character(DOW.DP)]

x$DOW.DP <- as.factor(x$DOW.DP)
x$DOW.DP <- make.names(x$DOW.DP)


## ==================================== ##
##  Optimise paramters over large grid  ##
## ==================================== ##

## Create formula from data
indep_vars <- names(x)[names(x) != "DOW.DP"]
rhs <- paste(indep_vars, collapse = " + ")
my_formula <- as.formula(paste("DOW.DP ~", rhs))

## Use repeated cross validation for tuneing grid
myControl <- trainControl(method = "repeatedcv", # take consequetive portions of the time-series
                          number = 5, returnResamp = "none",
                          summaryFunction = twoClassSummary, classProbs = TRUE)

## Create a grid of all paramters to optimise over
myGrid <- expand.grid(interaction.depth = 1,
                    n.trees = seq(100, 5000, by = 100),
                    shrinkage = c(0.005, 0.01, 0.05),
                    n.minobsinnode = c(1, 5, 10))

## Train model over entire paramter grid
mod <- train(form = my_formula, data = x1,
             method = "gbm",
             metric = "ROC",
             tuneGrid = myGrid,
             trControl = myControl,
             preProcess = "center",
             verbose = FALSE)

## ------------------- ##
##  results - summary  ##
## ------------------- ##

## Tuning parameter 'interaction.depth' was held constant at a value of 1
## ROC was used to select the optimal model using  the largest value.
## The final values used for the model were n.trees = 5000, interaction.depth = 1, shrinkage = 0.01 and n.minobsinnode = 10. 

## ========================================================= ##
##  Run on smaller data sets with out-of-sample predictions  ##
## ========================================================= ##

## Uses the optimum paramters found above

## Load data and extract the target data set
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/5_stochastic_gbm/subsets_lagged.rda")
x <- copy(subsets_lagged$Lag.3$combined)
newd <- x[62] %>% .[, DOW.DP := NULL]   #1 day to test (predict)

## Create train and test data sets
x1 <- x[2:61]                           #60 days to train
## These sizes used to test if it'll go through our framework
x1[DOW.DP > 0, "DOW.DP"]  <- 1
x1[DOW.DP < 0, "DOW.DP"]  <- 0

## Chang outcome to factors with suitable namaes
x1$DOW.DP <- as.factor(x1$DOW.DP)
x1$DOW.DP <- make.names(x1$DOW.DP)

## Create formula from data
indep_vars1 <- names(x1)[names(x1) != "DOW.DP"]
rhs1 <- paste(indep_vars, collapse = " + ")
my_formula1 <- as.formula(paste("DOW.DP ~", rhs))

mod2 <- gbm(formula = my_formula1, data = x1,
            distribution = "bernoulli",
            bag.fraction = 0.5,
            n.trees = 5000, shrinkage = 0.01, n.minobsinnode = 10)

## Make aprediction
pred <- predict(mod, newdata = newd, type = "raw")
x$DOW.DP[61]

