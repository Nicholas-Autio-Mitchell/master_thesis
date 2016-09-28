###################### =========================================== ######################
######################  Define models to optimise most parameters  ######################
###################### =========================================== ######################

## =========================== ##
##  Comments about the models  ##
## =========================== ##

#' There are many dimensions along which we can optimise. We can break it down into several steps:
#' Step 1 - which model is used: glmboost, gbm, gamboost, ...
#' Step 2 - which data is used: [Dow(t) ~ Dow(t-1)] or [Dow(t) ~ macro factors] or [Dow(t) ~ Sentiment] or ...
#' Step 3 - compare the optimised version of each model

#' We will create one data set that can be run through all the models to produces one large output.
#' This process must work for the data sets created up until now, i.e. the contents of "all_data_"
#' "ready_to_boost.rda"
#' 
#' We begin by creating a basic function to run glmboost
#' Then a functions using {caret} where possible for: - gbm()
#'                                                    - gamboost()
#'                                                    - blackboost()
#' These must work for one model. This will then be run in a loop to
#' go over all numbers of lags that we have created
#' This large function can then be applied to data sets that have been imputed differently
#' 

## =================== ##
##  Parall processing  ##
## =================== ##

library(doMC)                           # for Unix systems
registerDoMC(4)
##library(doParallel)                   # for Windows
##registerDoParallel

###################### ============================== ######################
######################  Create one standard data set  ######################
###################### ============================== ######################

## Load data - to contains all the various imputations methods and lags
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/ready_to_boost.rda")

## "data in" -> the basic LOCF-imputed set with one lag
din <- ready_to_boost$ds_locf_lags$ds_locf_L1
#din <- ready_to_boost$ds_locf_L0

din <- as.data.frame(din)
din$dates <- NULL
## Add them back later as required
din$dates <- ready_to_boost$ds_locf_lags$ds_locf_L1$dates

## ==================================================================== ##
##  Visualise prediction removal as a function of pairwise correlation  ##
## ==================================================================== ##

## Create new variable to analysis pairwise correlation
corr_data <- as.data.table(din)
## dettach and save factor-predictors to reappend later
to_remove <- grep("(is\\.)", names(din), perl = TRUE)
corr_data <- corr_data[, !to_remove, with=FALSE]

## ## Basically the same as:
## corr_data$L1_is.holiday <- NULL
## corr_data$L1_is.weekend <- NULL
## corr_data$L1_is.monday <- NULL
## corr_data$is.holiday <- NULL
## corr_data$is.weekend <- NULL
## corr_data$is.monday <- NULL

## ============================================================================================== ##
##  Create function to remove covariates pbased on pairwise correlation - visualisation possible  ##
## ============================================================================================== ##

## Get the correlation matrix
corr_pruner <- function(x, cutoff = .85, plot = FALSE, low_corr = .65, high_corr = .99) {
    
    ## Create correlation matrix of input data
    x <- cor(x)

    ## If user wants a plot, create one
    if(plot == TRUE) {
        ## Choose some parameters
        low_corr <- low_corr * 100
        high_corr <- high_corr * 100
        n.points = high_corr - low_corr         # Warning message left in by design - to get nice data.frame
        corr_rem <- as.matrix(cbind(seq(low_corr, high_corr, 1),
                                    rep(0, times = n.points),
                                    rep(0, times = n.points)))

        ## Use the findCorrelation() function from {caret}
        for( i in low_corr:high_corr){
            tmp <- length(findCorrelation(x, cutoff = i/100, names = TRUE, exact = TRUE))
            corr_rem[(i-(low_corr-1)), 2] <- tmp
            corr_rem[(i-(low_corr-1)), 3] <- tmp/dim(x)[[2]]
        }

        ## ## Simple plot
        ## plot(corr_rem, main = "Number of variables removed against cutoff correlation used",
        ##      xlab = "Correlation cutoff (%)",
        ##      ylab = "Percentage of predictors removed",
        ##      type = "l", lwd = 2, col = "red")

        ## Add a variable for the delta of predictors between points
        corr_rem <- as.data.table(corr_rem)
        names(corr_rem) <- c("Cutoff", "Total", "Percent")
        corr_rem$Delta <- diff(corr_rem[,Total])

        ## Plot the percentage of predictors removed on second axis
        par(mar = c(5,5,5,5))
        with(corr_rem, plot(Cutoff, Total, type="l", col="blue",
                            xlab = "Correlation used for cutoff (%)",
                            ylab= "Number of predictors removed",
                            main = "Predictors removed as a function of average pairwise correlation",
                            ylim=range(max(corr_rem[,Total]), min(corr_rem[,Total]))))
        arrows(corr_rem$Cutoff, corr_rem$Total-corr_rem$Delta , corr_rem$Cutoff, corr_rem$Total+corr_rem$Delta, length= .05, angle=90, code = 3)

        par(new = T)
        with(corr_rem, plot(Cutoff, Percent, type = "l", col="blue", pch=16, axes=F,
                            xlab=NA, ylab=NA, cex=1.2),
             ylim=rev(range(min(corr_rem[,Percent]), (max(corr_rem[,Percent])))))
        axis(side = 4)
        mtext(side = 4, line = 3, 'Percentage of total predictors removed')
        with(corr_rem, arrows(Cutoff, Total-Delta , Cutoff, Total+Delta, length= 50, angle=90, code=3))
        legend("topright",pch = NULL,
               legend=c("'Error' bars reflect relative number\n of predictors removed at threshold"),
               lty=c(2,0))
    } else message("++ To visualise the numbers of covariates removed as a function of chosen cutoff,\n++ run the same function again with 'plot = TRUE'")
    ## Pick a percentage and see how many and which predictors will be removed as a result
    corr_cutoff <- cutoff
    to_remove <- findCorrelation(x, cutoff = corr_cutoff, names = TRUE, exact = TRUE)
    output <- as.data.table(x)[, !to_remove, with=FALSE]
    return(output)
}

## ================================================= ##
##  Remove predictors based on pairwise correlation  ##
## ================================================= ##

## Obtain names of predictors to remove
threshold <- 0.9
to_remove <- findCorrelation(x, cutoff = threshold, names = TRUE, exact = TRUE)

## Remove the predictors from the main data.frame including is.monday etc.
din = as.data.table(din)[, !to_remove, with=FALSE]

## We can maybe remove some more variables
nearZV <- nearZeroVar(x = din,
                      ##freqCut = 50/50,  #can use this to show that dow_SPDR has low dispersion
                      ##uniqueCut = 20,   # this also show that the copper_vol proxies exhibit low uniqueness
                      saveMetrics = TRUE)
nearZV
## dow_SPDR is clearly not a very helpful predictor -> remove it

## remove the dow_SPDR predictors and is.weekend
to_remove <- grep("dow_SPDR", names(din))
din <- din[, !to_remove, with=FALSE]
to_remove <- grep("is.weekend", names(din))
din <- as.data.frame(as.data.table(din)[, !to_remove, with=FALSE])


###################### =========================================================== ######################
######################  Create loop to test all subsets - excluding caret for now  ######################
###################### =========================================================== ######################

## Load data
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/data_subsets/all_subsets_v1.rda")

## ## Create structure to save results - this replicates the input structure that we loop through
## all_results <- rapply(all_subsets, function(q) { q <- NULL; q }, how = "list")

## ================================== ##
##  Create container for all results  ##
## ================================== ##

## Create each layer of results container
my_results <- sapply(c("caret", "locf", "spline"), function(x) NULL)
my_lags <- sapply(c("lag_0", "lag_1", "lag_2", "lag_3", "lag_4", "lag_5"), function(x) NULL)
my_subsets <- sapply(c("dow_only", "dow_trad", "dow_macro", "dow_SA_avg", "dow_SA_all", "dow_best"), function(x) NULL)
## Add short lists throughout to assign output from glmboost(), cvrisk() and the mstop value
my_results_headers <- sapply(c("glmFit", "cvr", "mstop"), function(x) NULL)

## Create the container
for(i in 1:3){
    my_results[[i]] <- my_lags
    for(j in 1:6){
        my_results[[i]][[j]] <- my_subsets
        for(k in 1:6){
            my_results[[i]][[j]][[k]] <- my_results_headers
        }
    }
}

#' i is imputation --> 2:3 to rule out caret for now
#' j is the lag --> 1:6 for lags zero to five
#' k is the subset --> 1:5 (sixth is dow_best)

## Run glmboost on each subset of data, with CV
for( i in 2:3) {

    for( j in 1:6) {

        for( k in 1:5) {

            if(j==1 & k==1){k=2}               # skip lag = 0 for "dow_only", as it cannot work
            
            data_in <- copy(all_subsets[[i]][[j]][[k]]) # We have to copy a data.table, otherwise it is a reference
            data_in[, index:=NULL]
            
            ## Create the formula
            myPreds <- names(data_in)[names(data_in) != "log_ret_DJI"]
            rhs <- paste(myPreds, collapse = " + ")
            formula <- as.formula(paste("log_ret_DJI ~", rhs))
            
            ## rhs <- paste(colnames(data_in)[1:length(colnames(data_in))-1], collapse = " + ")
            ## formula <- as.formula(paste("log_ret_DJI ~", rhs))

            ## Fit the model using glmboost
            my_results[[i]][[j]][[k]][[1]] <- glmboost(formula, data = data_in, center = TRUE,
                                                       control = boost_control(mstop = 5000, nu = 0.01))
            ## Perform cross-validation to find optimal 'mstop'
            my_results[[i]][[j]][[k]][[2]] <- cvrisk(my_results[[i]][[j]][[k]][[1]])

            ## Save the optimal mstop
            my_results[[i]][[j]][[k]][[3]] <- mstop(my_results[[i]][[j]][[k]][[2]])
        }
    }
}


###################### ========================== ######################
######################  Procedure for glmboost()  ######################
###################### ========================== ######################

## The data must be loaded and cleaned as performed above:
#' The dow_SPDR sentiment analysis data is removed
#' For a given level of pairwise-correlation (~ 0.85) predictors will be removed
#'
#' The caret package cannot be used to optimise the model, to the steps are peformed manually:
#' glmboost() is executd
#' cvrisk() is executed
#' plots are made

## Create the formula
predictors <- names(din)[names(din) != "log_ret_DJI"]
rhs <- paste(predictors, collapse = " + ")
formula <- as.formula(paste("log_ret_DJI ~", rhs))

## Run a basic model
glmFit0 <- glmboost(formula, data = din,
                    control = boost_control(mstop = 5000, nu = 0.01)) # wide spread of paths
## Center the data
glmFit1 <- glmboost(formula, data = din, center = TRUE,
                    control = boost_control(mstop = 5000, nu = 0.01)) # juxtapositional results? slower conv.
## With centering and various learning rates
glmFit2 <- glmboost(formula, data = din, center = TRUE,
                    control = boost_control(mstop = 5000, nu = 0.1))
glmFit3 <- glmboost(formula, data = din, center = TRUE,
                    control = boost_control(mstop = 5000, nu = 0.01)) #should be identical to gbmFit1
glmFit4 <- glmboost(formula, data = din, center = TRUE,
                    control = boost_control(mstop = 5000, nu = 0.005))
glmFit5 <- glmboost(formula, data = din, center = TRUE,
                    control = boost_control(mstop = 5000, nu = 0.001))

## Cross-validate (default 25-folds) and see "mstop" for each
cvr0 <- cvrisk(glmFit0)
cvr1 <- cvrisk(glmFit1)
cvr2 <- cvrisk(glmFit2)
cvr3 <- cvrisk(glmFit3)
cvr4 <- cvrisk(glmFit4)
cvr5 <- cvrisk(glmFit5)

mstop(cvr0)
mstop(cvr1)
mstop(cvr2)
mstop(cvr3)
mstop(cvr4)
mstop(cvr5)

## Visualise results
plot(cvr0)
plot(cvr1)
plot(cvr2)
plot(cvr3)
plot(cvr4)
plot(cvr5)
## Compare coefficients
coef(glmFit0)
coef(glmFit1)
coef(glmFit2)
coef(glmFit3)
coef(glmFit4)
coef(glmFit5)


din <- dind
din <- din[, predictors, with=FALSE]
din$log_ret_DJI <- dind$log_ret_DJI
nglm <- glmboost(formula, data = din, control = boost_control(mstop = 10, nu = 0,3))

extract(nglm, what = "design")
x <- extract(glmFit3, what = "coefficients")
y <- as.data.frame(x)
z <- y[order(y[,2]),]


qplot(seq(1, dim(z)[1]), z$coef,
      main = "value of coefficients for selected predictors") + geom_line()


## ======================= ##
##  Make some predictions  ##
## ======================= ##

## We need to set each model to its optimal value of mstop
glmFit0[mstop(cvr0)]
glmFit1[mstop(cvr1)]
glmFit2[mstop(cvr2)]
glmFit3[mstop(cvr3)]
glmFit4[mstop(cvr4)]
glmFit5[mstop(cvr5)]

## Predictions to be completed after data subets have ben created - also train/test sets!


###################### ===================== ######################
######################  Procedure for gbm()  ######################
###################### ===================== ######################

## The data must be loaded and cleaned as performed above:
#' The dow_SPDR sentiment analysis data is removed
#' For a given level of pairwise-correlation (~ 0.85) predictors will be removed
#'
#' The caret package can be used to optimise the model.
#' The steps are perfomed in the following order:
#' Step 1 - the formula, trainControl and tuneGrid are specified
#' Step 2 - the train function is specified, including Step 1 output plus further parameters
#' Step 3 - glmboost() is executed
#' Step 4 - The output object contain an element $finalModel, which contain gbm() with optimal parameters

## Create formula from data
rhs <- paste(colnames(din)[1:length(colnames(din))-1], collapse = " + ")
formula <- as.formula(paste("log_ret_DJI ~", rhs))

gbmFit0 <- gbm(formula = formula, data = din,
               distribution = "gaussian", # as we are regressing
               n.trees = 5000,            # large as we have many predictors
               interaction.depth = 4,
               shrinkage = 0.01,        # slow-ish learner
               cv.folds = 25,           # comparable to glmboost's B parameter
               bag.fraction = 1,         # no sub-sampling, i,e, not yet stochastic boosting
               verbose = FALSE)

myControl <- trainControl(method = "repeatedcv", # take consequetive portions of the time-series
                          number = 10,
                          repeats = 10,
                          ##initialWindow = 40, # ~2 months of trading days
                          ##horizon = 5,
                          ##fixedWindow = FALSE
                          ##classProbs = TRUE,                  # These can be used for classification
                          ##summaryFunction = twoClassSummary   # models, using ROC as a metric
                          )

myGrid <- expand.grid(interaction.depth = seq(1, 5, 7),
                      n.trees = seq(100, 2100, by = 500),
                      shrinkage = c(0.05, 0.01),
                      n.minobsinnode = c(5, 10))

## No scaling
gbm_no_scaling <- train(formula,
                        data = din,
                        method = "gbm",
                        metric = "RSquared",
                        disribution = "gaussian", # Ensures correct family
                        ## bag.fraction = 0.6) #This creates stochastic gradient boosting
                        ##metric = "ROC",    # For classifications models, using classProbs as above
                        trControl = myControl,
                        tuneGrid = myGrid,
                        verbose = FALSE)

## Scaling included
gbm_scaled <- train(log_ret_DJI ~ .,
                    data = din,
                    method = "gbm",
                    disribution = "gaussian", # Ensures correct family                    
                    preProcess = c("center", "scale", "YeoJohnson"),
                    trControl = myControl,
                    tuneGrid = myGrid,
                    verbose = FALSE)



###################### ============================ ######################
######################  Bringing all data together  ######################
###################### ============================ ######################

## A list containing one data frame for each model
#' Each data frame must hold the 
