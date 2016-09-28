###################### =============================================== ######################
######################  Function to perform modelling on one data set  ######################
###################### =============================================== ######################

## This file contains the main function, which performs the boosting algorithm over the data
## it also performs the cvrisk() functions and makes predictions using the mstop value obtained.
#' This is executed on a rolling basis over the time-series using a fixed-window of n days.
#' Errors are measured and recorded. The final output object returned is saved by the parent file,
#' which sources this file: 'predictor_of_values.R'

## Taken from 'predictor_of_values_long.R'

## ================================== ##
##  Create one function to run loops  ##
## ================================== ##

stoch_model <- function(data_in, frame_size = 60, look_ahead = 1, #CV parameters
           nIter = 5000, learnRate = 0.01,          #glmboost parameters
           RNG.seed = TRUE,                          #set.seed for reproducible results
           updates = TRUE)                           #print loop number and time at intervals
{

    ## =============================================================== ##
    ##  Define parameters for our time-series cross-validation method  ##
    ## =============================================================== ##

    data_in <- input_data.one_list$Lag.2.combined
    
    ## This is how many models are fitted. Must be equal to length(cv_sets$train)
    num_shifts <- floor((dim(data_in)[1] - frame_size) - (look_ahead -1))
    ## ## Check that we are using the data efficiently
    ## ifelse((dim(data_in)[1] - (num_shifts) - frame_size) != 0,
    ##        warning("++ Not all time-series utilised \n++ Consider using different frame_size/horizon values", call. = FALSE),
    ##        warning("++ All data used", call. = FALSE))

    ## Create index range (a.k.a. 'time slices') for each train and test sets
    #' To make the CV somewhat sparser in order to optimise the shrinkage,
    #' we could set 'skip = 10', reducing the number of models run by 90%
    cv_sets <- createTimeSlices(data_in$DOW.bin, initialWindow= frame_size,
                                horizon = look_ahead, fixedWindow=TRUE, skip = 0)
    ## Create the object to store all output
    bin_results <- results_template(num_shifts,
                                    frame_size, look_ahead, nIter, shrinkage) #Saved with results
    ## ========================================= ##
    ##  Define model parameters and run CV loop  ##
    ## ========================================= ##

    ## Create the formula
    indep_vars <- names(data_in)[names(data_in) != "DOW.bin"]
    rhs <- paste(indep_vars, collapse = " + ")
    my_formula <- as.formula(paste("DOW.bin ~", rhs))
    ## ## Alternative:
    ## my_formula <- "log_ret_DJI ~ ."

    print(paste0("Cross validation started: ", as.character(Sys.time())))
    
    ## Loop through all data for cross validation
    ## Conscious decision NOT to store all cvrisk() output
    for(i in 1:length(cv_sets$train)) {
    #for(i in 1:1) {    
       
        ## Set seed (unique for each loop)
        ## (cvrisk() uses rmultinom() in bootstraps)
        ## if (RNG.seed == TRUE) {
        ##     #set.seed(i)
        ##     set.seed.alpha(paste0("Nicholas_Mitchell_", i))
        ## } else {
        ##     message("Seed not set for pseudo-random number generation")
        ## }

        ## Ensure that the outcome variable is a factor with two levels
        data_in$DOW.bin <- as.character(data_in$DOW.bin)

        ## Run the stochastic model
        gbmFit <- gbm(formula = my_formula, data = data_in,
                      distribution = "bernoulli",
                      bag.fraction = 0.5,
                      n.trees = nIter,
                      shrinkage = learnRate,
                      n.minobsinnode = 1,
                      n.cores = 1,
                      verbose = FALSE)

        ## gbmFit <- gbm.fit(y = data_in$DOW.bin, x = data_in[, -c("DOW.bin"), with=FALSE],
        ##               distribution = "bernoulli",
        ##               bag.fraction = 0.5,
        ##                                 #cv.folds = 3,
        ##               n.trees = nIter,
        ##               shrinkage = learnRate,
        ##               n.minobsinnode = 1,
        ##               response.name = "DOW.bin",
        ##               verbose = FALSE)


        ## Extract the best iteration from cross.validation
        best.iter <- gbm.perf(gbmFit,method="OOB")

        
        ## Predict the (i+1)th value and measure the error
        my.pred <- predict(gbmFit, n.trees = nIter, newdata = data_in[cv_sets$test[[i]], -c("DOW.bin"), with = FALSE],
                           type = "response") #type is class as we want a 1 or 0 back

        my.result <- NA
        ifelse(my.pred > 0, my.result <- "1", my.result <- "0")

        true_value <- data_in[cv_sets$test[[i]], DOW.bin]

        ## Assign results: model output, mstop value, actual and forecasted results
        bin_results$results[[i]][[2]] <- true_value
        bin_results$results[[i]][[3]] <- my.result
        ## Save a value {FALSE, TRUE}, reporting an incorrect or correct prediction, respectively.
        bin_results$results[[i]][[4]] <- my.result == true_value
        ## Save the top five variables according to 'relative influence'
        bin_results$results[[i]][[5]] <- as.character(head(summary(mod2))$var[1:5])

        ## Force garbage collection of larger objects
        rm(gbmFit)

        ## if(updates == TRUE){
        ##     ## print some output to show progress
        ##     print(i)
        ##     ifelse(i == 10, print(Sys.time()), "")
        ##     ifelse(i == 100, print(Sys.time()), "")
        ##     ifelse(i == 200, print(Sys.time()), "")
        ##     ifelse(i == 300, print(Sys.time()), "")
        ##     ifelse(i == 400, print(Sys.time()), "")
        ##     ifelse(i == 500, print(Sys.time()), "")
        ##     ifelse(i == 600, print(Sys.time()), "")
        ## }
    }
    print(paste0("Cross validation completed: ", as.character(Sys.time())))
    
    return(bin_results)
}
