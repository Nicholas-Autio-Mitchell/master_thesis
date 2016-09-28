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

lm_model <- function(data_in, frame_size = 40, look_ahead = 1, #CV parameters
           nIter = 2000, shrinkage = 0.05,          #glmboost parameters
           RNG.seed = TRUE,                          #set.seed for reproducible results
           updates = TRUE)                           #print loop number and time at intervals
{

    ## =============================================================== ##
    ##  Define parameters for our time-series cross-validation method  ##
    ## =============================================================== ##

    ## This is how many models are fitted. Must be equal to length(cv_sets$train)
    num_shifts <- floor((dim(data_in)[1] - frame_size) - (look_ahead -1))
    ## ## Check that we are using the data efficiently
    ## ifelse((dim(data_in)[1] - (num_shifts) - frame_size) != 0,
    ##        warning("++ Not all time-series utilised \n++ Consider using different frame_size/horizon values", call. = FALSE),
    ##        warning("++ All data used", call. = FALSE))

    ## Create index range (a.k.a. 'time slices') for each train and test sets
    #' To make the CV somewhat sparser in order to optimise the shrinkage,
    #' we could set 'skip = 10', reducing the number of models run by 90%
    cv_sets <- createTimeSlices(data_in$DOW.DP, initialWindow= frame_size,
                                horizon = look_ahead, fixedWindow=TRUE, skip = 0)
    ## Create the object to store all output
    lm_results <- results_template(num_shifts,
                                    frame_size, look_ahead, nIter, shrinkage) #Saved with results
    ## ========================================= ##
    ##  Define model parameters and run CV loop  ##
    ## ========================================= ##

    ## Create the formula
    indep_vars <- names(data_in)[names(data_in) != "DOW.DP"]
    rhs <- paste(indep_vars, collapse = " + ")
    my_formula <- as.formula(paste("DOW.DP ~", rhs))

    ## Output to console
    print(paste0("Cross validation started: ", as.character(Sys.time())))
    
    ## Loop through all data for cross validation
    ## Conscious decision NOT to store all cvrisk() output
    for(i in 1:length(cv_sets$train)) {
    ##for(i in 1:1) {
        
        ## Run model and cross validate
        lmFit <- lm(my_formula, data = data_in[cv_sets$train[[i]]], offset = NULL)

        ## Predict the (i+1)th value and measure the error
        my.pred <- predict(lmFit, newdata = data_in[cv_sets$test[[i]], -c("DOW.DP"), with = FALSE], type = "response")

        ## Extract the true value 
        true_value <- data_in[cv_sets$test[[i]], DOW.DP]


                ## Compute the estimation error
        lmErr <- as.matrix(true_value) - my.pred

        ## Assign results: model output, mstop value, actual and forecasted results
        #lm_results$results[[i]][[1]] <-  #not used here, but left for convenience of results interpreting later
        lm_results$results[[i]][[2]] <- data_in[cv_sets$test[[i]], .(DOW.DP)]
        lm_results$results[[i]][[3]] <- my.pred
        ## Assign step-wise errors (functions allow to expand prediction horizon)
        lm_results$results[[i]][[4]][[1]] <- lmErr
        lm_results$results[[i]][[4]][[2]] <- mae(lmErr)
        lm_results$results[[i]][[4]][[3]] <- mse(lmErr)
        lm_results$results[[i]][[4]][[4]] <- rmse(lmErr)
        lm_results$results[[i]][[4]][[5]] <- signs(my.pred, true_value)

    }
    print(paste0("Cross validation completed: ", as.character(Sys.time())))
    
    return(lm_results)
}
