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

bin_model <- function(data_in, frame_size = 40, look_ahead = 1, #CV parameters
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
    cv_sets <- createTimeSlices(data_in$DOW.bin, initialWindow= frame_size,
                                horizon = look_ahead, fixedWindow=FALSE, skip = 0)
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
    ##foreach(i = 1:8, .packages = c("mboost", "caret", "data.table")) %dopar% {
        
        ## Run model and cross validate
        glmFit <- glmboost(my_formula, data = data_in[cv_sets$train[[i]]], center = TRUE,
                           family = Binomial(link = "probit"),
                           control = boost_control(mstop = nIter, nu = shrinkage))

        ## Set seed (unique for each loop)
        ## (cvrisk() uses rmultinom() in bootstraps)
        if (RNG.seed == TRUE) {
            #set.seed(i)
            set.seed.alpha(paste0("Nicholas_Mitchell_", i))
        } else {
            message("Seed not set for pseudo-random number generation")
        }

        ## Complete 25-fold stratified bootstrap CV
        cvr <- cvrisk(glmFit)
        
        ## Put model into the position of its optimal mstop value
        glmFit[mstop(cvr), return = FALSE]

        ## Predict the (i+1)th value and measure the error
        my.pred <- predict(glmFit, newdata = data_in[cv_sets$test[[i]], -c("DOW.bin"), with = FALSE],
                          type = "class") #type is class as we want a 1 or 0 back
        true_values <- data_in[cv_sets$test[[i]], DOW.bin]

        ## Assign results: model output, mstop value, actual and forecasted results
        bin_results$results[[i]][[1]] <- mstop(cvr)
        bin_results$results[[i]][[2]] <- true_values
        bin_results$results[[i]][[3]] <- my.pred
        ## Save a value {FALSE, TRUE}, reporting an incorrect or correct prediction, respectively.
        bin_results$results[[i]][[4]] <- my.pred == true_values
        ## Save the coefficients that were actually used for the prediction
        #' (2*value required to compare to results from any other models as Binomial() returns
        #' half the values (historic reasons))
        bin_results$results[[i]][[5]] <- 2 * coef(glmFit)

        ## Force garbage collection of larger objects
        rm(glmFit, cvr)

        if(updates == TRUE){
            ## print some output to show progress
            print(i)
            ifelse(i == 10, print(Sys.time()), "")
            ifelse(i == 100, print(Sys.time()), "")
            ifelse(i == 200, print(Sys.time()), "")
            ifelse(i == 300, print(Sys.time()), "")
            ifelse(i == 400, print(Sys.time()), "")
            ifelse(i == 500, print(Sys.time()), "")
            ifelse(i == 600, print(Sys.time()), "")
        }
    }
    print(paste0("Cross validation completed: ", as.character(Sys.time())))
    
    return(bin_results)
}
